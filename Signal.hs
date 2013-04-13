{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal ( Signal
              , Event(..)
              , signal
              , subscribe
              , (>>:)
              , never
              , Signal.empty
              ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Zip
import Data.IORef
import Data.Monoid
import Data.Sequence as Seq
import Data.Word
import Disposable
import Prelude hiding (length, drop, zip)
import Scheduler.Internal
import Signal.Event
import Signal.Subscriber

-- | A signal which will send values of type @v@ on a scheduler of type @s@.
data Signal s v where
    Signal :: Scheduler s => (Subscriber s v -> SchedulerIO s Disposable) -> Signal s v

-- | Constructs a signal which sends its values to new subscribers synchronously.
signal :: Scheduler s => (Subscriber s v -> SchedulerIO s Disposable) -> Signal s v
signal = Signal

-- | Subscribes to a signal.
subscribe
    :: Scheduler s
    => Signal s v               -- ^ The signal to subscribe to.
    -> Subscriber s v           -- ^ The subscriber to attach.
    -> SchedulerIO s Disposable -- ^ A disposable which can be used to terminate the subscription.

subscribe (Signal f) = f

-- | Returns a signal which never sends any events.
never :: Scheduler s => Signal s v
never = Signal $ const $ return EmptyDisposable

-- | Returns a signal which immediately completes.
empty :: Scheduler s => Signal s v
empty = mempty

-- | Creates a subscriber and subscribes to the signal.
(>>:) :: Scheduler s => Signal s v -> (Event v -> SchedulerIO s ()) -> SchedulerIO s Disposable
(>>:) s f = do
    sub <- liftIO $ subscriber f
    subscribe s sub

infixl 1 >>:

instance Scheduler s => Monad (Signal s) where
    return v =
        Signal $ \sub -> do
            send sub (NextEvent v)
            send sub CompletedEvent
            return EmptyDisposable

    s >>= f =
        Signal $ \sub -> do
            sc <- liftIO $ newIORef (1 :: Word32)
            ds <- liftIO newDisposableSet

            let decSubscribers = do
                    rem <- liftIO $ atomicModifyIORef sc $ \n ->
                        let n' = n - 1
                        in (n', n')

                    when (rem == 0) $ send sub CompletedEvent

                onInner CompletedEvent = decSubscribers
                onInner ev = send sub ev

                onOuter CompletedEvent = decSubscribers
                onOuter (ErrorEvent e) = send sub $ ErrorEvent e
                onOuter (NextEvent v) = do
                    liftIO $ atomicModifyIORef sc $ \n -> (n + 1, ())

                    d <- f v >>: onInner
                    liftIO $ ds `addDisposable` d

            d <- s >>: onOuter
            liftIO $ ds `addDisposable` d
            liftIO $ toDisposable ds

instance Scheduler s => Functor (Signal s) where
    fmap = liftM

instance Scheduler s => Applicative (Signal s) where
    pure = return
    (<*>) = ap

instance Scheduler s => Monoid (Signal s v) where
    mempty = Signal $ \sub -> do
        send sub CompletedEvent
        return EmptyDisposable

    a `mappend` b =
        Signal $ \sub -> do
            ds <- liftIO newDisposableSet

            let onEvent CompletedEvent = do
                    d <- b `subscribe` sub
                    liftIO $ ds `addDisposable` d

                onEvent e = send sub e
            
            d <- a >>: onEvent
            liftIO $ ds `addDisposable` d
            liftIO $ toDisposable ds

instance Scheduler s => MonadPlus (Signal s) where
    mzero = mempty
    a `mplus` b =
        join $ Signal $ \sub -> do
            send sub $ NextEvent a
            send sub $ NextEvent b
            send sub CompletedEvent
            return EmptyDisposable

-- Implementing mzip outside of an instance declaration so we can get some scoped type variables
-- (without needing the InstanceSigs extension).
szip :: forall a b s. Scheduler s => Signal s a -> Signal s b -> Signal s (a, b)
szip a b =
    Signal $ \sub -> do
        aVals <- liftIO $ atomically $ newTVar (Seq.empty :: Seq a)
        aDone <- liftIO $ atomically $ newTVar False

        bVals <- liftIO $ atomically $ newTVar (Seq.empty :: Seq b)
        bDone <- liftIO $ atomically $ newTVar False

        ds <- liftIO newDisposableSet

        let completed :: STM [Event (a, b)]
            completed = do
                ac <- readTVar aDone
                al <- length <$> readTVar aVals

                bc <- readTVar bDone
                bl <- length <$> readTVar bVals

                return $ if (ac && al == 0) || (bc && bl == 0) then [CompletedEvent] else []
            
            onEvent' :: (TVar (Seq x), TVar Bool) -> (TVar (Seq y), TVar Bool) -> (Seq x -> Seq y -> Seq (a, b)) -> Event x -> STM [Event (a, b)]
            onEvent' vt@(vals, _) ot@(otherVals, _) f (NextEvent v) = do
                modifyTVar' vals (|> v)

                vs <- readTVar vals
                os <- readTVar otherVals

                case viewl $ f vs os of
                    (t :< _) -> do
                        modifyTVar' vals $ drop 1
                        modifyTVar' otherVals $ drop 1
                        
                        (:) (NextEvent t) <$> completed

                    _ -> return []

            onEvent' (_, done) _ _ CompletedEvent = writeTVar done True >> completed
            onEvent' _ _ _ (ErrorEvent e) = return [ErrorEvent e]

            onEvent :: (TVar (Seq x), TVar Bool) -> (TVar (Seq y), TVar Bool) -> (Seq x -> Seq y -> Seq (a, b)) -> Event x -> SchedulerIO s ()
            onEvent vt ot f ev = do
                evl <- liftIO $ atomically $ onEvent' vt ot f ev
                mapM_ (send sub) evl

        let at = (aVals, aDone)
            bt = (bVals, bDone)

        ad <- a >>: onEvent at bt zip
        liftIO $ ds `addDisposable` ad

        bd <- b >>: onEvent bt at (flip zip)
        liftIO $ ds `addDisposable` bd

        liftIO $ toDisposable ds

instance Scheduler s => MonadZip (Signal s) where
    mzip = szip
