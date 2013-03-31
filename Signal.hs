{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}

module Signal ( SignalM
              , Signal
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
import Event
import Prelude hiding (length, drop, zip)
import ScheduledIO
import Scheduler
import Subscriber

-- | A signal which will send values of type @v@ on a scheduler of type @s@.
data Signal s v where
    Signal :: Scheduler s => (Subscriber s v -> IO Disposable) -> Signal s v

-- | Constructs a signal which sends its values to new subscribers immediately.
signal :: (Subscriber ImmediateScheduler v -> IO Disposable) -> Signal ImmediateScheduler v
signal = Signal

-- | Subscribes to a signal.
subscribe
    :: Scheduler s
    => Signal s v       -- ^ The signal to subscribe to.
    -> Subscriber s v   -- ^ The subscriber to attach.
    -> IO Disposable    -- ^ A disposable which can be used to terminate the subscription.

subscribe (Signal f) = f

-- | Returns a signal which never sends any events.
never :: Scheduler s => Signal s v
never = Signal $ const $ return EmptyDisposable

-- | Returns a signal which immediately completes.
--empty :: Signal ImmediateScheduler v
--empty = mempty

-- | Returns a signal which immediately sends a single value, then completes.
constant :: v -> Signal ImmediateScheduler v
constant = return

-- | Creates a subscriber and subscribes to the signal.
(>>:) :: Scheduler s => Signal s v -> (Event v -> ScheduledIO s ()) -> IO Disposable
(>>:) s f = do
    sub <- subscriber f
    subscribe s sub

infixl 1 >>:

instance Scheduler s => Monad (Signal s) where
    return v =
        Signal $ \sub ->
            runScheduledIO $ send sub (NextEvent v) >> send sub CompletedEvent

    s >>= f =
        signal $ \sub -> do
            sc <- newIORef (1 :: Word32)
            ds <- newDisposableSet

            let decSubscribers = do
                    rem <- liftIO $ atomicModifyIORef sc $ \n ->
                        let n' = n - 1
                        in (n', n')

                    when (rem == 0) $ send sub CompletedEvent

                onInner CompletedEvent = decSubscribers
                onInner ev = send sub ev

                onOuter CompletedEvent = decSubscribers
                onOuter (ErrorEvent e) = send sub $ ErrorEvent e
                onOuter (NextEvent v) =
                    liftIO $ do
                        atomicModifyIORef sc $ \n -> (n + 1, ())

                        f v >>: onInner >>= addDisposable ds

            s >>: onOuter >>= addDisposable ds
            toDisposable ds

{-
instance (MonadIO m, Scheduler s) => Functor (SignalM m s) where
    fmap = liftM

instance (MonadIO m, Scheduler s) => Applicative (SignalM m s) where
    pure = return
    (<*>) = ap

instance (MonadIO m, Scheduler s) => Monoid (SignalM m s v) where
    mempty = signal $ \sub -> do
        send sub CompletedEvent
        return EmptyDisposable

    a `mappend` b =
        signal $ \sub -> do
            ds <- newDisposableSet

            let onEvent CompletedEvent = b `subscribe` sub >>= addDisposable ds
                onEvent e = send sub e
            
            a >>: onEvent >>= addDisposable ds
            toDisposable ds

instance (MonadIO m, Scheduler s) => MonadPlus (SignalM m s) where
    mzero = mempty
    a `mplus` b =
        join $ signal $ \sub -> do
            send sub $ NextEvent a
            send sub $ NextEvent b
            send sub CompletedEvent
            return EmptyDisposable

instance (MonadIO m, Scheduler s) => MonadZip (SignalM m s) where
    a `mzip` b =
        signal $ \sub -> do
            aVals <- liftIO $ atomically $ newTVar Seq.empty
            aDone <- liftIO $ atomically $ newTVar False

            bVals <- liftIO $ atomically $ newTVar Seq.empty
            bDone <- liftIO $ atomically $ newTVar False

            ds <- newDisposableSet

            let completed :: STM [Event (x, y)]
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

                onEvent vt ot f ev = do
                    evl <- liftIO $ atomically (onEvent' vt ot f ev)
                    mapM_ (send sub) evl

            let at = (aVals, aDone)
                bt = (bVals, bDone)

            a >>: onEvent at bt zip >>= addDisposable ds
            b >>: onEvent bt at (flip zip) >>= addDisposable ds
            toDisposable ds
-}
