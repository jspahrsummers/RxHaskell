{-# LANGUAGE Safe #-}

module Signal ( Signal
              , signal
              , subscribe
              , (>>:)
              , never
              ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Zip
import Data.IORef
import Data.Monoid
import Data.Sequence as Seq
import Data.Word
import Disposable
import Event
import Prelude hiding (length, drop, zip)
import Subscriber

-- | A stream of future values.
data Signal a = Signal (Subscriber a -> IO Disposable)

-- | Constructs a signal.
signal
    :: (Subscriber a -> IO Disposable)  -- ^ A function to run upon each subscription to the signal.
    -> Signal a                         -- ^ The constructed signal.

signal = Signal

-- | Returns a signal which never sends any events.
never = signal $ const $ return Disposable.empty

-- | Subscribes to a signal.
subscribe :: Signal a -> Subscriber a -> IO Disposable
subscribe (Signal s) = s

-- | Creates a subscriber and subscribes to the signal.
(>>:) :: Signal a -> OnEvent a -> IO Disposable
(>>:) s f = do
    sub <- subscriber f
    subscribe s sub

infixl 1 >>:

instance Monad Signal where
    return v =
        signal $ \sub -> do
            send sub $ NextEvent v
            send sub CompletedEvent
            return Disposable.empty

    s >>= f =
        signal $ \sub -> do
            sc <- newIORef (1 :: Word32)
            cd <- newCompositeDisposable

            let decSubscribers :: IO ()
                decSubscribers = do
                    rem <- atomicModifyIORef sc $ \n ->
                        let n' = n - 1
                        in (n', n')

                    if rem == 0
                        then send sub CompletedEvent
                        else return ()

                onInner CompletedEvent = decSubscribers
                onInner ev = send sub ev

                onOuter CompletedEvent = decSubscribers
                onOuter (ErrorEvent e) = send sub $ ErrorEvent e
                onOuter (NextEvent v) = do
                    atomicModifyIORef sc $ \n -> (n + 1, ())
                    f v >>: onInner >>= addDisposable cd

            s >>: onOuter >>= addDisposable cd
            return cd

    a >> b =
        signal $ \sub -> do
            cd <- newCompositeDisposable

            let onEvent CompletedEvent = b `subscribe` sub >>= addDisposable cd
                onEvent (ErrorEvent e) = send sub $ ErrorEvent e
                onEvent _ = return ()

            a >>: onEvent >>= addDisposable cd
            return cd

instance Monoid (Signal a) where
    mempty =
        signal $ \sub ->
            Disposable.empty <$ send sub CompletedEvent

    a `mappend` b =
        signal $ \sub -> do
            cd <- newCompositeDisposable

            let onEvent CompletedEvent = b `subscribe` sub >>= addDisposable cd
                onEvent e = send sub e
            
            a >>: onEvent >>= addDisposable cd
            return cd

instance Functor Signal where
    fmap f s = s >>= return . f

instance MonadZip Signal where
    a `mzip` b =
        signal $ \sub -> do
            aVals <- atomically $ newTVar Seq.empty
            aDone <- atomically $ newTVar False

            bVals <- atomically $ newTVar Seq.empty
            bDone <- atomically $ newTVar False

            cd <- newCompositeDisposable

            let completed :: (TVar (Seq a), TVar Bool) -> (TVar (Seq b), TVar Bool) -> STM [Event (x, y)]
                completed (vals, done) (otherVals, otherDone) = do
                    vc <- readTVar done
                    vl <- length <$> readTVar vals

                    oc <- readTVar otherDone
                    ol <- length <$> readTVar otherVals

                    if (vc && vl == 0) || (oc && ol == 0)
                        then return [CompletedEvent]
                        else return []
                
                onEvent' :: (TVar (Seq a), TVar Bool) -> (TVar (Seq b), TVar Bool) -> (Seq a -> Seq b -> Seq (x, y)) -> Event a -> STM [Event (x, y)]
                onEvent' vt@(vals, _) ot@(otherVals, _) f (NextEvent v) = do
                    modifyTVar' vals (|> v)

                    vs <- readTVar vals
                    os <- readTVar otherVals

                    case viewl $ f vs os of
                        (t :< _) -> do
                            modifyTVar' vals $ drop 1
                            modifyTVar' otherVals $ drop 1
                            
                            (:) (NextEvent t) <$> completed vt ot

                        _ -> return []

                onEvent' vt@(vals, done) ot _ CompletedEvent = completed vt ot
                onEvent' _ _ _ (ErrorEvent e) = return [ErrorEvent e]

                onEvent vt ot f ev = do
                    evl <- atomically (onEvent' vt ot f ev)
                    mapM_ (send sub) evl

            let at = (aVals, aDone)
                bt = (bVals, bDone)

            a >>: onEvent at bt zip >>= addDisposable cd
            b >>: onEvent bt at (flip zip) >>= addDisposable cd
            return cd
