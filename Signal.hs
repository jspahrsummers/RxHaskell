{-# LANGUAGE Safe #-}

module Signal ( Signal
              , signal
              , subscribe
              , (>>:)
              , never
              ) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Monoid
import Data.Word
import Disposable
import Event
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
