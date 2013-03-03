{-# LANGUAGE Safe #-}

module Signal ( Subscriber
              , Signal
              , signal
              , subscribe
              , (>>:)
              , never
              ) where

import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Word

-- | A callback for a 'Signal'.
-- | The values of the signal are sent as @Just a@. 'Nothing' will be sent upon completion.
type Subscriber a = Maybe a -> IO ()

-- | A stream of future values.
data Signal a = Signal (Subscriber a -> IO ())

-- | Constructs a signal.
signal
    :: (Subscriber a -> IO ()) -- ^ A function to run upon each subscription to the signal.
    -> Signal a                -- ^ The constructed signal.

signal = Signal

-- | Returns a signal which never sends any events.
never = signal $ const $ return ()

-- | Subscribes to a signal.
subscribe :: Signal a -> Subscriber a -> IO ()
subscribe (Signal s) = s

-- | A symbolic alias for 'subscribe'.
(>>:) = subscribe
infixl 1 >>:

instance Monad Signal where
    return v =
        signal $ \next -> do
            next $ Just v
            next Nothing

    s >>= f =
        signal $ \sub -> do
            sc <- newIORef (1 :: Word32)

            let decSubscribers :: IO ()
                decSubscribers = do
                    rem <- atomicModifyIORef sc $ \n ->
                        let n' = n - 1
                        in (n', n')

                    if rem == 0
                        then sub Nothing
                        else return ()

                onInnerNext Nothing = decSubscribers
                onInnerNext m = sub m

                onOuterNext Nothing = decSubscribers
                onOuterNext (Just v) = do
                    atomicModifyIORef sc $ \n -> (n + 1, ())
                    f v >>: onInnerNext

            s >>: onOuterNext

    a >> b =
        signal $ \sub ->
            let onNext Nothing = b >>: sub
                onNext _ = return ()
            in a >>: onNext

instance Monoid (Signal a) where
    mempty = signal $ \sub -> sub Nothing
    a `mappend` b =
        signal $ \sub ->
            let onNext Nothing = b >>: sub
                onNext m = sub m
            in a >>: onNext

instance Functor Signal where
    fmap f s = s >>= return . f
