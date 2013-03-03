{-# LANGUAGE Safe #-}

module Signal (Subscriber, Signal, signal, subscribe) where

import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Monoid

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

-- | Subscribes to a signal.
subscribe :: Signal a -> Subscriber a -> IO ()
subscribe (Signal s) = s

instance Monad Signal where
    return v =
        let f next = do
                next $ Just v
                next Nothing
        in Signal f

    s >>= f = Signal $ \sub -> do
        sc <- newIORef (1 :: Integer)

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
                subscribe (f v) onInnerNext

        subscribe s onOuterNext

    a >> b = Signal $ \sub ->
        let onNext Nothing = subscribe b sub
            onNext _ = return ()
        in subscribe a onNext

instance Monoid (Signal a) where
    mempty = Signal $ \sub -> sub Nothing
    a `mappend` b = Signal $ \sub ->
        let onNext Nothing = subscribe b sub
            onNext m = sub m
        in subscribe a onNext

instance Functor Signal where
    fmap f s = s >>= return . f
