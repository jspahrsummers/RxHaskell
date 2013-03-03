{-# LANGUAGE Safe #-}

module Signal.Operators ( filter
                        , doNext
                        , take
                        ) where

import Data.IORef
import Data.Monoid
import Prelude hiding (filter, take)
import Signal

-- | Filters the values of a signal according to a predicate.
filter :: Signal a -> (a -> Bool) -> Signal a
filter s f =
    let f' x = if f x then return x else mempty
    in s >>= f'

-- | Runs a function on each event.
doNext :: Signal a -> (Maybe a -> IO ()) -> Signal a
doNext s f =
    signal $ \sub ->
        let onNext m = f m >> sub m
        in s >>: onNext

-- | Returns a signal of the first @n@ elements.
take :: Integral n => Signal a -> n -> Signal a
take s n =
    signal $ \sub -> do
        remRef <- newIORef n

        let onNext Nothing = do
                b <- atomicModifyIORef remRef $ \rem ->
                    (0, if rem == 0 then False else True)

                if b then sub Nothing else return ()

            onNext (Just v) = do
                old <- atomicModifyIORef remRef $ \rem ->
                    if rem == 0 then (0, 0) else (rem - 1, rem)

                case old of
                    0 -> return ()
                    1 -> sub (Just v) >> sub Nothing
                    _ -> sub (Just v)

        s >>: onNext
