{-# LANGUAGE Safe #-}

module Signal.Operators ( filter
                        , doEvent
                        , doNext
                        , doCompleted
                        , take
                        ) where

import Data.IORef
import Data.Monoid
import Data.Maybe (isJust, isNothing)
import Prelude hiding (filter, take)
import Signal

-- | Filters the values of a signal according to a predicate.
filter :: Signal a -> (a -> Bool) -> Signal a
filter s f =
    let f' x = if f x then return x else mempty
    in s >>= f'

-- | Runs the function whenever signal sends an event.
doEvent :: Signal a -> (Maybe a -> IO ()) -> Signal a
doEvent s f =
    signal $ \sub ->
        let onEvent m = f m >> sub m
        in s >>: onEvent

-- | Runs a function on each value.
doNext :: Signal a -> (a -> IO ()) -> Signal a
doNext s f =
    let f' (Just x) = f x
        f' Nothing = return ()
    in doEvent s f'

-- | Runs the function on each completion.
doCompleted :: Signal a -> IO () -> Signal a
doCompleted s f =
    let f' (Just x) = return ()
        f' Nothing = f
    in doEvent s f'

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
