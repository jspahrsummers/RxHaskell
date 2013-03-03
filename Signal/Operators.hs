{-# LANGUAGE Safe #-}

module Signal.Operators ( filter
                        , doEvent
                        , doNext
                        , doCompleted
                        , take
                        ) where

import Data.IORef
import Data.Monoid
import Event
import Prelude hiding (filter, take)
import Signal
import Subscriber

-- | Filters the values of a signal according to a predicate.
filter :: Signal a -> (a -> Bool) -> Signal a
filter s f =
    let f' x = if f x then return x else mempty
    in s >>= f'

-- | Runs a function whenever the signal sends an event.
doEvent :: Signal a -> (Event a -> IO ()) -> Signal a
doEvent s f =
    signal $ \sub ->
        let onEvent e = f e >> send sub e
        in s >>: onEvent

-- | Runs a function on each value.
doNext :: Signal a -> (a -> IO ()) -> Signal a
doNext s f =
    let f' (NextEvent x) = f x
        f' _ = return ()
    in doEvent s f'

-- | Runs a function on completion.
doCompleted :: Signal a -> IO () -> Signal a
doCompleted s f =
    let f' CompletedEvent = f
        f' _ = return ()
    in doEvent s f'

-- | Returns a signal of the first @n@ elements.
take :: Integral n => Signal a -> n -> Signal a
take s n =
    signal $ \sub -> do
        remRef <- newIORef n

        let onEvent ev@(NextEvent _) = do
                old <- atomicModifyIORef remRef $ \rem ->
                    if rem == 0 then (0, 0) else (rem - 1, rem)

                case old of
                    0 -> return ()
                    1 -> send sub ev >> send sub CompletedEvent
                    _ -> send sub ev

            onEvent ev = do
                b <- atomicModifyIORef remRef $ \rem ->
                    (0, if rem == 0 then False else True)

                if b then send sub ev else return ()

        s >>: onEvent
