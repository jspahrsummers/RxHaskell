{-# LANGUAGE Safe #-}

module Signal.Operators where

import Data.Monoid
import Data.Maybe (isJust, isNothing)
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
        in s `subscribe` onEvent

-- | Runs a function on each value.
doNext :: Signal a -> (a -> IO ()) -> Signal a
doNext s f =
    let f' (Just x) = f x
        f' Nothing = return ()
    in doEvent s f'

-- | Runs the function on each completion.
doCompleted :: Signal a -> (() -> IO ()) -> Signal a
doCompleted s f =
    let f' (Just x) = return ()
        f' Nothing = f ()
    in doEvent s f'
