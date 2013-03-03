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

-- | Runs the function whenever the event passes the given predicate.
doEvent :: (Maybe a -> Bool) -> Signal a -> (Maybe a -> IO ()) -> Signal a
doEvent p s f =
    signal $ \sub ->
        let onEvent m = if p m then f m >> sub m else sub m
        in s `subscribe` onEvent

-- | Runs a function on each value.
doNext :: Signal a -> (Maybe a -> IO ()) -> Signal a
doNext = doEvent isJust

-- | Runs the function on each completion.
doCompleted :: Signal a -> (Maybe a -> IO ()) -> Signal a
doCompleted = doEvent isNothing
