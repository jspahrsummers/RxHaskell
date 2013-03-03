{-# LANGUAGE Safe #-}

module Signal.Operators ( filter
                        , doNext
                        ) where

import Data.Monoid
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
        in s `subscribe` onNext
