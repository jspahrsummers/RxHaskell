{-# LANGUAGE Safe #-}

module Signal.Operators where

import Data.Monoid
import Signal

-- | Filters the values of a signal according to a predicate.
filter :: Signal a -> (a -> Bool) -> Signal a
filter s f =
    let f' x = if f x then return x else mempty
    in s >>= f'
