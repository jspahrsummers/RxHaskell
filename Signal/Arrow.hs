{-# LANGUAGE Safe #-}

module Signal.Arrow ( SignalArrow
                    , runSignalArrow
                    ) where

import Control.Arrow
import Control.Category
import Control.Monad
import Prelude hiding ((.), id)
import Signal

newtype SignalArrow b c = SignalArrow {
    runSignalArrow :: (Signal b -> Signal c)
}

instance Arrow SignalArrow where
    arr = SignalArrow . liftM
    first (SignalArrow f) =
        let first' bds =
                bds >>= \(b, d) ->
                    let cs = f $ return b
                    in fmap (flip (,) d) cs
        in SignalArrow first'

instance Category SignalArrow where
    (SignalArrow g) . (SignalArrow f) = SignalArrow (g . f)
    id = arr id
