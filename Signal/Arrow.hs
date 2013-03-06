{-# LANGUAGE Safe #-}

module Signal.Arrow ( SignalArrow
                    , runSignalArrow
                    ) where

import Control.Arrow
import Control.Category
import Control.Monad
import Event
import Prelude hiding ((.), id)
import Signal

newtype SignalArrow b c = SignalArrow {
    runSignalArrow :: Signal b -> Signal c
}

instance Category SignalArrow where
    (SignalArrow g) . (SignalArrow f) = SignalArrow (g . f)
    id = arr id

instance Arrow SignalArrow where
    arr = SignalArrow . liftM
    first (SignalArrow f) =
        let first' bds =
                bds >>= \(b, d) ->
                    let cs = f $ return b
                    in fmap (flip (,) d) cs
        in SignalArrow first'

instance ArrowChoice SignalArrow where
    left (SignalArrow a) =
        let onNext (Left b) =
                let cs = a $ return b
                in fmap Left cs

            onNext (Right d) = return $ Right d
        in SignalArrow (>>= onNext)
