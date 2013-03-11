{-# LANGUAGE Arrows #-}

module TestCircuit where

import Control.Arrow
import Signal
import Signal.Circuit
import Signal.Operators

testConcat = do
    let a :: Circuit String String
        a =
            proc x -> do
                foo <- arr (++ "foo") -< x
                bar <- arr (++ "bar") -< x
                returnA -< (foo ++ ":" ++ bar)

    runCircuit a (fromFoldable ["hello", "world"])
        >>: print
