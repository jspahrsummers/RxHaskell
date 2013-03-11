{-# LANGUAGE Arrows #-}

module TestArrow where

import Control.Arrow
import Signal
import Signal.Arrow
import Signal.Operators

testConcat = do
    let a :: SignalArrow String String
        a =
            proc x -> do
                foo <- arr (++ "foo") -< x
                bar <- arr (++ "bar") -< x
                returnA -< (foo ++ ":" ++ bar)

    runSignalArrow a (fromFoldable ["hello", "world"])
        >>: print
