module Tests where

import Control.Monad
import Data.Monoid
import Prelude hiding (filter)
import Signal
import Signal.Operators
import Subject

hello =
    signal $ \sub -> do
        sub $ Just "hello"
        sub Nothing

world =
    signal $ \sub -> do
        sub $ Just "world"
        sub Nothing

putSub = putStrLn . show

testBinding =
    let ss =
            signal $ \sub -> do
                sub $ Just hello
                sub $ Just world
                sub Nothing
    in join ss >>: putSub

testSequencing = do
    (hello >> world) >>: putSub
    (world >> hello) >>: putSub

testAppending = do
    hello
        `mappend` mempty
        >>: putSub

    hello
        `mappend` world
        >>: putSub

    world
        `mappend` hello
        >>: putSub

testSubject = do
    (subj, s) <- subject
    s >>: putSub
    subj $ Just "hello world"

testFilter = do
    hello
        `mappend` world
        `filter` (\(x:xs) -> x == 'h')
        >>: putSub

testDoNext = do
    hello
        `doNext` (\_ -> putStrLn "next")
        >>: putSub
