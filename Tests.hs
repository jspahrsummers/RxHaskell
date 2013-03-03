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
    in join ss `subscribe` putSub

testSequencing = do
    (hello >> world) `subscribe` putSub
    (world >> hello) `subscribe` putSub

testAppending = do
    hello
        `mappend` mempty
        `subscribe` putSub

    hello
        `mappend` world
        `subscribe` putSub

    world
        `mappend` hello
        `subscribe` putSub

testSubject = do
    (subj, s) <- subject
    s `subscribe` putSub
    subj $ Just "hello world"

testFilter = do
    hello
        `mappend` world
        `filter` (\(x:xs) -> x == 'h')
        `subscribe` putSub

testDoNext = do
    hello
        `doNext` (\_ -> putStrLn "next")
        `subscribe` putSub

testDoCompleted = do
    hello
        `doCompleted` (\_ -> putStrLn "completed")
        `subscribe` putSub
