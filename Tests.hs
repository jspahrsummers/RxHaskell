module Tests where

import Control.Monad
import Data.Monoid
import Prelude hiding (filter)
import Signal
import Signal.Operators
import Subject

s =
    signal $ \sub -> do
        sub $ Just "hello"
        sub Nothing

s' =
    signal $ \sub -> do
        sub $ Just "world"
        sub Nothing

sub = putStrLn . show

testBinding =
    let ss =
            signal $ \sub -> do
                sub $ Just s
                sub $ Just s'
                sub Nothing
    in join ss `subscribe` sub

testSequencing = do
    (s >> s') `subscribe` sub
    (s' >> s) `subscribe` sub

testAppending = do
    s
        `mappend` mempty
        `subscribe` sub

    s
        `mappend` s'
        `subscribe` sub

    s'
        `mappend` s
        `subscribe` sub

testSubject = do
    (subj, s'') <- subject
    s'' `subscribe` sub
    subj $ Just "hello world"

testFilter = do
    s
        `mappend` s'
        `filter` (\(x:xs) -> x == 'h')
        `subscribe` sub

testDoNext = do
    s
        `doNext` (\_ -> putStrLn "next")
        `subscribe` sub
