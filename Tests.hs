module Tests where

import Control.Monad
import Data.Monoid
import Signal
import Signal.Operators as Op
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
        `Op.filter` (\(x:xs) -> x == 'h')
        `subscribe` sub

testDoNext = do
    s
        `Op.doNext` (\_ -> putStrLn "next")
        `subscribe` sub
