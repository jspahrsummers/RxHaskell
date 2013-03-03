module Tests where

import Control.Monad
import Data.Monoid
import Prelude hiding (filter, take)
import Disposable
import Event
import Signal
import Signal.Operators
import Subject
import Subscriber

hello =
    signal $ \sub -> do
        send sub $ NextEvent "hello"
        send sub CompletedEvent
        return Disposable.empty

world =
    signal $ \sub -> do
        send sub $ NextEvent "world"
        send sub CompletedEvent
        return Disposable.empty

putSub = putStrLn . show

testBinding =
    let ss =
            signal $ \sub -> do
                send sub $ NextEvent hello
                send sub $ NextEvent world
                send sub CompletedEvent
                return Disposable.empty
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
    send subj $ NextEvent "hello world"

testFilter = do
    hello
        `mappend` world
        `filter` (\(x:xs) -> x == 'h')
        >>: putSub

testDoEvent = do
    hello
        `doEvent` (\_ -> putStrLn "event")
        >>: putSub

testDoNext = do
    hello
        `doNext` (\_ -> putStrLn "next")
        >>: putSub

testDoCompleted = do
    hello
        `doCompleted` (putStrLn "completed")
        >>: putSub

testTake = do
    hello
        `mappend` world
        `mappend` hello
        `take` 2
        >>: putSub
