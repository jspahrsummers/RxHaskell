module Tests where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Zip
import Data.Monoid
import Prelude hiding (filter, take, drop)
import Disposable
import Scheduler
import Scheduler.Main
import Signal
import Signal.Operators
import Signal.Scheduled
import Subject

hello = fromFoldable ["hello"]
world = fromFoldable ["world"]

testBinding :: SchedulerIO MainScheduler Disposable
testBinding =
    let ss =
            signal $ \sub -> do
                send sub $ NextEvent hello
                send sub $ NextEvent world
                send sub CompletedEvent
                return EmptyDisposable
    in join ss >>: liftIO . print

testSequencing :: SchedulerIO MainScheduler Disposable
testSequencing = do
    hello >> world >>: liftIO . print
    world >> hello >>: liftIO . print

testAppending :: SchedulerIO MainScheduler Disposable
testAppending = do
    hello
        `mappend` empty
        >>: liftIO . print

    hello
        `mappend` world
        >>: liftIO . print

    world
        `mappend` hello
        >>: liftIO . print

testSubject :: SchedulerIO MainScheduler ()
testSubject = do
    (sub, sig) <- liftIO newSubject
    sig >>: liftIO . print
    send sub $ NextEvent "hello world"

testUnlimitedReplaySubject :: SchedulerIO MainScheduler Disposable
testUnlimitedReplaySubject = do
    (sub, sig) <- liftIO $ newReplaySubject UnlimitedCapacity

    send sub $ NextEvent "hello"
    send sub $ NextEvent "world"
    send sub CompletedEvent

    sig >>: liftIO . print

testLimitedReplaySubject :: SchedulerIO MainScheduler Disposable
testLimitedReplaySubject = do
    (sub, sig) <- liftIO $ newReplaySubject $ LimitedCapacity 2

    send sub $ NextEvent "hello"
    send sub $ NextEvent "world"
    send sub CompletedEvent

    sig >>: liftIO . print

testFirst :: SchedulerIO MainScheduler ()
testFirst = do
    (sub, sig) <- liftIO $ newReplaySubject $ LimitedCapacity 1
    send sub $ NextEvent "foobar"

    ev <- liftIO $ first sig
    liftIO $ print ev

testFilter :: SchedulerIO MainScheduler Disposable
testFilter = do
    hello
        `mappend` world
        `filter` (\(x:xs) -> x == 'h')
        >>: liftIO . print

testDoEvent :: SchedulerIO MainScheduler Disposable
testDoEvent = do
    hello
        `doEvent` (\_ -> liftIO $ putStrLn "event")
        >>: liftIO . print

testDoNext :: SchedulerIO MainScheduler Disposable
testDoNext = do
    hello
        `doNext` (\_ -> liftIO $ putStrLn "next")
        >>: liftIO . print

testDoCompleted :: SchedulerIO MainScheduler Disposable
testDoCompleted = do
    hello
        `doCompleted` (liftIO $ putStrLn "completed")
        >>: liftIO . print

testTake :: SchedulerIO MainScheduler Disposable
testTake = do
    fromFoldable ["foo", "bar", "buzz", "baz"]
        `take` 2
        >>: liftIO . print

testDrop :: SchedulerIO MainScheduler Disposable
testDrop = do
    fromFoldable ["foo", "bar", "buzz", "baz"]
        `drop` 2
        >>: liftIO . print

testZip :: SchedulerIO MainScheduler Disposable
testZip = do
    let zipSub (NextEvent (a, b)) = liftIO $ putStrLn $ a ++ " / " ++ b
        zipSub x = liftIO $ print x

    mzip (fromFoldable ["foo", "bar"]) (fromFoldable ["buzz", "baz"])
        >>: zipSub

testMaterialize :: SchedulerIO MainScheduler Disposable
testMaterialize = do
    materialize hello
        >>: liftIO . print

    dematerialize (materialize hello)
        >>: liftIO . print

testScheduling :: IO ()
testScheduling = do
    s <- newScheduler
    s' <- newScheduler
    mapM_ (schedule s . liftIO . print) [1..50]
    mapM_ (schedule s' . liftIO . print) [1..50]

testScheduledSignal :: IO Disposable
testScheduledSignal = do
    s <- newScheduler
    sig <- start s $ \sub -> do
        send sub $ NextEvent "foo"
        send sub $ NextEvent "bar"
        send sub CompletedEvent

    schedule s $ void (sig >>: liftIO . print)

testMainScheduler :: IO ()
testMainScheduler = do
    s <- getMainScheduler
    schedule s $ liftIO $ putStrLn "hello"
    schedule s $ liftIO $ putStrLn "world"
    runMainScheduler

testMerging :: SchedulerIO MainScheduler ()
testMerging = do
    (sub, sig) <- liftIO newSubject
    (sub', sig') <- liftIO newSubject

    sig `mplus` sig' >>: liftIO . print

    send sub $ NextEvent "foo"
    send sub' $ NextEvent "bar"
    send sub $ NextEvent "fuzz"
    send sub CompletedEvent
    send sub' $ NextEvent "buzz"
    send sub' CompletedEvent

testSwitch :: SchedulerIO MainScheduler ()
testSwitch = do
    (outerSub, outerSig) <- liftIO newSubject
    (innerSub, innerSig) <- liftIO newSubject
    switch outerSig >>: liftIO . print

    send outerSub $ NextEvent $ fromFoldable ["1", "2"]
    send outerSub $ NextEvent innerSig

    send outerSub CompletedEvent

    send innerSub $ NextEvent "3"
    
    send innerSub CompletedEvent

testCombine :: SchedulerIO MainScheduler ()
testCombine = do
    (sub, sig) <- liftIO newSubject
    (sub', sig') <- liftIO newSubject

    sig `combine` sig' >>: liftIO . print

    send sub $ NextEvent "foo"
    send sub' $ NextEvent "bar"
    send sub' $ NextEvent "fuzz"
    send sub' CompletedEvent
    send sub $ NextEvent "buzz"
    send sub CompletedEvent
