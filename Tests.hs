module Tests where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Zip
import Data.Monoid
import Prelude hiding (filter, take, drop)
import Disposable
import Scheduler
import Scheduler.Internal (unsafeRunSchedulerIO)
import Scheduler.Main
import Scheduler.Unsafe
import Signal
import Signal.Channel
import Signal.Command
import Signal.Connection
import Signal.Operators
import Signal.Scheduled
import System.Mem

hello = fromFoldable ["hello"]
world = fromFoldable ["world"]

testBinding :: SchedulerIO MainScheduler ()
testBinding =
    let ss =
            signal $ \sub -> do
                send sub $ NextEvent hello
                send sub $ NextEvent world
                send sub CompletedEvent
                return EmptyDisposable
    in void $ join ss >>: liftIO . print

testSequencing :: SchedulerIO MainScheduler ()
testSequencing = do
    hello >> world >>: liftIO . print
    world >> hello >>: liftIO . print
    return ()

testAppending :: SchedulerIO MainScheduler ()
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

    return ()

testChannel :: SchedulerIO MainScheduler ()
testChannel = do
    (sub, sig) <- liftIO newChannel
    sig >>: liftIO . print
    send sub $ NextEvent "hello world"

testUnlimitedReplayChannel :: SchedulerIO MainScheduler ()
testUnlimitedReplayChannel = do
    (sub, sig) <- liftIO $ newReplayChannel UnlimitedCapacity

    send sub $ NextEvent "hello"
    send sub $ NextEvent "world"
    send sub CompletedEvent

    void $ sig >>: liftIO . print

testLimitedReplayChannel :: SchedulerIO MainScheduler ()
testLimitedReplayChannel = do
    (sub, sig) <- liftIO $ newReplayChannel $ LimitedCapacity 2

    send sub $ NextEvent "hello"
    send sub $ NextEvent "world"
    send sub CompletedEvent

    void $ sig >>: liftIO . print

testFirst :: SchedulerIO MainScheduler ()
testFirst = do
    (sub, sig) <- liftIO $ newReplayChannel $ LimitedCapacity 1
    send sub $ NextEvent "foobar"

    ev <- liftIO $ first sig
    liftIO $ print ev

testFilter :: SchedulerIO MainScheduler ()
testFilter = do
    hello
        `mappend` world
        `filter` (\(x:xs) -> x == 'h')
        >>: liftIO . print

    return ()

testDoEvent :: SchedulerIO MainScheduler ()
testDoEvent = do
    hello
        `doEvent` (\_ -> liftIO $ putStrLn "event")
        >>: liftIO . print

    return ()

testDoNext :: SchedulerIO MainScheduler ()
testDoNext = do
    hello
        `doNext` (\_ -> liftIO $ putStrLn "next")
        >>: liftIO . print

    return ()

testDoCompleted :: SchedulerIO MainScheduler ()
testDoCompleted = do
    hello
        `doCompleted` (liftIO $ putStrLn "completed")
        >>: liftIO . print

    return ()

testTake :: SchedulerIO MainScheduler ()
testTake = do
    fromFoldable ["foo", "bar", "buzz", "baz"]
        `take` 2
        >>: liftIO . print

    return ()

testDrop :: SchedulerIO MainScheduler ()
testDrop = do
    fromFoldable ["foo", "bar", "buzz", "baz"]
        `drop` 2
        >>: liftIO . print

    return ()

testZip :: SchedulerIO MainScheduler ()
testZip = do
    let zipSub (NextEvent (a, b)) = liftIO $ putStrLn $ a ++ " / " ++ b
        zipSub x = liftIO $ print x

    mzip (fromFoldable ["foo", "bar"]) (fromFoldable ["buzz", "baz"])
        >>: zipSub

    return ()

testMaterialize :: SchedulerIO MainScheduler ()
testMaterialize = do
    materialize hello
        >>: liftIO . print

    dematerialize (materialize hello)
        >>: liftIO . print

    return ()

testScheduling :: IO ()
testScheduling = do
    s <- newScheduler
    s' <- newScheduler
    mapM_ (schedule s . liftIO . print) [1..50]
    mapM_ (schedule s' . liftIO . print) [1..50]

testScheduledSignal :: IO ()
testScheduledSignal = do
    s <- newScheduler
    sig <- start s $ \sub -> do
        send sub $ NextEvent "foo"
        send sub $ NextEvent "bar"
        send sub CompletedEvent

    void $ schedule s $ void (sig >>: liftIO . print)

testMainScheduler :: IO ()
testMainScheduler = do
    s <- getMainScheduler
    schedule s $ liftIO $ putStrLn "hello"
    schedule s $ liftIO $ putStrLn "world"
    runMainScheduler

testMerging :: SchedulerIO MainScheduler ()
testMerging = do
    (sub, sig) <- liftIO newChannel
    (sub', sig') <- liftIO newChannel

    sig `mplus` sig' >>: liftIO . print

    send sub $ NextEvent "foo"
    send sub' $ NextEvent "bar"
    send sub $ NextEvent "fuzz"
    send sub CompletedEvent
    send sub' $ NextEvent "buzz"
    send sub' CompletedEvent

testSwitch :: SchedulerIO MainScheduler ()
testSwitch = do
    (outerSub, outerSig) <- liftIO newChannel
    (innerSub, innerSig) <- liftIO newChannel
    switch outerSig >>: liftIO . print

    send outerSub $ NextEvent $ fromFoldable ["1", "2"]
    send outerSub $ NextEvent innerSig

    send outerSub CompletedEvent

    send innerSub $ NextEvent "3"
    
    send innerSub CompletedEvent

testCombine :: SchedulerIO MainScheduler ()
testCombine = do
    (sub, sig) <- liftIO newChannel
    (sub', sig') <- liftIO newChannel

    sig `combine` sig' >>: liftIO . print

    send sub $ NextEvent "foo"
    send sub' $ NextEvent "bar"
    send sub' $ NextEvent "fuzz"
    send sub' CompletedEvent
    send sub $ NextEvent "buzz"
    send sub CompletedEvent

testConnection :: SchedulerIO MainScheduler ()
testConnection = do
    conn <- liftIO $ publish $ hello `mappend` world

    multicastedSignal conn >>: liftIO . print
    void $ connect conn

testReplay :: SchedulerIO MainScheduler ()
testReplay = do
    sig <- replay $ hello `mappend` world
    void $ sig >>: liftIO . print

testCommand :: SchedulerIO MainScheduler Bool
testCommand = do
    c <- newCommand ExecuteSerially $ return True

    executing c >>: liftIO . print
    values c >>: liftIO . print

    execute c 5

testOnExecute :: IO ()
testOnExecute = do
    sch <- getMainScheduler

    schedule sch $ do
        c <- newCommand ExecuteConcurrently $ return True

        onExecute c $ \v ->
            signal $ \sub -> do
                liftIO $ schedule sch $ mapM_ (liftIO . print) v
                send sub $ ErrorEvent $ userError "Test error"
                return EmptyDisposable

        executing c >>: \e -> liftIO $ putStrLn $ "executing: " ++ show e
        errors c >>: \err -> liftIO $ putStrLn $ "error: " ++ show err

        execute c [1..20]
        execute c [20..40]
        return ()

    runMainScheduler

testCommandCompletion :: IO ()
testCommandCompletion = do
    sch <- getMainScheduler

    schedule sch $ do
        c <- newCommand ExecuteSerially $ return True

        executing c >>: \e -> liftIO $ putStrLn $ "executing: " ++ show e
        return ()

    schedule sch $ liftIO performGC
    runMainScheduler

testSubscriberDisposal :: SchedulerIO MainScheduler ()
testSubscriberDisposal =
    let s = signal $ \sub -> do
        send sub $ NextEvent "hello"
        send sub CompletedEvent
        send sub $ NextEvent "world"
        return EmptyDisposable
    in void $ s >>: liftIO . print
