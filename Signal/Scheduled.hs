{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Scheduled ( start
                        , subscribeOn
                        , deliverOn
                        , first
                        , last
                        , Scheduler
                        , SchedulerIO
                        , Signal
                        ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Disposable
import Prelude hiding (take, last)
import Scheduler
import Signal
import Signal.Channel
import Signal.Operators
import Signal.Subscriber

-- | Starts a signal which executes @action@ on @s@.
start :: Scheduler s => s -> (Subscriber s v -> SchedulerIO s ()) -> IO (Signal s v)
start s action = do
    (sub, sig) <- newReplayChannel UnlimitedCapacity
    schedule s $ action sub
    return sig

-- | Returns a signal which subscribes to @sig@ on scheduler @sch@.
subscribeOn :: forall s v. Scheduler s => Signal s v -> s -> Signal s v
subscribeOn sig sch =
    let onSubscribe :: Subscriber s v -> SchedulerIO s Disposable
        onSubscribe sub = do
            ds <- liftIO newDisposableSet

            schD <- liftIO $ schedule sch $ do
                d <- subscribe sig sub
                liftIO $ ds `addDisposable` d

            liftIO $ ds `addDisposable` schD
            liftIO $ toDisposable ds
    in signal onSubscribe

-- | Returns a signal which subscribes to @sig@ on scheduler @schA@ and delivers its events onto scheduler @schB@.
deliverOn :: forall s t v. (Scheduler s, Scheduler t) => Signal s v -> s -> t -> Signal t v
deliverOn sig schA schB =
    let onSubscribe :: Subscriber t v -> SchedulerIO t Disposable
        onSubscribe sub =
            let forward :: Event v -> SchedulerIO s ()
                forward ev =
                    -- Although we could hold onto any disposable returned from scheduling,
                    -- the complexity of managing all of them probably isn't worth the
                    -- slightly faster cancellation.
                    void $ liftIO $ schedule schB $ send sub ev
            in do
                ds <- liftIO newDisposableSet

                schD <- liftIO $ schedule schA $ do
                    d <- sig >>: forward
                    liftIO $ ds `addDisposable` d

                liftIO $ ds `addDisposable` schD
                liftIO $ toDisposable ds
    in signal onSubscribe

-- | Subscribes to @sig@ and synchronously waits for an event.
first :: forall s v. Scheduler s => Signal s v -> SchedulerIO s (Event v)
first sig = do
    var <- liftIO newEmptyMVar

    let onEvent :: Event v -> SchedulerIO s ()
        onEvent ev = void $ liftIO $ tryPutMVar var ev

    take sig 1 >>: onEvent
    liftIO $ takeMVar var

-- | Subscribes to @sig@ and synchronously waits for the value of the final 'NextEvent' (if any).
last :: forall s v. Scheduler s => Signal s v -> SchedulerIO s (Maybe v)
last sig = do
    previousValue <- liftIO $ newTVarIO Nothing
    lastValue <- liftIO newEmptyTMVarIO

    let onEvent :: Event v -> SchedulerIO s ()
        onEvent (NextEvent v) = liftIO $ atomically $ writeTVar previousValue $ Just v
        onEvent _ = liftIO $ atomically $ readTVar previousValue >>= putTMVar lastValue

    sig >>: onEvent
    liftIO $ atomically $ takeTMVar lastValue
