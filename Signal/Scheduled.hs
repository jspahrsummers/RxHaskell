{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Scheduled ( start
                        , Scheduler
                        , newScheduler
                        , subscribeOn
                        , deliverOn
                        , first
                        ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Disposable
import Prelude hiding (take)
import Scheduler
import Scheduler.Internal
import Signal
import Signal.Operators
import Subject

-- | Starts a signal which executes @action@ on @s@.
start :: Scheduler s => s -> (Subscriber s v -> SchedulerIO s ()) -> IO (Signal s v)
start s action = do
    (sub, sig) <- newReplaySubject UnlimitedCapacity
    schedule s $ action sub
    return sig

-- | Returns a signal which subscribes to @sig@ on scheduler @sch@.
subscribeOn :: forall s t v. (Scheduler s, Scheduler t) => Signal s v -> t -> Signal t v
subscribeOn sig sch =
    let onSubscribe :: Subscriber t v -> SchedulerIO t Disposable
        onSubscribe sub = do
            ds <- liftIO newDisposableSet

            let forward :: Event v -> SchedulerIO s ()
                forward ev = SchedulerIO $ unsafeRunSchedulerIO $ sub `send` ev

                subscribe :: SchedulerIO t ()
                subscribe = do
                    d <- SchedulerIO $ unsafeRunSchedulerIO $ sig >>: forward
                    liftIO $ ds `addDisposable` d

            schD <- liftIO $ sch `schedule` subscribe

            liftIO $ ds `addDisposable` schD
            liftIO $ toDisposable ds
    in signal onSubscribe

-- | Returns a signal which delivers the events of @sig@ on scheduler @sch@.
deliverOn :: forall s t v. (Scheduler s, Scheduler t) => Signal s v -> t -> Signal t v
deliverOn sig sch =
    let onSubscribe :: Subscriber t v -> SchedulerIO t Disposable
        onSubscribe sub = do
            -- Although we could hold onto any disposable returned from scheduling,
            -- the complexity of managing all of them probably isn't worth the
            -- slightly faster cancellation.
            let deliver :: t -> Event v -> SchedulerIO s Disposable
                deliver sch ev =
                    let sio = SchedulerIO $ unsafeRunSchedulerIO $ sub `send` ev
                    in liftIO $ sch `schedule` sio

                forward :: Event v -> SchedulerIO s ()
                forward ev = void $ deliver sch ev

            SchedulerIO $ unsafeRunSchedulerIO $ sig >>: forward
    in signal onSubscribe

-- | Synchronously waits for the signal to send an event.
first :: forall s v. Scheduler s => Signal s v -> IO (Event v)
first s = do
    var <- newEmptyMVar

    let onEvent :: Event v -> SchedulerIO s ()
        onEvent ev = void $ liftIO $ tryPutMVar var ev
        
        subscribe :: SchedulerIO s Disposable
        subscribe = take s 1 >>: onEvent

    unsafeRunSchedulerIO subscribe
    ev <- takeMVar var

    return ev
