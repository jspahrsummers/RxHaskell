{-# LANGUAGE Safe #-}

module Scheduler ( Scheduler(ImmediateScheduler)
                 , newScheduler
                 , withScheduler
                 , schedule
                 ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import Data.Maybe
import Disposable
import Scheduler.Internal

-- | Creates a new background scheduler.
newScheduler :: IO Scheduler
newScheduler = DynamicScheduler <$> atomically newTQueue

-- | Runs an action with a new scheduler, then runs all actions enqueued on the scheduler.
withScheduler
    :: Bool                 -- ^ Whether to run the scheduler indefinitely. If 'False', the returned action
                            --   will complete right after all enqueued actions have finished.
    -> (Scheduler -> IO a)  -- ^ A function returning the action to run.
    -> IO a

withScheduler True f = do
    s <- IndefiniteScheduler <$> atomically newTQueue

    a <- f s
    schedulerMain s
    return a

withScheduler False f = do
    s <- newScheduler
    f s

-- | Creates a new scheduled action, and returns a disposable which can be used to cancel it.
newScheduledAction :: IO () -> IO (ScheduledAction, Disposable IO)
newScheduledAction action = do
    ref <- newIORef False
    d <- newDisposable $ atomicModifyIORef ref $ const (True, ())
    return ((ref, action), d)

-- | Schedules @action@ on @s@. Returns a disposable which can be used to cancel it.
schedule :: Scheduler -> IO () -> IO (Disposable IO)
schedule s@(DynamicScheduler q) action = do
    (sa, d) <- newScheduledAction action

    let schedule' = do
            e <- isEmptyTQueue q
            writeTQueue q sa
            return e

    e <- atomically schedule'

    -- If the queue was previously empty, spin up a thread for the scheduler.
    when e $ void $ forkIO $ schedulerMain s

    return d

schedule (IndefiniteScheduler q) action = do
    (sa, d) <- newScheduledAction action
    atomically $ writeTQueue q sa
    return d

schedule ImmediateScheduler action = do
    action
    return EmptyDisposable
