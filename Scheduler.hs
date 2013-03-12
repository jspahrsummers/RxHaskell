{-# LANGUAGE Safe #-}

module Scheduler ( Scheduler
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

-- | Represents an action on a scheduler, along with a flag indicating whether it should be canceled.
type ScheduledAction = (IORef Bool, IO ())

-- | Executes actions serially in FIFO order.
data Scheduler = DynamicScheduler (TQueue ScheduledAction)
               | IndefiniteScheduler (TQueue ScheduledAction)

-- | Creates a new background scheduler.
newScheduler :: IO Scheduler
newScheduler = DynamicScheduler <$> atomically newTQueue

-- | Runs an action with a scheduler, then runs all actions enqueued on the scheduler.
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

-- | Executes the given action, then re-enters 'schedulerMain'.
executeScheduledAction :: Scheduler -> ScheduledAction -> IO ()
executeScheduledAction s (ref, action) = do
    d <- readIORef ref
    unless d action

    yield
    schedulerMain s

-- | Executes all current and future actions enqueued on the given scheduler.
schedulerMain :: Scheduler -> IO ()
schedulerMain s@(DynamicScheduler q) = do
    m <- atomically $ tryReadTQueue q

    -- If the queue is empty, let this thread die.
    maybe (return ()) (executeScheduledAction s) m

schedulerMain s@(IndefiniteScheduler q) = do
    sa <- atomically $ readTQueue q
    executeScheduledAction s sa
