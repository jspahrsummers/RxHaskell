{-# LANGUAGE Safe #-}

module Scheduler.Internal ( ScheduledAction
                          , Scheduler(..)
                          , schedulerMain
                          )where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.IORef

-- | Represents an action on a scheduler, along with a flag indicating whether it should be canceled.
type ScheduledAction = (IORef Bool, IO ())

-- | Executes actions serially in FIFO order.
data Scheduler = DynamicScheduler (TQueue ScheduledAction)
               | IndefiniteScheduler (TQueue ScheduledAction)
               | ImmediateScheduler

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
