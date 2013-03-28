{-# LANGUAGE Safe #-}

module Scheduler.Internal ( ScheduledAction
                          , Scheduler(..)
                          , ImmediateScheduler(..)
                          , BackgroundScheduler(..)
                          , MainScheduler(..)
                          , executeScheduledAction
                          ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import Disposable

-- | Represents an action on a scheduler, along with a flag indicating whether it should be canceled.
type ScheduledAction = (IORef Bool, IO ())

-- | Creates a new scheduled action, and returns a disposable which can be used to cancel it.
newScheduledAction :: IO () -> IO (ScheduledAction, Disposable IO)
newScheduledAction action = do
    ref <- newIORef False
    d <- newDisposable $ atomicModifyIORef ref $ const (True, ())
    return ((ref, action), d)

-- | Represents a queue of IO actions which can be executed in FIFO order.
class Scheduler s where
    -- | Schedules an action on the scheduler. Returns a disposable which can be used to cancel it.
    schedule :: s -> IO () -> IO (Disposable IO)

    -- | Executes all current and future actions enqueued on the given scheduler.
    schedulerMain :: s -> IO ()

-- | A scheduler which runs actions immediately, blocking the caller.
data ImmediateScheduler = ImmediateScheduler

instance Scheduler ImmediateScheduler where
    schedule _ action = do
        action
        return EmptyDisposable

    schedulerMain _ = return ()

-- | A scheduler which runs enqueued actions in a dedicated background thread.
newtype BackgroundScheduler = BackgroundScheduler (TQueue ScheduledAction)

instance Scheduler BackgroundScheduler where
    schedule s@(BackgroundScheduler q) action = do
        (sa, d) <- newScheduledAction action

        let schedule' = do
                e <- isEmptyTQueue q
                writeTQueue q sa
                return e

        e <- atomically schedule'

        -- If the queue was previously empty, spin up a thread for the scheduler.
        when e $ void $ forkIO $ schedulerMain s

        return d

    schedulerMain s@(BackgroundScheduler q) = do
        m <- atomically $ tryReadTQueue q

        -- If the queue is empty, let this thread die.
        maybe (return ()) (executeScheduledAction s) m

-- | A scheduler which runs enqueued actions on the main thread.
newtype MainScheduler = MainScheduler (TQueue ScheduledAction)

instance Scheduler MainScheduler where
    schedule (MainScheduler q) action = do
        (sa, d) <- newScheduledAction action
        atomically $ writeTQueue q sa
        return d

    schedulerMain s@(MainScheduler q) = do
        sa <- atomically $ readTQueue q
        executeScheduledAction s sa

-- | Executes the given action, then re-enters 'schedulerMain'.
executeScheduledAction :: Scheduler s => s -> ScheduledAction -> IO ()
executeScheduledAction s (ref, action) = do
    d <- readIORef ref
    unless d action

    yield
    schedulerMain s
