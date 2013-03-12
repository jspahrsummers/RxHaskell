{-# LANGUAGE Safe #-}

module Scheduler ( Scheduler
                 , newScheduler
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
newtype Scheduler = Scheduler (TQueue ScheduledAction)

-- | Creates a new background scheduler.
newScheduler :: IO Scheduler
newScheduler = Scheduler <$> atomically newTQueue

-- | Schedules @action@ on @s@. Returns a disposable which can be used to cancel it.
schedule :: Scheduler -> IO () -> IO (Disposable IO)
schedule s@(Scheduler q) action = do
    ref <- newIORef False

    let schedule' = do
            e <- isEmptyTQueue q
            writeTQueue q (ref, action)
            return e

    e <- atomically schedule'

    -- If the queue was previously empty, spin up a thread for the scheduler.
    when e $ void $ forkIO $ schedulerMain s

    newDisposable $ atomicModifyIORef ref $ const (True, ())

-- | Executes all current and future actions enqueued on the given scheduler.
schedulerMain :: Scheduler -> IO ()
schedulerMain s@(Scheduler q) =
    let schedulerMain' (ref, action) = do
            d <- readIORef ref
            unless d action

            yield
            schedulerMain s
    in do
        m <- atomically $ tryReadTQueue q

        -- If the queue is empty, let this thread die.
        maybe (return ()) schedulerMain' m
