{-# LANGUAGE Safe #-}

module Subscriber ( Subscriber
                  , subscriber
                  , send
                  , Event(..)
                  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Disposable
import Event
import ScheduledIO
import Scheduler

-- | Receives events from a signal with values of type @v@ and running in a scheduler of type @s@.
-- |
-- | Note that @s@ refers to the scheduler that events must be sent on. Events are always sent
-- | synchronously, regardless of @s@.
data Subscriber s v = Subscriber {
    onEvent :: Event v -> ScheduledIO s (),
    disposable :: Disposable,
    lockedThread :: TVar ThreadId,
    threadLockCounter :: TVar Word32,
    disposed :: TVar Bool
}

-- | Constructs a subscriber.
subscriber :: Scheduler s => (Event v -> ScheduledIO s ()) -> IO (Subscriber s v)
subscriber f = do
    b <- atomically $ newTVar False
    d <- newDisposable $ atomically $ writeTVar b True

    tid <- myThreadId
    lt <- atomically $ newTVar tid
    tlc <- atomically $ newTVar 0

    return Subscriber {
        onEvent = f,
        disposable = d,
        lockedThread = lt,
        threadLockCounter = tlc,
        disposed = b
    }

-- | Acquires a subscriber for the specified thread.
-- | This is used to ensure that a subscriber never receives multiple events concurrently.
acquireSubscriber :: Subscriber s v -> ThreadId -> STM Bool
acquireSubscriber sub tid = do
    d <- readTVar (disposed sub)
    if d
        then return False
        else do
            -- TODO: Skip all this synchronization for singleton scheduler types.
            tlc <- readTVar (threadLockCounter sub)
            lt <- readTVar (lockedThread sub)
            when (tlc > 0 && lt /= tid) retry

            writeTVar (lockedThread sub) tid
            writeTVar (threadLockCounter sub) $ tlc + 1
            return True

-- | Releases a subscriber from the specified thread's ownership.
releaseSubscriber :: Subscriber s v -> ThreadId -> STM ()
releaseSubscriber sub tid = do
    -- TODO: Skip all this synchronization for singleton scheduler types.
    always $ fmap (== tid) $ readTVar (lockedThread sub)

    tlc <- readTVar (threadLockCounter sub)
    always $ return $ tlc > 0

    writeTVar (threadLockCounter sub) $ tlc - 1

-- | Synchronously sends an event to a subscriber.
send :: Scheduler s => Subscriber s v -> Event v -> ScheduledIO s ()
send s ev =
    let send' ev@(NextEvent _) = onEvent s ev
        send' ev = do
            d <- liftIO $ readTVarIO $ disposed s
            unless d $ onEvent s ev
    in do
        tid <- liftIO myThreadId
        b <- liftIO $ atomically $ acquireSubscriber s tid

        when b $ send' ev >> liftIO (atomically (releaseSubscriber s tid))
