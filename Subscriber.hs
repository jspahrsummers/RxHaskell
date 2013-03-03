{-# LANGUAGE Safe #-}

module Subscriber ( Subscriber
                  , subscriber
                  , send
                  , OnEvent
                  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.Word
import Disposable
import Event

-- | A function to run when a signal sends an event.
type OnEvent a = Event a -> IO ()

-- | Receives events from a signal.
data Subscriber a = Subscriber {
    onEvent :: OnEvent a,
    disposable :: Disposable,
    lockedThread :: TVar ThreadId,
    threadLockCounter :: TVar Word32
}

-- | Constructs a subscriber.
subscriber :: OnEvent a -> IO (Subscriber a)
subscriber f = do
    d <- newDisposable $ return ()

    tid <- myThreadId
    lt <- atomically $ newTVar tid
    tlc <- atomically $ newTVar 0

    return $ Subscriber {
        onEvent = f,
        disposable = d,
        lockedThread = lt,
        threadLockCounter = tlc
    }

-- | Acquires a subscriber for the specified thread.
acquireSubscriber :: Subscriber a -> ThreadId -> STM ()
acquireSubscriber sub tid = do
    tlc <- readTVar (threadLockCounter sub)
    lt <- readTVar (lockedThread sub)
    if tlc > 0 && lt /= tid then retry else return ()

    writeTVar (lockedThread sub) tid
    writeTVar (threadLockCounter sub) $ tlc + 1

-- | Releases a subscriber from the specified thread's ownership.
releaseSubscriber :: Subscriber a -> ThreadId -> STM ()
releaseSubscriber sub tid = do
    always $ fmap (== tid) $ readTVar (lockedThread sub)

    tlc <- readTVar (threadLockCounter sub)
    always $ return $ tlc > 0

    writeTVar (threadLockCounter sub) $ tlc - 1

-- | Sends an event to a subscriber.
send :: Subscriber a -> Event a -> IO ()
send s ev =
    let send' s ev@(NextEvent _) = onEvent s ev
        send' s ev = dispose (disposable s) >> onEvent s ev
    in do
        tid <- myThreadId
        atomically $ acquireSubscriber s tid
        send' s ev
        atomically $ releaseSubscriber s tid

instance Eq (Subscriber a) where
    a == b = (disposable a) == (disposable b)
