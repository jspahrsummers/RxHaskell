{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Connection ( Connection
                         , multicast
                         , publish
                         , connect
                         , multicastedSignal
                         , replay
                         , replayLast
                         , Channel
                         , Signal
                         , Scheduler
                         , SchedulerIO
                         , Disposable
                         ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Disposable
import Scheduler
import Signal
import Signal.Channel

-- | Multicasts a signal to many subscribers, without triggering any side effects more than once.
data Connection s v = Connection {
    baseSignal :: Signal s v,
    disposable :: TVar Disposable,
    channel :: Channel s v,
    hasConnected :: TMVar Bool
}

-- | Creates a connection that will subscribe to the given base signal,
--   and forward all events onto the given channel.
multicast :: Scheduler s => Signal s v -> Channel s v -> IO (Connection s v)
multicast sig chan = do
    d <- atomically $ newTVar EmptyDisposable
    hc <- atomically $ newTMVar False

    return Connection {
        baseSignal = sig,
        disposable = d,
        channel = chan,
        hasConnected = hc
    }

-- | Multicasts to a simple channel.
publish :: Scheduler s => Signal s v -> IO (Connection s v)
publish sig = newChannel >>= multicast sig

-- | Multicasts to a replay channel of unlimited capacity, then connects immediately.
replay :: Scheduler s => Signal s v -> SchedulerIO s (Signal s v)
replay sig = do
    chan <- liftIO $ newReplayChannel UnlimitedCapacity
    conn <- liftIO $ multicast sig chan
    
    connect conn
    return $ multicastedSignal conn

-- | Multicasts to a replay channel of capacity 1, then connects immediately.
replayLast :: Scheduler s => Signal s v -> SchedulerIO s (Signal s v)
replayLast sig = do
    chan <- liftIO $ newReplayChannel $ LimitedCapacity 1
    conn <- liftIO $ multicast sig chan
    
    connect conn
    return $ multicastedSignal conn

-- | Returns the multicasted signal of a connection.
--
--   No events will be sent on the resulting signal until 'connect' is invoked.
multicastedSignal :: Connection s v -> Signal s v
multicastedSignal conn = snd $ channel conn

-- | Activates a connection by subscribing to its underlying signal.
--   Calling this function multiple times just returns the existing disposable.
connect :: forall s v. Scheduler s => Connection s v -> SchedulerIO s Disposable
connect conn =
    let connect' :: SchedulerIO s Disposable
        connect' = do
            d <- baseSignal conn `subscribe` fst (channel conn)
            liftIO $ atomically $ setDisposable d
            return d

        setDisposable :: Disposable -> STM ()
        setDisposable d = do
            putTMVar (hasConnected conn) True
            writeTVar (disposable conn) d
            
        shouldConnect :: STM (Bool, Disposable)
        shouldConnect = do
            hc <- takeTMVar $ hasConnected conn
            d <- if hc
                then putTMVar (hasConnected conn) hc >> return EmptyDisposable
                -- If we're going to connect, leave the MVar empty until we've filled in the disposable.
                else readTVar $ disposable conn

            return (not hc, d)
    in do
        (shouldConnect, d) <- liftIO $ atomically shouldConnect
        if shouldConnect
            then connect'
            else return d
