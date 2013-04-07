{-# LANGUAGE Safe #-}

module Channel ( newChannel
               , newReplayChannel
               , Channel
               , Subscriber
               , send
               , ChannelCapacity(..)
               ) where

import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.Sequence as Seq
import Data.Traversable
import Disposable
import Prelude hiding (mapM_, length, drop)
import Scheduler
import Signal
import Subscriber

-- | A controllable signal, represented by a subscriber and signal pair.
-- |
-- | Values sent to the subscriber will automatically be broadcast to all of the signal's subscribers.
-- | In effect, the subscriber is the "write" end, while the signal is the "read" end.
type Channel s v = (Subscriber s v, Signal s v)

-- | Determines how many events a replay channel will save.
data ChannelCapacity = LimitedCapacity Int  -- ^ The channel will only save the specified number of events.
                     | UnlimitedCapacity    -- ^ The channel will save an unlimited number of events.
                     deriving (Eq, Show)

-- | Creates a simple channel which broadcasts all values sent to it.
newChannel :: Scheduler s => IO (Channel s v)
newChannel = do
    subsRef <- newIORef Seq.empty

    let s =
            signal $ \sub ->
                liftIO $ atomicModifyIORef subsRef $ \seq -> (seq |> sub, EmptyDisposable)

        onEvent ev = do
            subs <- liftIO $ readIORef subsRef
            mapM_ (`send` ev) subs

    sub <- subscriber onEvent
    return (sub, s)

-- | Like 'newChannel', but new subscriptions to the returned signal will receive all values
-- | (up to the specified capacity) which have been sent thus far.
newReplayChannel :: Scheduler s => ChannelCapacity -> IO (Channel s v)
newReplayChannel cap = do
    subsVar <- atomically $ newTVar Seq.empty
    eventsVar <- atomically $ newTVar Seq.empty

    let addSubscriber sub = do
            modifyTVar' subsVar (|> sub)
            readTVar eventsVar
        
        s =
            signal $ \sub -> do
                events <- liftIO $ atomically $ addSubscriber sub
                mapM_ (send sub) events
                return EmptyDisposable

        limit n seq =
            if length seq > n
            then drop (length seq - n) seq
            else seq

        addEvent ev =
            let addEvent' (LimitedCapacity c) seq = limit c $ seq |> ev
                addEvent' UnlimitedCapacity seq = seq |> ev
            in do
                modifyTVar' eventsVar (addEvent' cap)
                readTVar subsVar

        onEvent ev = do
            subs <- liftIO $ atomically $ addEvent ev
            mapM_ (`send` ev) subs

    sub <- subscriber onEvent
    return (sub, s)
