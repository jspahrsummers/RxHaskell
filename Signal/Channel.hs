{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Channel ( Channel
                      , newChannel
                      , ChannelCapacity(..)
                      , newReplayChannel
                      , Signal
                      , Subscriber
                      , Scheduler
                      ) where

import Control.Concurrent.STM
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Sequence as Seq
import Disposable
import Prelude hiding (mapM_, length, drop)
import Scheduler
import Signal
import Signal.Subscriber
import Signal.Subscriber.Internal

-- | A controllable signal, represented by a 'Subscriber' and 'Signal' pair.
--
--   Values sent to the subscriber will automatically be broadcast to all of the signal's subscribers.
--   In effect, the subscriber is the write end, while the signal is the read end.
type Channel s v = (Subscriber s v, Signal s v)

-- | Determines how many events a replay channel will save.
data ChannelCapacity = LimitedCapacity Int  -- ^ The channel will only save the specified number of events.
                     | UnlimitedCapacity    -- ^ The channel will save an unlimited number of events.
                     deriving (Eq, Show)

-- | Creates a simple channel which broadcasts all values sent to it.
--
--   Sending an 'ErrorEvent' or 'CompletedEvent' will terminate the channel.
newChannel :: Scheduler s => IO (Channel s v)
newChannel = newReplayChannel $ LimitedCapacity 0

-- | Like 'newChannel', but new subscriptions to the returned signal will receive all values
--   (up to the specified capacity) which have been sent thus far.
--
--   Sending an 'ErrorEvent' or 'CompletedEvent' will terminate the channel. Any terminating event
--   will be replayed to future subscribers, assuming sufficient capacity.
newReplayChannel :: forall s v. Scheduler s => ChannelCapacity -> IO (Channel s v)
newReplayChannel cap = do
    subs <- atomically $ newTVar Seq.empty
    disposed <- atomically $ newTVar False
    events <- atomically $ newTVar Seq.empty

    let addSubscriber :: Subscriber s v -> STM (Seq (Event v), Bool)
        addSubscriber sub = do
            d <- readTVar disposed
            unless d $ modifyTVar' subs (|> sub)

            seq <- readTVar events
            return (seq, d)
        
        s :: Signal s v
        s =
            signal $ \sub -> do
                (events, d) <- liftIO $ atomically $ addSubscriber sub

                -- TODO: Allow these sends to be interrupted through disposal.
                mapM_ (send sub) events
                when d $ liftIO $ disposeSubscriber sub

                return EmptyDisposable

        limit :: Int -> Seq (Event v) -> Seq (Event v)
        limit n seq
            | n <= 0 = Seq.empty
            | length seq > n = drop (length seq - n) seq
            | otherwise = seq

        addEvent' :: ChannelCapacity -> Event v -> Seq (Event v) -> Seq (Event v)
        addEvent' (LimitedCapacity c) ev seq = limit c $ seq |> ev
        addEvent' UnlimitedCapacity ev seq = seq |> ev

        addEvent :: Event v -> STM (Seq (Subscriber s v))
        addEvent ev@(NextEvent _) = do
            d <- readTVar disposed
            unless d $ modifyTVar' events $ addEvent' cap ev
            readTVar subs

        addEvent ev = do
            d <- swapTVar disposed True
            unless d $ modifyTVar' events $ addEvent' cap ev
            swapTVar subs Seq.empty

        onEvent :: Event v -> SchedulerIO s ()
        onEvent ev = do
            subs <- liftIO $ atomically $ addEvent ev
            mapM_ (`send` ev) subs

    sub <- subscriber onEvent
    return (sub, s)
