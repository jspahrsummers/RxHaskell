{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Command ( Command
                      , CommandPolicy(..)
                      , newCommand
                      , canExecute
                      , executing
                      , execute
                      , values
                      , onExecute
                      , errors
                      , Channel
                      , Signal
                      , Scheduler
                      , SchedulerIO
                      , BackgroundScheduler
                      , MainScheduler
                      ) where

import Control.Concurrent.MVar
import Control.Exception hiding (finally)
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Word
import Prelude hiding (map)
import Scheduler
import Scheduler.Main
import Signal
import Signal.Channel
import Signal.Connection
import Signal.Operators
import Signal.Scheduled
import Signal.Subscriber
import System.Mem.Weak

-- | Determines a command's behavior.
data CommandPolicy = ExecuteSerially        -- ^ The command can only be executed once at a time.
                                            --   Attempts to 'execute' while the command is already running will fail.
                   | ExecuteConcurrently    -- ^ The command can be executed concurrently any number of times.
                   deriving (Eq, Show)

-- | A signal triggered in response to some action, typically UI-related.
data Command v = Command {
    policy :: CommandPolicy,
    valuesChannel :: Channel MainScheduler v,
    errorsChannel :: Channel MainScheduler IOException,
    itemsInFlight :: MVar Word32,
    executingChannel :: Channel MainScheduler Bool,
    canExecuteChannel :: Channel MainScheduler Bool
}

-- | Creates a command.
newCommand
    :: CommandPolicy                -- ^ Determines how the command behaves when asked to 'execute' multiple times simultaneously.
    -> Signal MainScheduler Bool    -- ^ A signal which sends whether the command should be enabled.
                                    --   'canExecute' will send 'True' before this signal sends its first value.
    -> SchedulerIO MainScheduler (Command v)

newCommand p ces = do
    vChan <- liftIO newChannel
    errChan <- liftIO newChannel
    exChan <- liftIO $ newReplayChannel $ LimitedCapacity 1
    ceChan <- liftIO $ newReplayChannel $ LimitedCapacity 1
    items <- liftIO $ newMVar 0

    let canExecute :: Bool -> Bool -> CommandPolicy -> Bool
        canExecute ce executing ExecuteSerially = ce && not executing
        canExecute ce _ ExecuteConcurrently = ce
    
        onEvent :: Event (Bool, Bool) -> SchedulerIO MainScheduler ()
        onEvent (NextEvent (ce, ex)) = send (fst ceChan) $ NextEvent $ canExecute ce ex p
        onEvent _ = return ()

        complete :: IO ()
        complete = do
            sch <- getMainScheduler
            void $ schedule sch $ do
                send (fst vChan) CompletedEvent
                send (fst errChan) CompletedEvent
                send (fst exChan) CompletedEvent
                send (fst ceChan) CompletedEvent

    -- Start with True for 'canExecute'.
    combine (return True `mappend` ces) (snd exChan) >>: onEvent

    let command = Command {
            policy = p,
            valuesChannel = vChan,
            errorsChannel = errChan,
            itemsInFlight = items,
            executingChannel = exChan,
            canExecuteChannel = ceChan
        }

    -- Set the starting value for 'executing'.
    setExecuting command False

    liftIO $ addFinalizer command complete
    return command

-- | Sends whether this command is currently executing.
--
--   This signal will always send at least one value immediately upon subscription.
executing :: Command v -> Signal MainScheduler Bool
executing = snd . executingChannel

-- | A signal of errors received from all signals created by 'onExecute'.
errors :: Command v -> Signal MainScheduler IOException
errors = snd . errorsChannel

-- | A signal of the values passed to 'execute'.
values :: Command v -> Signal MainScheduler v
values = snd . valuesChannel

-- | Sends whether this command is able to execute.
--
--   This signal will always send at least one value immediately upon subscription.
canExecute :: Command v -> Signal MainScheduler Bool
canExecute = snd . canExecuteChannel

-- | Sends a new value for 'executing'.
setExecuting :: Command v -> Bool -> SchedulerIO MainScheduler ()
setExecuting c b = send (fst $ executingChannel c) $ NextEvent b

-- | Attempts to execute a command.
execute
    :: Command v                        -- ^ The command to execute.
    -> v                                -- ^ A value to send to the command's subscribers.
    -> SchedulerIO MainScheduler Bool   -- ^ Whether execution succeeded. This will be 'False' if the command is disabled.

execute c v = do
    items <- liftIO $ takeMVar $ itemsInFlight c

    let execute' :: SchedulerIO MainScheduler Bool
        execute' = do
            liftIO $ putMVar (itemsInFlight c) $ items + 1

            setExecuting c True
            fst (valuesChannel c) `send` NextEvent v

            items <- liftIO $ modifyMVar (itemsInFlight c) $ \items ->
                return $ (items - 1, items - 1)

            when (items == 0) $ setExecuting c False
            return True

    ce <- liftIO $ first $ canExecute c

    case ce of
        (NextEvent True) -> execute'
        _ -> do
            liftIO $ putMVar (itemsInFlight c) items
            return False

-- | Creates a signal whenever the command executes, then subscribes to it.
onExecute
    :: Command v                                                                        -- ^ The command to attach behavior to.
    -> (v -> Signal BackgroundScheduler ())                                             -- ^ A function which maps from the command's values to a signal of side-effecting work.
    -> SchedulerIO MainScheduler (Signal MainScheduler (Signal BackgroundScheduler ())) -- ^ A signal of the created signals.

onExecute c f =
    let incItems :: SchedulerIO MainScheduler ()
        incItems = do
            liftIO $ modifyMVar_ (itemsInFlight c) $ \items -> return $ items + 1
            setExecuting c True

        decItems :: SchedulerIO BackgroundScheduler ()
        decItems = do
            mainSch <- liftIO getMainScheduler
            void $ liftIO $ schedule mainSch $ do
                items <- liftIO $ modifyMVar (itemsInFlight c) $ \items -> return (items - 1, items - 1)
                when (items == 0) $ setExecuting c False

        sendError :: IOException -> IO ()
        sendError ex = do
            sch <- getMainScheduler
            void $ schedule sch $ send (fst $ errorsChannel c) $ NextEvent ex

        coldSignals :: Signal MainScheduler (Signal BackgroundScheduler ())
        coldSignals =
            values c
                `doNext` const incItems
                `map` \v ->
                    f v
                        `doError` (liftIO . sendError)
                        `finally` decItems

        signals :: Signal MainScheduler (Signal BackgroundScheduler ())
        signals =
            coldSignals >>= \sig ->
                signal $ \sub -> do
                    sch <- liftIO newScheduler
                    chan <- liftIO $ newReplayChannel UnlimitedCapacity
                    conn <- liftIO $ multicast sig chan

                    d <- liftIO $ schedule sch $ void $ connect conn
                    send sub $ NextEvent $ multicastedSignal conn
                    return d
    in replayLast signals
