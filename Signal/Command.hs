{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Command ( Command
                      , canExecute
                      , newCommand
                      , executing
                      , execute
                      , errors
                      , values
                      --, onExecute
                      , CommandPolicy(..)
                      ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Word
import Scheduler
import Signal
import Signal.Channel
import Signal.Operators
import Signal.Scheduled
import Signal.Subscriber

-- | Determines a command's behavior.
data CommandPolicy = ExecuteSerially        -- ^ The command can only be executed once at a time.
                                            --   Attempts to 'execute' while the command is already running will fail.
                   | ExecuteConcurrently    -- ^ The command can be executed concurrently any number of times.
                   deriving (Eq, Show)

-- | A signal triggered in response to some action, typically UI-related.
data Command s v = Command {
    policy :: CommandPolicy,
    -- TODO: These channels never complete, causing subscribers to leak.
    valuesChannel :: Channel s v,
    errorsChannel :: Channel MainScheduler IOException,
    itemsInFlight :: MVar Word32,
    executingChannel :: Channel s Bool,
    canExecuteChannel :: Channel s Bool
}

-- | Creates a command.
newCommand
    :: Scheduler s
    => CommandPolicy    -- ^ Determines how the command behaves when asked to 'execute' multiple times simultaneously.
    -> Signal s Bool    -- ^ A signal which sends whether the command should be enabled.
                        --   'canExecute' will send 'True' before this signal sends its first value.
    -> SchedulerIO s (Command s v)

newCommand p ces = do
    vChan <- liftIO newChannel
    errChan <- liftIO newChannel
    exChan <- liftIO $ newReplayChannel $ LimitedCapacity 1
    ceChan <- liftIO $ newReplayChannel $ LimitedCapacity 1
    items <- liftIO $ newMVar 0

    let onEvent (NextEvent (ce, ex)) = send (fst ceChan) $ NextEvent $ ce && not ex
        onEvent _ = return ()

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

    return command

-- | Sends whether this command is currently executing.
-- | This signal will always send at least one value immediately upon subscription.
executing :: Scheduler s => Command s v -> Signal s Bool
executing = snd . executingChannel

-- | Sends a new value for 'executing'.
setExecuting :: Scheduler s => Command s v -> Bool -> SchedulerIO s ()
setExecuting c b = send (fst $ executingChannel c) $ NextEvent b

-- | A signal of errors received from all signals created by 'doExecute'.
errors :: Command s v -> Signal MainScheduler IOException
errors = snd . errorsChannel

-- | A signal of the values passed to 'execute'.
values :: Scheduler s => Command s v -> Signal s v
values = snd . valuesChannel

-- | Sends whether this command is able to execute.
-- | This signal will always send at least one value immediately upon subscription.
canExecute :: Scheduler s => Command s v -> Signal s Bool
canExecute = snd . canExecuteChannel

-- | Attempts to execute a command.
execute
    :: forall s v. Scheduler s
    => Command s v          -- ^ The command to execute.
    -> v                    -- ^ A value to send to the command's subscribers.
    -> SchedulerIO s Bool   -- ^ Whether execution succeeded. This will be 'False' if the command is disabled.

execute c v = do
    items <- liftIO $ takeMVar $ itemsInFlight c

    let execute' :: SchedulerIO s Bool
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

{-
TODO

-- | Creates a signal whenever the command executes, then subscribes to it.
onExecute
    :: MonadIO m
    => CommandM m v         -- ^ The command to attach behavior to.
    -> (v -> SignalM m ())  -- ^ A function which maps from the command's values to a signal of side-effecting work.
    -> m ()                 -- ^ An action which subscribes to the command with the given function.

onExecute c f = return ()
-}
