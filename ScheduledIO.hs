{-# LANGUAGE Safe #-}

module ScheduledIO ( ScheduledIO
                   , runScheduledIO
                   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Disposable
import Scheduler

-- | An IO computation that must be performed in a scheduler of type @s@.
newtype ScheduledIO s a = ScheduledIO (IO a)

instance Functor (ScheduledIO s) where
    fmap f (ScheduledIO action) = ScheduledIO $ fmap f action

instance Monad (ScheduledIO s) where
    return = ScheduledIO . return
    (ScheduledIO action) >>= f =
        let unwrap (ScheduledIO action) = action
        in ScheduledIO $ do
            v <- action
            unwrap $ f v

instance MonadIO (ScheduledIO s) where
    liftIO = ScheduledIO

instance Applicative (ScheduledIO s) where
    pure = return
    (<*>) = ap

-- | Runs a ScheduledIO computation (possibly asynchronously) and discards the result.
runScheduledIO
    :: Scheduler s
    => ScheduledIO s a  -- ^ The action to run.
    -> s                -- ^ The scheduler to enqueue the action on.
    -> IO Disposable    -- ^ A disposable which can be used to cancel the computation before it begins to run.

runScheduledIO (ScheduledIO action) s = schedule s $ void action
