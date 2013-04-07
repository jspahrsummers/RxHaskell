{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}

module Scheduler.IO ( SchedulerIO(..)
                    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Scheduler

-- | An IO computation that must be performed in a scheduler of type @s@.
data SchedulerIO s a where
    SchedulerIO :: Scheduler s => IO a -> SchedulerIO s a

instance Functor (SchedulerIO s) where
    fmap f (SchedulerIO action) = SchedulerIO $ fmap f action

instance Scheduler s => Monad (SchedulerIO s) where
    return = SchedulerIO . return
    (SchedulerIO action) >>= f =
        let unwrap (SchedulerIO action) = action
        in SchedulerIO $ do
            v <- action
            unwrap $ f v

instance Scheduler s => MonadIO (SchedulerIO s) where
    liftIO = SchedulerIO

instance Scheduler s => Applicative (SchedulerIO s) where
    pure = return
    (<*>) = ap
