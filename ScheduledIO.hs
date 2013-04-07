{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}

module ScheduledIO ( ScheduledIO(..)
                   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Scheduler

-- | An IO computation that must be performed in a scheduler of type @s@.
data ScheduledIO s a where
    ScheduledIO :: Scheduler s => IO a -> ScheduledIO s a

instance Functor (ScheduledIO s) where
    fmap f (ScheduledIO action) = ScheduledIO $ fmap f action

instance Scheduler s => Monad (ScheduledIO s) where
    return = ScheduledIO . return
    (ScheduledIO action) >>= f =
        let unwrap (ScheduledIO action) = action
        in ScheduledIO $ do
            v <- action
            unwrap $ f v

instance Scheduler s => MonadIO (ScheduledIO s) where
    liftIO = ScheduledIO

instance Scheduler s => Applicative (ScheduledIO s) where
    pure = return
    (<*>) = ap
