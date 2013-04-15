{-# LANGUAGE Safe #-}

module Scheduler ( Scheduler(schedule)
                 , SchedulerIO
                 , getCurrentScheduler
                 , BackgroundScheduler
                 , newScheduler
                 ) where

import Control.Applicative
import Control.Concurrent.STM
import Scheduler.Internal

-- | Creates a new background scheduler.
newScheduler :: IO BackgroundScheduler
newScheduler = BackgroundScheduler <$> atomically newTQueue
