{-# LANGUAGE Safe #-}

module Scheduler ( Scheduler(schedule)
                 , ImmediateScheduler
                 , BackgroundScheduler
                 , MainScheduler
                 , newScheduler
                 ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import Data.Maybe
import Disposable
import Scheduler.Internal

-- | Creates a new background scheduler.
newScheduler :: IO BackgroundScheduler
newScheduler = BackgroundScheduler <$> atomically newTQueue
