{-# LANGUAGE Unsafe #-}

module Scheduler.Unsafe (
                        ) where

import Scheduler.Internal
import System.IO.Unsafe (unsafePerformIO)

-- | Unsafely executes a 'SchedulerIO' action and shows the result.
--   This is for /DEBUGGING PURPOSES ONLY/.
instance (Scheduler s, Show v) => Show (SchedulerIO s v) where
    show action = show $ unsafePerformIO $ unsafeRunSchedulerIO action
