{-# LANGUAGE Unsafe #-}

module Scheduler.Unsafe (
                        ) where

import Scheduler.Internal
import System.IO.Unsafe (unsafePerformIO)

instance (Scheduler s, Show v) => Show (SchedulerIO s v) where
    show action = show $ unsafePerformIO $ unsafeRunSchedulerIO action
