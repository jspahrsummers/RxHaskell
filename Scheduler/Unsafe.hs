{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Unsafe #-}

module Scheduler.Unsafe (
                        ) where

import Scheduler (newScheduler)
import Scheduler.Internal
import Scheduler.Main
import System.IO.Unsafe (unsafePerformIO)

-- | Extracts the underlying 'IO' action from a 'SchedulerIO' action.
--
--   This can be unsafe because the type system does not have enough information to determine
--   whether the calling code is running on an appropriate scheduler.
unsafeRunSchedulerIO :: Scheduler s => s -> SchedulerIO s a -> IO a
unsafeRunSchedulerIO sch (SchedulerIO mf) = mf sch

-- | Unsafely executes a 'SchedulerIO' action and shows the result.
--   This is for /DEBUGGING PURPOSES ONLY/.
instance Show v => Show (SchedulerIO MainScheduler v) where
    show action = show $ unsafePerformIO $ do
        sch <- getMainScheduler
        unsafeRunSchedulerIO sch action

instance Show v => Show (SchedulerIO BackgroundScheduler v) where
    show action = show $ unsafePerformIO $ do
        sch <- newScheduler
        unsafeRunSchedulerIO sch action
