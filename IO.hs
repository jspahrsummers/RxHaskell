{-# LANGUAGE Safe #-}

module IO ( hLineSignal
          ) where

import Control.Monad
import Signal
import Signal.Scheduled
import Subscriber
import System.IO

-- | Creates a signal which reads lines from the given handle.
hLineSignal :: Handle -> IO (Signal String)
hLineSignal h = do
    s <- newScheduler
    start s $ \sub ->
        let readLoop :: IO ()
            readLoop = do
                b <- hWaitForInput h (-1)
                when b $ hGetLine h >>= send sub . NextEvent
                readLoop
        in readLoop
