module TestIO where

import Control.Concurrent
import Control.Monad
import IO
import Signal
import System.IO

main = do
    s <- hLineSignal stdin
    s >>: print
    wait

wait = threadDelay 1000 >> wait
