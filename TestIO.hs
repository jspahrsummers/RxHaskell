module TestIO where

import Control.Concurrent
import Control.Monad
import Signal
import Signal.IO
import System.IO

main = do
    s <- hLineSignal stdin
    s >>: print
    wait

wait = threadDelay 1000 >> wait
