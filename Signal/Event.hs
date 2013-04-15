{-# LANGUAGE Safe #-}

module Signal.Event ( Event(..)
                    ) where

import Control.Exception

-- | Represents an event that a signal might send.
--
--   Signals may send any number of 'NextEvent's, followed by one 'ErrorEvent' /or/ 'CompletedEvent'.
data Event v = NextEvent v              -- ^ A value @v@ in the monad.
             | ErrorEvent IOException   -- ^ Sent when an error or exception occurs in the signal. Outside of the monad.
             | CompletedEvent           -- ^ Sent when the signal completes successfully. Outside of the monad.

instance Eq v => Eq (Event v) where
    (NextEvent v) == (NextEvent v') = v == v'
    (ErrorEvent e) == (ErrorEvent e') = e == e'
    CompletedEvent == CompletedEvent = True
    _ == _ = False

instance Show v => Show (Event v) where
    show (NextEvent v) = "NextEvent " ++ show v
    show (ErrorEvent e) = "ErrorEvent " ++ show e
    show CompletedEvent = "CompletedEvent"
