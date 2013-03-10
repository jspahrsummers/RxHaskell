{-# LANGUAGE Safe #-}

module Event ( Event(..)
             ) where

import Control.Exception

-- | Represents an event that a signal might send.
data Event v =
    NextEvent v |
    ErrorEvent IOException |
    CompletedEvent

instance Eq v => Eq (Event v) where
    (NextEvent v) == (NextEvent v') = v == v'
    (ErrorEvent e) == (ErrorEvent e') = e == e'
    CompletedEvent == CompletedEvent = True
    _ == _ = False

instance Show v => Show (Event v) where
    show (NextEvent v) = "NextEvent " ++ show v
    show (ErrorEvent e) = "ErrorEvent " ++ show e
    show CompletedEvent = "CompletedEvent"
