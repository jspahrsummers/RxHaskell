{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification #-}

module Event ( Event(..)
             ) where

import Control.Exception

-- | Represents an event that a signal might send.
data Event a =
    NextEvent a |
    ErrorEvent IOException |
    CompletedEvent
    deriving (Eq, Show)
