{-# LANGUAGE Safe #-}

module Subscriber ( Subscriber
                  , subscriber
                  , sendNext
                  , sendError
                  , sendCompleted
                  ) where

import Control.Exception
import Data.IORef

-- | A function to run when a signal sends a value.
type OnNext a = a -> IO ()

-- | A function to run when an exception is thrown from within a signal.
type OnError = SomeException -> IO ()

-- | A function to run when a signal completes successfully.
type OnCompleted = IO ()

-- | Receives events from a signal.
data Subscriber a = Subscriber {
    onNext :: OnNext a,
    onError :: OnError,
    onCompleted :: OnCompleted,
    disposed :: IORef Bool
}

-- | Constructs a subscriber.
subscriber :: OnNext a -> OnError -> OnCompleted -> IO (Subscriber a)
subscriber next error completed = do
    d <- newIORef False

    return $ Subscriber { onNext = next
                        , onError = error
                        , onCompleted = completed
                        , disposed = d
                        }

-- | Sends a value to a subscriber.
sendNext :: Subscriber a -> a -> IO ()
sendNext s x = do
    d <- readIORef $ disposed s
    if d then return () else onNext s x

-- | Sends an error to a subscriber.
sendError :: Exception e => Subscriber a -> e -> IO ()
sendError s e = do
    d <- atomicModifyIORef (disposed s) $ \d -> (True, d)
    if d then return () else onError s $ toException e

-- | Sends @completed@ to a subscriber.
sendCompleted :: Subscriber a -> IO ()
sendCompleted s = do
    d <- atomicModifyIORef (disposed s) $ \d -> (True, d)
    if d then return () else onCompleted s
