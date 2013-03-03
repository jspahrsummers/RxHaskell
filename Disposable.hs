{-# LANGUAGE Safe #-}

module Disposable ( Disposable
                  , dispose
                  , newDisposable
                  , newCompositeDisposable
                  , empty
                  , addDisposable
                  ) where

import Control.Applicative ((<$), (<$>))
import Control.Concurrent.MVar
import Data.IORef

-- | A list of disposables, along with a flag indicating whether disposal already happened.
type DisposableList = (Bool, [Disposable])

-- | Represents an operation which can be canceled or a resource which can be freed.
data Disposable = ActionDisposable (IO ()) (IORef Bool)
                | CompositeDisposable (MVar DisposableList)
                | EmptyDisposable

-- | Disposes a disposable. Returns whether it was already disposed.
dispose :: Disposable -> IO Bool
dispose EmptyDisposable = return True
dispose (ActionDisposable action d) = do
    b <- atomicModifyIORef d $ \b -> (True, b)
    if b
        then b <$ action
        else return b

dispose (CompositeDisposable dl) = 
    let modify (True, _) = return ((True, []), True)
        modify (False, xs) = do
            mapM_ dispose xs
            return ((True, []), False)
    in modifyMVar dl modify

-- | Creates a disposable which runs the given action upon disposal.
newDisposable :: IO () -> IO Disposable
newDisposable action = ActionDisposable action <$> newIORef False

-- | Creates a disposable which can dispose of other disposables.
newCompositeDisposable :: IO Disposable
newCompositeDisposable = CompositeDisposable <$> newMVar (False, [])

-- | Adds disposable @d@ to the composite disposable.
addDisposable :: Disposable -> Disposable -> IO ()
addDisposable (CompositeDisposable dl) d =
    let modify (True, _) = (True, []) <$ dispose d
        modify (False, xs) = return (False, d : xs)
    in modifyMVar_ dl modify

-- | Returns a disposable which does no work.
empty :: Disposable
empty = EmptyDisposable
