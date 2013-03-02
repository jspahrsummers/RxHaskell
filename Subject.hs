{-# LANGUAGE Safe #-}

module Subject (Subject, subject, send) where

import Control.Concurrent.MVar
import Data.Foldable
import Data.Functor
import Data.Sequence as Seq
import Prelude hiding (sequence_)
import Signal

type SubSeq a = Seq (Subscriber a)
data Subject a = Subject (MVar (SubSeq a))

subject :: IO (Subject a, Signal a)
subject = do
    subj <- newMVar Seq.empty

    let s = signal $ \sub ->
                modifyMVar_ subj $ \subSeq ->
                    return $ subSeq |> sub

    return (Subject subj, s)

send :: Subject a -> Subscriber a
send (Subject subj) =
    let next m = do
            subSeq <- readMVar subj
            sequence_ (fmap (\sub -> sub m) subSeq)
    in next
