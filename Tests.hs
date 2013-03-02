module Tests where

import Control.Monad
import Data.Monoid
import Signal

s = signal $ \sub -> do
    sub $ Just "hello"
    sub Nothing

s' = signal $ \sub -> do
    sub $ Just "world"
    sub Nothing

sub = putStrLn . show

testBinding =
    let ss = signal $ \sub -> do
                sub $ Just s
                sub $ Just s'
                sub Nothing
    in subscribe (join ss) sub

testSequencing = do
    subscribe (s >> s') sub
    subscribe (s' >> s) sub

testAppending = do
    subscribe (s `mappend` mempty) sub
    subscribe (s `mappend` s') sub
    subscribe (s' `mappend` s) sub
