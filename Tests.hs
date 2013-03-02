module Tests where

import Signal

s = signal $ \sub -> do
    sub $ Just "hello"
    sub Nothing

s' = signal $ \sub -> do
    sub $ Just "world"
    sub Nothing

sub = putStrLn . show

testSequencing = do
    subscribe (s >> s') sub
    subscribe (s' >> s) sub
