module Chapter1.Ex1_4_i where

to :: (b -> a) -> (c -> a) -> Either b c -> a
to ba _ (Left b)  = ba b
to _ ca (Right c) = ca c

from :: (Either b c) -> (b -> a) -> (c -> a) -> a
from (Left b) ba _  = ba b
from (Right c) _ ca = ca c
