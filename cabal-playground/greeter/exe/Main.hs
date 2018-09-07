module Main where

import Greeter
import System.Environment

main :: IO ()
main = mapM_ (putStrLn . greet) =<< getArgs
