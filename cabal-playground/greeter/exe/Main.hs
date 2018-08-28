module Main where

import System.Environment
import Greeter

main :: IO ()
main = mapM_ (putStrLn . greet) =<< getArgs
