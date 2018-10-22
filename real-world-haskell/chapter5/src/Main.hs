module Main where
import           SimpleJSON

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print (JObject [("foo", (JNumber 1)), ("bar", (JBool False))])
