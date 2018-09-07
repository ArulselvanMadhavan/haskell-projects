module Main where
import           Product
import           Quicksort


main :: IO ()
main = do
  putStrLn "Sample solution for Q4:"
  putStrLn (show $ product' [3,4,5])
  putStrLn "Sample solution for Q5"
  putStrLn (show $ qsort [4,3,7,8,1])
  putStrLn "Sample solution for Q6"
  putStrLn (show $ qsortR [4,3,7,8,1])
