module Pearl1.SmallestMissingNumberUsingArray where
import           Data.Array
import           Data.Array.ST

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

searchForZero :: Array Int Int -> Int
searchForZero = length . takeWhile (/= 0) . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0,n)
               (zip (filter (<= n) xs) (repeat True))
               where n = length xs

countList :: [Int] -> Array Int Int
countList xs = accumArray (+) 0 (0, n)
               (zip xs (repeat 1))
               where n = length xs

sort :: [Int] -> [Int]
sort xs = concat [replicate k x | (x, k) <- assocs $ countList xs]

{--
minfree extracts the minimum natural number from a list of integers.
--}
minfree :: [Int] -> Int
minfree = search . checklist

minfreeWithDuplicates :: [Int] -> Int
minfreeWithDuplicates = searchForZero . countList

checkListWithMutableArray :: [Int] -> Array Int Bool
checkListWithMutableArray xs =
  runSTArray
    (do a <- newArray (0, n) False
        sequence_ [writeArray a x True | x <- xs, x <= n]
        return a)
  where
    n = length xs

