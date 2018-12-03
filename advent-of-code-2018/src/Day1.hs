{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Day1 where
import           Control.Applicative              (many)
import           Control.Monad.Trans.State.Strict
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString                  as B
import           Data.ByteString.Char8
import qualified Data.Map.Strict                  as M
import           Data.Scientific
import           Prelude                          hiding (putStrLn)

day1InputFile :: FilePath
day1InputFile = "../resources/Day1_Part1.txt"

parseSignedInteger :: ABC.Parser [Scientific]
parseSignedInteger = many $ ABC.signed ABC.scientific <* ABC.endOfLine

findSolution :: IO (Either String Scientific)
findSolution = do
  contents <- B.readFile day1InputFile
  pure $ case ABC.parseOnly parseSignedInteger contents of
           Right xs -> Right $ sum xs
           Left _   -> Left "Parsing Failed"

findSolutionForPart2 :: IO (Either String Scientific)
findSolutionForPart2 = do
  contents <- B.readFile day1InputFile
  pure $ case ABC.parseOnly parseSignedInteger contents of
           Right xs -> Right $ checkUntilFound xs (0.0 :: Scientific, M.fromList [(0.0, True)])
           Left _   -> Left "Parsing Failed"

type FrequencyMemory = M.Map Scientific Bool
type CurrentFrequency = Scientific
type FrequencyState = (CurrentFrequency, FrequencyMemory)

saveFrequency :: Scientific -> State FrequencyState Bool
saveFrequency inputFreq = do
  (currentFreq, mem) <- get
  let nextFreq = inputFreq + currentFreq
  let lookupResult =  M.member nextFreq mem
  put (nextFreq, (M.insert nextFreq True mem))
  return lookupResult

checkFrequency :: Scientific -> FrequencyState -> (Bool, FrequencyState)
checkFrequency freq = runState (saveFrequency freq)

checkAndHalt :: [Scientific] -> Either FrequencyState Scientific -> Either FrequencyState Scientific
checkAndHalt [] acc = acc
checkAndHalt (x:xs) (Left fs) =
  case checkFrequency x fs of
    (True, (f, _)) -> Right f
    (_, nxt)       -> checkAndHalt xs (Left nxt)
checkAndHalt _ (Right fs) = Right fs

checkUntilFound :: [Scientific] -> FrequencyState -> Scientific
checkUntilFound xs fs =
  case checkAndHalt xs (Left fs) of
    Left fs' -> checkUntilFound xs fs'
    Right f  -> f
