{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Day1 where
import           Control.Applicative              (many)
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString                  as B
import           Data.ByteString.Char8
import           Data.Scientific
import           Prelude                          hiding (putStrLn)

day1InputFile :: FilePath
day1InputFile = "../resources/Day1_Part1.txt"

parseSignedInteger :: ABC.Parser [Scientific]
parseSignedInteger = many $ ABC.signed ABC.scientific <* ABC.endOfLine

readInput :: IO (Either String Scientific)
readInput = do
  contents <- B.readFile day1InputFile
  pure $ case ABC.parseOnly parseSignedInteger contents of
           Right xs -> Right $ sum xs
           Left _   -> Left "Parsing Failed"

