{-# LANGUAGE ViewPatterns #-}
module Chapter2.DataTypes where

data Person =
  Person String
         String
  deriving (Show)

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
