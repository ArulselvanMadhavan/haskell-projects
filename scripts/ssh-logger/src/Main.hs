{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Applicative
import qualified Control.Exception         as Exception
import           Control.Monad.Error.Class
import           Control.Monad.Except
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString.Lazy      as B
import qualified Data.Either               as E
import           Data.Either.Utils
import           Data.List                 as L
import           Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           GHC.Generics
import           Turtle


data Config = Config {
  branchName::Text
  } deriving(Generic, FromJSON, ToJSON, Show)


jsonFile :: Prelude.FilePath
jsonFile = "config.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

constructHostName :: Text -> Text
constructHostName ipAddress =
  T.concat ["arul.madhavan@", ipAddress]

getHomeDirectory :: IO Text
getHomeDirectory = do
  homeFP <- home
  liftEither $ first (userError . T.unpack) (toText homeFP)

data GithubCredentials = GithubCredentials { userName:: Text, password::Text } deriving (Show)

githubUserName :: Text
githubUserName = "ArulselvanMadhavan"

tokenPath :: Turtle.FilePath
tokenPath = Turtle.fromText $ T.concat[".github/tokens/", githubUserName]

appendTokenPath :: Turtle.FilePath -> Turtle.FilePath
appendTokenPath h = h </> tokenPath

githubTokenPath :: IO (Turtle.FilePath)
githubTokenPath = do
  hd <- getHomeDirectory
  pure $ appendTokenPath (Turtle.fromText hd)

readGithubToken :: IO Text
readGithubToken = do
  tokenPath <- githubTokenPath
  strict $ input tokenPath

buildGithubCredentials :: IO GithubCredentials
buildGithubCredentials = do
  token <- readGithubToken
  let tk = stripEnd token
  pure $ GithubCredentials githubUserName tk

constructCloneUrl :: GithubCredentials -> Text
constructCloneUrl (GithubCredentials un token) =
  T.concat ["https://", un, ":", token, "@github.com/chartBoost/adserver.git"]

constructCloneCommand :: GithubCredentials -> Text
constructCloneCommand gc = T.concat ["git ", "clone ", constructCloneUrl gc]

main :: IO ()
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String Config)
  Prelude.putStrLn $ show d
  TIO.putStrLn "Enter the ip address to connect:"
  ipaddress <- TIO.getLine
  homeDir <- getHomeDirectory
  TIO.putStrLn $ T.concat ["Connecting to " , ipaddress]
  TIO.putStrLn $ T.concat ["Copying bash script to ", ipaddress]
  gc <- buildGithubCredentials
  stdout $ inproc "ssh" ["-oStrictHostKeyChecking=no", ipaddress, constructCloneCommand gc] Turtle.empty
  stdout $ inproc "ssh" ["-oStrictHostKeyChecking=no", ipaddress, "cd ~/adserver && sbt exit"] Turtle.empty
  stdout $ inproc "ssh" ["-oStrictHostKeyChecking=no", ipaddress, "cd ~/adserver && sbt \";project adserver-exchange; debian:packageBin\""] Turtle.empty
  TIO.putStrLn "All set!!!"




