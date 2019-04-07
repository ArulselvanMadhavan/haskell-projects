{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString.Builder (hPutBuilder)
import qualified Data.Conduit.List       as CL
import           Data.Monoid
import           Network.AWS.Data
import           Network.AWS.EC2
import           System.IO
import Conduit
import qualified Data.Text as T (Text, unpack, concat, pack)
import qualified Data.Text.IO as TIO (putStrLn)
import Data.Maybe (fromJust)
import qualified Turtle
import Path
import Options.Applicative

data Config = Config { serverGroup :: T.Text
                     , sourceFile :: T.Text
                     , destLocation   :: T.Text
                     } deriving (Show)

configParser :: Parser Config
configParser =
  Config
    <$> strOption
          (  long "serverGroup"
          <> short 'g'
          <> metavar "SERVER GROUP"
          <> help
               "Provide Server Group with the version number ex: shiba-arul-v0001"
          )
    <*> strOption
          (long "sourceFile" <> short 's' <> metavar "SOURCEFILE" <> help
            "Provide ABSOLUTE PATH to the Source File that should be copied."
          )
    <*> strOption
          (  long "destinationLocation"
          <> short 'd'
          <> metavar "DESTINATION LOCATION"
          <> help
               "Provide Absolue path in the Remote Destination where this file should be placed. ex: /etc/adserver-shiba/"
          )

instanceFilters :: Config -> DescribeInstances -> DescribeInstances
instanceFilters c = set
  diiFilters
  [filter' "tag:aws:autoscaling:groupName" & fValues .~ [serverGroup c]]

  -- Builds String to pretty print based on the Instance info
pp x = mconcat
  [ "[instance:" <> build (x ^. insInstanceId) <> "] {"
  , "\n  public-dns = " <> build (x ^. insPublicDNSName)
  , "\n  tags       = " <> build (x ^. insTags . to show)
  , "\n  state      = " <> build (x ^. insState . isName . to toBS)
  , "\n  private-ip = " <> build (x ^. insPrivateIPAddress)
  , "\n}\n"
  ]

getPrivateIpAddress :: Instance -> T.Text
getPrivateIpAddress x = fromJust $ (x ^. insPrivateIPAddress)

printIpAddress :: Instance -> IO ()
printIpAddress i = do
  let ipAddress = getPrivateIpAddress i
  putStrLn (T.unpack ipAddress)

-- Sink is a deprecated alias for Sink i m r = ConduitT i Void m r
printSink :: ConduitT Instance Void (AWST' Env (ResourceT IO)) ()
printSink = CL.mapM_ $ (liftIO . hPutBuilder stdout . pp)


remoteTempLocation :: T.Text
remoteTempLocation = ":~/"

buildMoveCommand :: T.Text -> T.Text -> T.Text
buildMoveCommand srcFile destFile =
  T.concat ["sudo", " ", "mv", " ", srcFile, " ", destFile]

doSomethingWithInstance :: Config -> Instance -> IO ()
doSomethingWithInstance c inst = do
  let ipAddress = getPrivateIpAddress inst
  fileToCopy         <- parseAbsFile (T.unpack . Main.sourceFile $ c)
  remoteDestPath <- parseAbsDir . T.unpack . Main.destLocation $ c
  let srcFileLocation = T.pack . toFilePath $ fileToCopy
  let remoteDestLocation = T.pack . toFilePath $ remoteDestPath
  TIO.putStrLn $ T.concat ["Copying " , srcFileLocation , " to ", remoteDestLocation]
  scpCommand (T.pack . fromAbsFile $ fileToCopy) remoteTempLocation ipAddress
  sshCommand ipAddress $ buildMoveCommand (T.pack . toFilePath . filename $ fileToCopy) remoteDestLocation

remoteCopySink :: Config -> ConduitT Instance Void (AWST' Env (ResourceT IO)) ()
remoteCopySink c = CL.mapM_ $ liftIO . doSomethingWithInstance c

constructHostName :: T.Text -> T.Text
constructHostName ipAddress = T.concat ["arul.madhavan", "@", ipAddress]

sshCommand :: T.Text -> T.Text -> IO ()
sshCommand ipAddress cmd = do
  Turtle.stdout $ Turtle.inproc "ssh"
                                ["-oStrictHostKeyChecking=no", ipAddress, cmd]
                                Turtle.empty

scpCommand :: T.Text -> T.Text -> T.Text -> IO ()
scpCommand sourceFP destFP ipAddress = do
  Turtle.stdout $ Turtle.inproc
    "scp"
    [ "-oStrictHostKeyChecking=no"
    , sourceFP
    , T.concat [constructHostName ipAddress, destFP]
    ]
    Turtle.empty

instanceOverview :: Config -> Region -> IO ()
instanceOverview c r = do
  lgr <- newLogger Info stdout
  env <- newEnv Discover <&> set envLogger lgr
  runResourceT
    .  runAWST env
    .  within r
    $  runConduit
    $  paginate (instanceFilters c describeInstances)
    .| CL.concatMap (view dirsReservations) -- Gets Reservations from DescribeInstanceResponse
    .| CL.concatMap (view rInstances) -- Gets Instances from Reservations
    .| getZipSink ((ZipSink printSink) *> (ZipSink $ remoteCopySink c))


opts :: ParserInfo Config
opts = info
  (configParser <**> helper)
  (  fullDesc
  <> progDesc
       "Run copier using command line arguments to copy a file to remote server(s)"
  <> Options.Applicative.header "Copier!!!"
  )

main :: IO ()
main = do
  config <- execParser opts
  -- TODO: Validate the inputs here.
  putStrLn "*** Running copier ***"
  instanceOverview config NorthVirginia
  putStrLn "*** Done ***"
