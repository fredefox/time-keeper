{-# language DeriveGeneric
  , NoImplicitPrelude
  , TypeApplications
  , GeneralizedNewtypeDeriving
  , FlexibleContexts
  , TypeFamilies
  , CPP #-}
module Main where

import ClassyPrelude
import GHC.Generics
import Data.Time
import Data.Time.Format
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, defaultOptions)
import Data.Yaml (decodeThrow)
import System.FilePath
import Data.Text.Prettyprint.Doc
import Data.Map (Map)
import qualified Data.Map as M
import Data.MonoTraversable
import Text.Printf
import Options.Applicative

data Entry = Entry { from :: UTCTime , to :: UTCTime } deriving (Show, Generic)

instance ToJSON Entry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Entry where

newtype Log = Log [Entry] deriving (Show, Generic, MonoFoldable)

instance ToJSON Log where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Log where

type instance Element Log = Entry

-- TODO
type Week = ()

data Summary = Summary
  -- Time per week.
  { timePerWeek :: Map Week NominalDiffTime
  , totalTime   :: NominalDiffTime
  }

main :: IO ()
main = do
  path <- execParser fileI
  log <- getLog path
  frmTime . getTotalTime $ log

frmTime :: FormatTime t => PrintfType r => t -> r
frmTime s = printf "Time spent: %s\n" (formatTimeHMS s)

formatTimeHMS :: FormatTime t => t -> String
formatTimeHMS = formatTime defaultTimeLocale "%H:%M:%S"

getTotalTime :: Log -> NominalDiffTime
getTotalTime = ofoldl' step 0
  where
  step :: NominalDiffTime -> Entry -> NominalDiffTime
  step t (Entry s e) = t + (e `diffUTCTime` s)

getLog :: FilePath -> IO Log
getLog = readFile >=> decodeThrow @IO @Log

file :: FilePath
file = "./worklog.yaml"

fileP :: Parser FilePath
fileP = strArgument @FilePath (metavar "LOG" <> help "Path to the log file" <> value file)

fileI :: ParserInfo FilePath
fileI = info
  (helper <*> fileP)
  (  fullDesc
  <> progDesc "Keeps Track of your time"
  <> header "time-keeper"
  )
