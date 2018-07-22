-- | Calculates time spent on a task given a log over intervals where
-- you've been working.
--
-- The log must be in a @.yaml@ file following this format:
--
--     # Use `C-u M-! date -Iseconds` to insert the time
--     - from: 2018-07-22T11:00:00+02:00
--       to:   2018-07-22T16:24:16+02:00
--     # 5.5 hrs
{-# language DeriveGeneric
  , NoImplicitPrelude
  , TypeApplications
  , GeneralizedNewtypeDeriving
  , FlexibleContexts
  , TypeFamilies
  , CPP #-}
module Main where

import ClassyPrelude hiding (log)
import Data.Time
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, defaultOptions)
import Data.Yaml (decodeThrow)
import Data.Map (Map)
import qualified Data.Map as M
import Data.MonoTraversable ()
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

-- | Parse a log and write out total time.
main :: IO ()
main = do
  path <- execParser logPathInfo
  log <- getLog path
  printLog log

printEntry :: Entry -> IO ()
printEntry (Entry a b) = do
  printf "Worked from %s to %s\n" (frmt a) (frmt b)
  where
    frmt = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))

printLog :: Log -> IO ()
printLog log = do
  omapM_ printEntry log
  frmTime . getTotalTime $ log

-- | Human readable output format.
frmTime :: FormatTime t => PrintfType r => t -> r
frmTime s = printf "Time spent: %s\n" (formatTimeHMS s)

-- | Get hours, minutes and seconds.
formatTimeHMS :: FormatTime t => t -> String
formatTimeHMS = formatTime defaultTimeLocale "%H:%M:%S"

-- | Calculate total time from log.
getTotalTime :: Log -> NominalDiffTime
getTotalTime = ofoldl' step 0
  where
  step :: NominalDiffTime -> Entry -> NominalDiffTime
  step t (Entry s e) = t + (e `diffUTCTime` s)

-- | Read and decode log.
getLog :: FilePath -> IO Log
getLog = readFile >=> decodeThrow @IO @Log


-- * Command line options

-- | Default path to log file.
defaultLogPath :: FilePath
defaultLogPath = "./worklog.yaml"

-- | Option parser for log file.
logPathP :: Parser FilePath
logPathP = strArgument @FilePath (metavar "LOG" <> help "Path to the log file" <> value defaultLogPath)

logPathInfo :: ParserInfo FilePath
logPathInfo = info
  (helper <*> logPathP)
  (  fullDesc
  <> progDesc "Keeps Track of your time"
  <> header "time-keeper"
  )
