module Aftok.Snaplet.WorkLog where

import           ClassyPrelude

import           Control.Lens
import           Control.Monad.Trans.Maybe (mapMaybeT)

import qualified Data.Aeson         as A
import           Data.Aeson.Types
import           Data.Thyme.Clock   as C
import           Data.UUID          as U

import           Aftok              (parseBtcAddr)
import           Aftok.Database
import           Aftok.Interval
import           Aftok.Json
import           Aftok.Project
import           Aftok.TimeLog
import           Aftok.Util (fromMaybeT)

import           Aftok.Snaplet
import           Aftok.Snaplet.Auth
import           Aftok.Snaplet.Util

import           Snap.Core
import           Snap.Snaplet       as S

logWorkHandler :: (C.UTCTime -> LogEvent) -> S.Handler App App EventId
logWorkHandler evCtr = do
  uid <- requireUserId
  pid <- requireProjectId
  requestBody <- readRequestBody 4096
  timestamp <- liftIO C.getCurrentTime
  case A.eitherDecode requestBody >>= parseEither (parseLogEntry uid evCtr) of
    Left err -> snapError 400 $ "Unable to parse log entry " <> (tshow requestBody) <> ": " <> tshow err
    Right entry -> snapEval $ createEvent pid uid (entry timestamp)

logWorkBTCHandler :: (C.UTCTime -> LogEvent) -> S.Handler App App EventId
logWorkBTCHandler evCtr = do
  uid <- requireUserId
  pid <- requireProjectId
  addrBytes <- getParam "btcAddr"
  requestBody <- readRequestBody 4096
  timestamp <- liftIO C.getCurrentTime
  case fmap decodeUtf8 addrBytes >>= parseBtcAddr of
    Nothing ->
      snapError 400 $ "Unable to parse bitcoin address from " <> (tshow addrBytes)
    Just addr ->
      snapEval . createEvent pid uid $
        LogEntry (CreditToAddress addr) (evCtr timestamp) (A.decode requestBody)

loggedIntervalsHandler :: S.Handler App App WorkIndex
loggedIntervalsHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  snapEval $ readWorkIndex pid uid

logEntriesHandler :: S.Handler App App [LogEntry]
logEntriesHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  endpoints <- (,) <$> timeParam "after" <*> timeParam "before"
  ival <- case endpoints of
    (Just s,  Just e)  -> pure $ During s e
    (Nothing, Just e)  -> pure $ Before e
    (Just s,  Nothing) -> pure $ After s
    (Nothing, Nothing) -> snapError 400 "You must at least one of the \"after\" or \"before\" query parameter"
  snapEval $ findEvents pid uid ival

payoutsHandler :: S.Handler App App Payouts
payoutsHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  project <- fromMaybeT
    (snapError 400 $ "Project not found for id " <> tshow pid)
    (mapMaybeT snapEval $ findUserProject uid pid)
  widx <- snapEval $ readWorkIndex pid uid
  ptime <- liftIO $ C.getCurrentTime
  pure $ payouts (toDepF $ project ^. depf) ptime widx

amendEventHandler :: S.Handler App App AmendmentId
amendEventHandler = do
  uid <- requireUserId
  eventIdBytes <- getParam "eventId"
  eventId <- maybe
    (snapError 400 "eventId parameter is required")
    (pure . EventId)
    (eventIdBytes >>= U.fromASCIIBytes)
  modTime <- ModTime <$> liftIO C.getCurrentTime
  requestJSON <- readRequestJSON 4096
  either
    (snapError 400 . pack)
    (snapEval . amendEvent uid eventId)
    (parseEither (parseEventAmendment modTime) requestJSON)
