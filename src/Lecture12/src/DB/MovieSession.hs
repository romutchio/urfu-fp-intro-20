{-# LANGUAGE DeriveAnyClass #-}

module DB.MovieSession where

import Data.Aeson
import Data.Text
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics

import DB.Internal

newtype MovieSessionId = MovieSessionId
  { unMovieSessionId :: Integer }
  deriving (Eq, Show)
  deriving ToRow via (Only Integer)
  deriving (FromHttpApiData, FromField, ToField, FromJSON, ToJSON)
    via Integer

data MovieSession = MovieSession
  { sessionId :: MovieSessionId
  , title :: Text
  , start_time :: UTCTime
  , duration :: Integer
  } deriving (Eq, Show, Generic)

deriving instance FromRow MovieSession
deriving instance ToRow MovieSession

instance ToJSON MovieSession
instance FromJSON MovieSession

{-
  Метод для получения сессий из базы данных.
-}
getMovieSessions
  :: DBMonad m
  => m [MovieSession]
getMovieSessions = runSQL $ \conn ->
  query_ conn "SELECT id, title, start_time, duration from movie_sessions"
