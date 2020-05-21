{-# LANGUAGE DeriveAnyClass #-}

module DB.Seat where

import Data.Aeson
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics

import DB.MovieSession
import DB.Internal

newtype SeatId = SeatId
  { unSeatId :: Integer }
  deriving (Eq, Show)
  deriving ToRow via (Only Integer)
  deriving (FromHttpApiData, FromField, ToField, FromJSON, ToJSON)
    via Integer

data Seat = Seat
  { seatId :: SeatId
  , row :: Integer
  , seat :: Integer
  , available :: Bool
  , movieSessionId :: MovieSessionId
  } deriving (Eq, Show, Generic)

deriving instance FromRow Seat
deriving instance ToRow Seat

instance ToJSON Seat
instance FromJSON Seat

getSeatsBySessionId
  :: DBMonad m
  => MovieSessionId
  -> m [Seat]
getSeatsBySessionId msId = runSQL $ \conn -> -- Чтобы передать параметры в запрос используется `?`  ↓
  query conn "SELECT id, row, seat, available, movie_session_id from seats where movie_session_id = ?" msId
