{-# LANGUAGE DeriveAnyClass #-}

module DB.Preliminary where

import Database.SQLite.Simple

import DB.Booking
import DB.MovieSession
import DB.Internal
import DB.Seat


{-
  Preliminary запрос должен создать предварительное бронирование, если
  заданное место на сеанс не занято. Если бронирование уже существует,
  необходимо вернуть сообщение об ошибке в JSON формате.
-}
createPreliminary
  :: DBMonad m
  => MovieSessionId
  -> SeatId
  -> m [Booking]
createPreliminary msId seatId = runSQL $ \conn -> do
  execute conn "INSERT INTO bookings (seat_id, movie_session_id, is_preliminary) values (?, ?, true)" (seatId, msId)
  query conn ("SELECT id, seat_id, movie_session_id, is_preliminary, created_at " <>
    "from bookings where movie_session_id = ? and seat_id = ?") (msId, seatId)
