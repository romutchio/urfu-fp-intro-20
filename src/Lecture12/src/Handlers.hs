{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Servant.Server

import App
import Data.Time
import DB.MovieSession
import DB.Seat (SeatId, Seat, getSeatsBySessionId)
import DB.Preliminary
import DB.Booking
import Utils

getSessions :: MonadIO m => AppT m [MovieSession]
getSessions = getMovieSessions

getSeats :: MonadIO m => MovieSessionId -> AppT m [Seat]
getSeats = getSeatsBySessionId

postPreliminary :: MonadIO m => MovieSessionId -> SeatId -> AppT m BookingId
postPreliminary msId seatId = do
  bookings <- createPreliminary msId seatId
  case bookings of
    (b:_) -> pure $ bookingId b
    _ -> throwJSONError err404 $ JSONError "booking is not found"

getCheckoutBooking :: MonadIO m => BookingId -> AppT m String
getCheckoutBooking bId = do
  bookings <- getBookings bId
  case bookings of
    [] -> return "Checkout failed"
    (booking:_) -> do
      currentTime <- liftIO $ getCurrentTime
      let bookingTime = createdAt booking
      let isValid = (diffUTCTime currentTime bookingTime) >= (fromInteger 600) 
      case (isPreliminary booking) of
        False -> return "Booking was already checked out"
        True -> do
          case isValid of
            True -> do
              _ <- deleteBooking bId
              return "Booking is over, more than 10 minutes passed"
            False -> do
              _ <- deleteBooking bId
              _ <- checkoutBooking (seatId booking)
              return "Booking was checked out"

getRefundBooking :: MonadIO m => BookingId -> AppT m String
getRefundBooking bId = do
  bookings <- getBookings bId
  case bookings of
    [] -> return "Refund failed"
    _ -> do
      _ <- deleteBooking bId
      return "Refund succeeded" 
