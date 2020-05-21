module API.Preliminary where

import Servant.API
import DB.MovieSession
import DB.Seat
import DB.Booking

-- Метод для создания предварительного бронирования
type PreliminaryAPI
  = "api" :> "movie_sessions"
    :> Capture "id" MovieSessionId
    :> "preliminary_booking"
    :> Capture "id" SeatId
    :> Get '[JSON] BookingId
--      ^     ^       ^
--      |     |   Тип возвращаемого значения
--      | список используемых content-type'ов https://hackage.haskell.org/package/servant-0.17/docs/Servant-API-ContentTypes.html
--  http-метод
-- (:>) специальный тип для описания API