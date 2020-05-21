module API.Session where

import Servant.API
import DB.MovieSession
import DB.Seat

type MovieSessionsAPI
  = "api" :> "movie_sessions" :> Get '[JSON] [MovieSession]
    -- ^ метод для получения доступных сеансов
  :<|>
    ("api" :> "movie_sessions" :> Capture "id" MovieSessionId :> "seats" :> Get '[JSON] [Seat])
    -- ^ метод для получения мест для конкретного сеанса с указанным id