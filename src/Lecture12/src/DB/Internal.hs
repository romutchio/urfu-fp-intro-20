module DB.Internal where

import Control.Monad.Reader
import Database.SQLite.Simple

import App

{-
  Синоним, чтобы объединить констрейнты по смыслу, необходимые для
  работы с базой данных.
-}
type DBMonad m = (MonadIO m, MonadReader Config m)

{-
  Функция высшего порядка, принимающее действие для работы с базой данных `Connection -> IO a`,
  в теле которой мы достаем путь к базе данных, открываем соединение и передаем его в `action`,
  с помощью функции `withConnection`.

  Эта функция позволяет нам каждый раз не доставать конфигурацию и открывать соединение вручную.
-}
runSQL
  :: DBMonad m
  => (Connection -> IO a)
  -> m a
runSQL action = do
  -- Код ниже эквивалентен тому, что мы вытащили все поля Config'а и положили их
  -- в переменные с такими же именами (ниже используется dbPath как переменная)
  Config{..} <- ask
  liftIO $
    withConnection (unDatabasePath dbPath) action
