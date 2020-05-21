module App where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Catch
import Servant

-- Тип обертка для пути к базе данных
newtype DatabasePath = DatabasePath { unDatabasePath :: String } deriving Show

-- Конфигурация приложения
data Config = Config
  { dbPath :: DatabasePath
  }

{-
 Трансформер приложения, который состоит из:
  - ExceptT ServerError m — что позволяет нам бросать серверные ошибки в нашем коде
  - ReaderT Config ... — что предоставляет нам доступ к настройкам приложения
-}
newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServerError m) a
  } deriving
    ( Functor, Applicative, Monad
    , MonadReader Config
    -- ^ мы выводим instance, чтобы получить метод ask, для удобного доступа к конфигурации
    , MonadIO
    -- ^ в нашем приложении мы хотим запускать IO методы с помощью `liftIO`, чтобы не думать
    -- о количестве трансформеров в стэке
    , MonadError ServerError
    -- ^ этот класс позволяет нам сообщать об ошибках
    , MonadThrow
    , MonadCatch
    -- ^ эти два класса нужны для обработки и сообщение об ошибках (нужны для функции `convertApp` в `Server`)
    )
