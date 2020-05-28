{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture11.PersonsT where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (id)
import Lecture10.Reader (Person (..), PersonId, persons, processSingle, processPair)


-- <Задачи для самостоятельного решения>

{-
  В этом задании нужно адаптировать код из 10 лекции к новым требованиям
  при помощи трансформеров.

  1. Необходимо собирать статистику найденных людей:
    - количество одиноких персон
    - количество замужних персон
  2. В функцию `findById` добавить логгирование с помощью монады Write.
    Нужно сообщать была ли найдена персона по данному id или нет.

  Вы можете переиспользовать функции, которые остались без изменения из Lecture10.Reader
-}

data PersonSearchStats = PersonSearchStats
  { marriedPersonsCount :: Integer
  , singlePersonsCount :: Integer
  } deriving (Show)

emptyStats :: PersonSearchStats
emptyStats = PersonSearchStats 0 0

newtype PersonsT a = PersonsT
  { runPersonsT :: StateT PersonSearchStats (WriterT [String] (Reader [Person])) a }
  deriving
    ( Functor
    , Applicative
    , Monad
-- <Удалить перед выкладкой>
    , MonadReader [Person]
    , MonadWriter [String]
    , MonadState PersonSearchStats
-- </Удалить перед выкладкой>
    )
-- { runPersonsT :: NotImplemented }

runPersons :: PersonsT a -> ((a, PersonSearchStats), [String])
runPersons p = runReader (runWriterT (runStateT (runPersonsT p) emptyStats)) persons
-- runPersons p = error "not implemented"

findById :: PersonId -> PersonsT (Maybe Person)
findById pId = do
  tell ["looking for a person:" ++ show pId]
  persons <- ask
  return $ find ((== pId) . id) persons
-- findById pId = error "not implemented"

processPerson :: PersonId -> PersonsT (Maybe String)
processPerson pId = findById pId >>= \case
  Nothing -> pure Nothing
  Just p -> do
    stats <- get
    case marriedBy p of
      Nothing -> do
        put $ stats { singlePersonsCount = singlePersonsCount stats + 1 }
        return $ Just $ processSingle p
      Just mId -> do
        mp' <- findById mId
        case mp' of
          Just p' -> do
            put $ stats { marriedPersonsCount = marriedPersonsCount stats + 2 }
            return $ Just $ processPair p p'
          Nothing -> error "invalid db"
-- processPerson pId = error "not implemented"

{-
  Функция должна выводить на экран:
  - общую поисковую статистику по всем найденым персонам.
  - результат каждого поиска

  Записывать в "persons.log" общий лог всех поисков.
-}
processPersons :: [PersonId] -> IO ()
processPersons personIds = do
  statsWithLogs <- forM personIds $ \personId -> do
    let ((result, stats), log) = runPersons $ processPerson personId
    case result of
      Just r -> putStrLn r
      Nothing -> pure ()
    pure (stats, log)
  pure ()
-- processPersons personIds = error "not implemented"

-- </Задачи для самостоятельного решения>
