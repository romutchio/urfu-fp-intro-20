{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture11.PersonsT where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (id)
import Lecture10.Reader (Person (..), Sex(..), PersonId, persons, processSingle, processPair)


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
  { runPersonsT :: (ReaderT [Person] (StateT PersonSearchStats (Writer [String])) a)}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState PersonSearchStats
    , MonadReader [Person]
    , MonadWriter [String]
    )

runPersons :: PersonsT a -> ((a, PersonSearchStats), [String])
runPersons p = runWriter . flip runStateT emptyStats . flip runReaderT persons . runPersonsT $ p

findById :: PersonId -> PersonsT (Maybe Person)
findById pId = do
  _persons <- ask
  let found = find (\p -> id p == pId) _persons
  case found of
    Just (Person id _ _ _ _ _) -> tell ["Found: " ++ show id]
    _ -> tell ["Not found: " ++ show pId]
  return $ found

processPerson :: PersonId -> PersonsT (Maybe String)
processPerson pId = do
    put emptyStats
    f <- findById pId
    s <- case f of
      Just (Person _ _ _ _ _ (Just id)) -> findById id
      _ -> return $ Nothing
    case (f,s) of
      (Just h@(Person _ _ _ _ Male _), Just w@(Person _ _ _ _ Female _)) -> do 
        _ <- modify married
        return $ Just (processPair h w)
      (Just w@(Person _ _ _ _ Female _), Just h@(Person _ _ _ _ Male _)) -> do
        _ <- modify married
        return $ Just (processPair h w)
      (Just p, _) -> do
        _ <- modify single
        return $ Just (processSingle p)
      _ -> return Nothing
      where
        single = \pss@(PersonSearchStats _ spc) -> pss {singlePersonsCount=spc+1}
        married = \pss@(PersonSearchStats mpc _) -> pss {marriedPersonsCount=mpc+1}

{-
  Функция должна выводить на экран:
  - общую поисковую статистику по всем найденым персонам.
  - результат каждого поиска

  Записывать в "persons.log" общий лог всех поисков.
-}
processPersons :: [PersonId] -> IO ()
processPersons personIds = do
  let ((results, stats), logs) = runPersons $ mapM (\p -> do processed <- processPerson p; return $ (p, processed)) personIds
  mapM_ (\(i, res) -> putStrLn ("Found: " ++ show res ++ "; id: " ++ show i)) results
  putStrLn ("Total: \n" ++ show stats)
  writeFile "persons.log" (show logs)

-- </Задачи для самостоятельного решения>
