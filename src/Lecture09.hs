{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Lecture09 where

import GHC.Generics
import Data.Aeson
import System.Directory
import Data.List
import System.Random
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.UUID


{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show, Generic)

instance ToJSON Id
instance FromJSON Id

newtype Title = Title String deriving (Eq, Show, Generic)

instance ToJSON Title
instance FromJSON Title

newtype Deadline = Deadline String deriving (Eq, Show, Generic)

instance ToJSON Deadline
instance FromJSON Deadline

newtype Content = Content String deriving (Eq, Show, Generic)

instance ToJSON Content
instance FromJSON Content

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show, Generic)

instance Ord Todo where
  compare (Todo _ _ _ (Deadline d1) _) (Todo _ _ _ (Deadline d2) _) = compare d1 d2

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

instance ToJSON Todo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Todo

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
                      _ <- createDirectory rootFolder
                      return $ TodoList rootFolder

generateUUID :: IO UUID
generateUUID = do
          g <- newStdGen
          let (u1, _) = random g
          return $ u1

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo (TodoList path) title text deadline = do
                          id <- generateUUID
                          let strId = toString id
                          let filename = toString id ++ ".txt"
                          let fullPath = path ++ "/" ++ filename
                          let todo = Todo {todoId = Id strId, title=title, content=text, deadline=deadline, isDone=False}
                          _ <- BS.writeFile fullPath $ encode todo
                          return $ Id strId

readTodo :: TodoList -> Id -> IO Todo
readTodo (TodoList path) (Id id) = do
              files <- getDirectoryContents path
              let (file:_) = filter (isPrefixOf id) files
              let fullPath = path ++"/"++ file
              todo <- filenameToTodo fullPath
              return $ todo

showTodo :: TodoList -> Id -> IO ()
showTodo todoList id = do
            todo <- readTodo todoList id
            C8.putStrLn $ encode todo


removeTodo :: TodoList -> Id -> IO ()
removeTodo (TodoList path) (Id id) = do
              files <- listDirectory path
              let (file:_) = filter (isPrefixOf id) files
              removeFile $ path ++ "/" ++ file

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo (TodoList path) (Id id) (TodoEdit title content deadline) = do
              files <- listDirectory path
              let (file:_) = filter (isPrefixOf id) files
              let fullPath = path ++"/"++ file
              todo <- filenameToTodo fullPath
              let updated = Todo {todoId = Id id, title=title, content=content, deadline=deadline, isDone=isDone todo}
              BS.writeFile fullPath $ encode updated

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone (TodoList path) (Id id) = do
              files <- listDirectory path
              let (file:_) = filter (isPrefixOf id) files
              let fullPath = path ++"/"++ file
              Todo (todoId) (title) (content) (deadline) (_) <- filenameToTodo fullPath
              let updated = Todo {todoId=todoId, title=title, content=content, deadline=deadline, isDone=True}
              BS.writeFile fullPath $ encode updated

filenameToTodo :: FilePath -> IO Todo
filenameToTodo file = do 
  content <- BS.readFile file
  case decode content of
    Just a -> return $ a
    Nothing -> error $ "not parsed" ++ file;

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo (TodoList path) = do
              files <- listDirectory path
              let fullPaths = map (\file -> path ++ "/" ++ file) files

              contents <- mapM (\x -> filenameToTodo x) fullPaths
              return $ sort contents

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
            todos <- readAllTodo todoList
            let unfinished = filter (\todo -> isDone todo == False) todos
            return $ unfinished

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = do
            todos <- readAllTodo todoList
            mapM_ (\todo -> C8.putStrLn $ encode todo) todos

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = do
            todos <- readUnfinishedTodo todoList
            mapM_ (\todo -> C8.putStrLn $ encode todo) todos

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}

playGuessGame :: IO ()
playGuessGame = do
  putStrLn "Шалом!"
  putStrLn "Отгадай число от 0 and 100."
  secret <- randomRIO (0, 100)
  guessNumber secret

guessNumber :: Integer -> IO ()
guessNumber secret = go 1 
 where
  go tries = do
     guess <- readLn
     putStrLn ("Your number: " ++ show guess)
     case compare guess secret of
        LT -> do
          putStrLn "Too small"
          go (tries + 1)
        GT -> do 
          putStrLn "Too big"
          go (tries + 1)
        EQ -> do 
          putStrLn "Yep, that's the number!" 
          go tries

-- </Задачи для самостоятельного решения>