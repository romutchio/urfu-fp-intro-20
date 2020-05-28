{-# LANGUAGE DuplicateRecordFields #-}

module Lecture09 where

-- Решение Изакова Артемия и Белянина Андрея

import System.Directory
import System.IO
import System.FilePath
import Data.List
import System.Random

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

newtype Id = Id String deriving (Eq, Show, Read)

newtype Title = Title String deriving (Eq, Show, Read)

newtype Deadline = Deadline String deriving (Eq, Show, Read, Ord)

newtype Content = Content String deriving (Eq, Show, Read)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show, Read)

instance Ord Todo where
  compare (Todo _ _ _ deadline1 _) (Todo _ _ _ deadline2 _) = compare deadline1 deadline2

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  createDirectory rootFolder
  return $ TodoList rootFolder

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo (TodoList rootFolder) title content deadline = do
  (fileName, file) <- openTempFile rootFolder "" 
  let
    todoId = Id $ takeFileName fileName
  hPutStr file $ show $ Todo todoId title content deadline False
  hClose file
  return todoId

getTodoFilePath :: TodoList -> Id -> FilePath
getTodoFilePath (TodoList rootFolder) (Id fileName) = joinPath [rootFolder, fileName]

readTodoStr :: TodoList -> Id -> IO String
readTodoStr todoList todoId = readFile $ getTodoFilePath todoList todoId

readTodo :: TodoList -> Id -> IO Todo
readTodo todoList todoId = do
  todo <- readTodoStr todoList todoId
  return $ read todo

showTodo :: TodoList -> Id -> IO ()
showTodo todoList todoId = do
  todo <- readTodoStr todoList todoId
  putStrLn todo

removeTodo :: TodoList -> Id -> IO ()
removeTodo todoList todoId = removeFile $ getTodoFilePath todoList todoId

replaceTodo :: TodoList -> Id -> Todo -> IO ()
replaceTodo todoList@(TodoList rootFolder) todoId todo = do
  (tempName, tempFile) <- openTempFile rootFolder "temp"
  hPutStr tempFile $ show todo
  hClose tempFile 
  let
    filePath = getTodoFilePath todoList todoId
  removeFile filePath
  renameFile tempName filePath

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList todoId (TodoEdit title content deadline) = do
  todo <- readTodo todoList todoId
  replaceTodo todoList todoId todo {title = title, content = content, deadline = deadline}

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList todoId = do
  todo <- readTodo todoList todoId
  replaceTodo todoList todoId todo {isDone = True}

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList@(TodoList rootFolder) = do
  files <- listDirectory rootFolder
  todo <- mapM (readTodo todoList . Id) files
  return $ sort todo

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  allTodo <- readAllTodo todoList
  return $ filter isUnfinished allTodo
  where
    isUnfinished (Todo _ _ _ _ isDone) = not isDone

showTodos :: [Todo] -> IO ()
showTodos = mapM_ $ putStrLn . show

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = readAllTodo todoList >>= showTodos

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = readUnfinishedTodo todoList >>= showTodos

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

loop :: Integer -> IO ()
loop secret = do
  putStr "Your number: "
  guess <- getLine
  case (compare (read guess) secret) of
    LT -> putStrLn "Too small" >> loop secret
    GT -> putStrLn "Too big" >> loop secret
    EQ -> putStrLn "Yep, that's the number!"

playGuessGame :: IO ()
playGuessGame = do
  putStrLn "Gues a number!"
  secret <- randomRIO (0, 100) :: IO Integer
  loop secret

-- </Задачи для самостоятельного решения>