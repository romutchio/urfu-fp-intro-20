module Lecture11 where

import Control.Monad
import Data.Char
import Control.Monad.Reader

--  11: Монадные трансформеры

{- Проблема

  Мы успели познакомиться с большим количеством полезных монад: Maybe, IO, State, Reader и т.д.
  При этом до этого момента мы всегда использовали их по отдельности. А ведь может возникнуть вполне
  естественная потребность в использовании двух и больше монад одновременно.

  Рассмотрим такой пример.
  Требуется написать функцию, которая будет спрашивать с пользователя пароль, принимая его только
  если он удовлетворяет некоторым требованием (не корочем, чем 8 символов, есть цифра и т.д.)
-}

-- Проверяет, что пароль подходящий
isValid :: String -> Bool
isValid passphrase = length passphrase >= 8
            && any isAlpha passphrase
            && any isNumber passphrase
            && any isPunctuation passphrase

-- Считывает пароль и проверяет его
getPassphrase :: IO (Maybe String)
getPassphrase = do
  s <- getLine
  return $ if isValid s
           then Just s
           else Nothing

-- Как бы сохраняет пароль в базу
savePassphrase :: String -> IO ()
savePassphrase passphrase = putStrLn "Passphrase is saved"

-- Спросить у пользователя пароль, если с ним всё в порядке, сохраним его в базу
askPassphrase  :: IO (Maybe ())
askPassphrase  = do
  putStr "Your new passphrase: "
  maybePassphrase <- getPassphrase
  case maybePassphrase of
    Just passphrase -> do
      savePassphrase passphrase    -- сходить в базу и сохранить новый пароль
      return $ Just ()
    Nothing -> return Nothing

{-
  Что здесь не так?

  В этом примере мы потеряли возможность использовать монадный синтаксис для Maybe —
  return, do, <- и т.д. относится к монаде IO, а Maybe здесь уже просто значение.
  В этом примере нам бы хотелось достать корректный пароль и сохранить его в базу
  без дополнительных проверок и pattern matching'а.

  Или возьмём другой пример.
  Вернёмся к монадам с прошлой лекции: Reader, Writer и State.
  На самом деле, конечно, если Reader берёт конфигурацию из переменной,
  а Writer пишет лог в строку, то они остаются игрушечными и неочень полезными.
  Хочется писать лог на диск или передавать по сети, то же с конфигурацией.
  А значит нам нужно использовать IO вместе с этими монадами.

  Попробуем сделать это на примере Reader. В нашем примере из Lecture10
  про считывание конфигурации, было бы очень естественно считывать её из файла.
  Для этого нам нужна монада IO и Reader одновременно. Мы можем либо завернуть Reader в IO,
  либо IO в Reader. С первым способом у нас вообще ничего не получится:

    configReaderIO :: IO (Reader String Config)   -- Завернули Reader в IO
    configReaderIO = do
        -- ? --  filename <- ask  -- ? --
        putStrLn ("Read configuration from '" ++ filename ++ "'")
        config <- readFile filename
        return $ Config (read config)

  Проблема возникает, если мы хотим получить окружение с помощью функции ask,
  да и вообще любым другим способом. Чтобы вытащить значение из `ask :: Reader e a`
  нам нужно быть в монаде Reader, но наша функция находится в монаде IO.
  Что делать совершенно непонятно. Можете попробовать реализовать такую функцию
  самостоятельно, чтобы действительно понять, в чём проблема.

  Второй способ всё-таки почти будет работать
-}

data Config = Config
  { port :: Int
  -- ...
  } deriving (Eq, Show)

configReader :: Reader String (IO Config)
configReader = do
    filename <- ask
    void $ return $ putStrLn ("Read configuration from '" ++ filename ++ "'")
    return $ (Config . read) <$> readFile filename

readConfig :: String -> IO Config
readConfig = runReader configReader

{-
  Хотя нам и пришлось явно протаскивать обработку данных из файлы с помощью <$>,
  чтение конфига действительно работает:

    Lecture11> readConfig "src/Lecture11/config"
    Config {port = 8088}

  Однако, написать отладочную информацию в консоль у нас не получилось. Мы никак
  не можем вытащить () из IO, что необходимо для выполнения putStrLn. Всё, что у нас
  получилось, это обернуть IO () в монаду Reader, но дотянуться до () не получается.
  Советуем попробовать разрешить эту проблему. Вдруг получится :)

  Другими словами, нужен механизм, позволящий объединять монады так, чтобы получалось
  не две (и больше) монады вложенные друг в друга `m1 (m2 a)`, а две объединённые монады,
  как бы `(m1 m2) a`, чтобы можно было дотягиваться до завёрнутых в них значений
  с помощью do-notation, >>= и т.д.

  Попробуем решить эту задачу на примере с паролем и монадой Maybe + IO.
  Для начала определим новый тип, который будет оборачивать Maybe в произвольную монаду
  и уметь выставлять наружу то её, то саму Maybe.
-}

-- Префикс T указывает на то, что это трансформер
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

{-
  Суть трансорфмера в том, что он может "превращаться" из одной монады в другую.
  Это в том числе значит, что он — тоже монада.

  Как мы помним, монадой может стать только апликативный функтор, так что перед
  instance'ом монады добавим instance'ы Functor и Applicative.
  Мы будем использовать монадные функции из Control.Monad, поэтому добавим
  ограничение на тип m
-}

instance Monad m => Functor (MaybeT m) where
  -- lift — Mонадный аналог fmap из Control.Monad
  -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
  fmap = liftM

instance Monad m => Applicative (MaybeT m) where
  pure = return

  -- ap — Mонадный аналог (<*>) из Control.Monad
  -- ap :: Monad m => m (a -> b) -> m a -> m b
  (<*>) = ap

instance Monad m => Monad (MaybeT m) where
  -- Мы последовательно оборачиваем переданное значение в монады.
  -- Сначала в Maybe с помощью Just, затем в монаду m с помощью return.
  -- И затем отдаём результат конструктору трансформера
  return  = MaybeT . return . Just

  -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (>>=) transformer f = MaybeT $ do -- Нужен конструктор, чтобы вернуть MeybeT
    -- достаём вычисление из трансформера и выполняем его с помощью <-
    maybe <- runMaybeT transformer
     -- результат должен быть типа m (MaybeT b), т.к. перед do есть вызов конструктора
     -- MaybeT :: m (Maybe a) -> MaybeT m a
    case maybe of
        -- eсли его результат Nothing, то просто оборачиваем его в m
        Nothing    -> return Nothing
         -- если результат Just, то
         -- - выполняем преобразование, резльутат которого MaybeT m b
         -- - вытаскиваем m b с помощью runMaybeT
        Just value -> runMaybeT $ f value
-- Если продраться через сложности, связанные с типами, то суть (>>=) сводится к (>>=) для Maybe

{-
  Мы сделали MaybeT монадой, пришло время сделать её собственно трансформером.
  Для этого нужно реализовать класс MonadTrans:

    > :i MonadTrans
    class MonadTrans (t :: (* -> *) -> * -> *) where
      lift :: Monad m => m a -> t m a

  Сначала посмотрим на этот впечатляющий kind (* -> *) -> * -> *) и убедимся, что
  у MaybeT он такой. Конструктор MaybeT принимает два типа `m` и `a`.
    - `m` здесь монада, значит она берёт простой тип и позвращает новый, её kind — * -> *
    - `a` здесь просто тип, тот, который мы обернём в Maybe, его kind — *
  Значит MaybeT берёт (* -> *) и (*) и возвращает новый тип. Как раз то, что надо.

  Теперь повнимательней посмотрим на функцию lift:

    lift :: Monad m => m a -> t m a

  Судя по типу, она должна брать монаду оборачивать её в трансформер, т.е. "поднимать"
  базовое вычисление до более сложного вычисления.

  В нашем случае нужно из `m a` получить `MaybeT m a`, это можно сделать через
  конструктор:

    > :t MaybeT
    MaybeT :: m (Maybe a) -> MaybeT m a

  Значит, нам нужна функция f :: m a -> m (Maybe a), т.е. такая функция, которая
  обработает значение внутри монады. У нас есть функция, которая умеет обрабатывать
  значение внутри монады — fmap. Нужно только передать ей функцию g :: a -> Maybe a

    > :t fmap Just
    fmap Just :: Functor f => f a -> f (Maybe a)
-}

-- Теперь у нас есть всё, чтобы реализовать класс MonadTrans для MaybeT
instance MonadTrans MaybeT where
    lift m = MaybeT (fmap Just m)
-- Так же можно было использовать liftM

-- Заведём специальное значение с Nothing в MaybeT
empty :: Monad m => MaybeT m a
empty = MaybeT $ return Nothing

getPassphraseT :: MaybeT IO String
getPassphraseT = do
  s <- lift $ getLine
  if isValid s
  then return $ s
  else empty

savePassphraseT :: String -> MaybeT IO ()
savePassphraseT passphrase = lift $ putStrLn "Passphrase is saved"

-- Воспользуемся нашим трансформером в примере с получением правильного пароля
askPassphraseT :: MaybeT IO ()
askPassphraseT = do
  lift $ putStr "Your new passphrase: "
  getPassphraseT >>= savePassphraseT

{-
  Этот небольшой и несколько искусственный пример мог показаться неубедительным.
  Как будто сложности, которые мы прошли ради MaybeT, были больше того, что мы получили.
  Вернёмся к примеру с чтением конфигурации из файлы с помощью Reader. Воспользуемся
  соответствующим трансформером.

    newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

  Напишем Reader'а, который считывает конфигурационный файл
-}

configReaderT :: ReaderT String IO Config
configReaderT = do
    filename <- ask
    lift $ putStrLn ("Read configuration from '" ++ filename ++ "'")
    config <- lift $ readFile filename
    return $ Config (read config)

readConfigT :: String -> IO Config
readConfigT = runReaderT configReaderT
{-
  > readConfig "src/Lecture11/config"
  Read configuration from 'src/Lecture11/config'
  Config {port = 8088}
-}

{-
  Из трансформера ReaderT можно получить и монадный трансформер. Нам только нужно
  передать в качестве монады Identity — монаду, которая просто хранит внутри значение.

    newtype Identity a = Identity {runIdentity :: a}
    type Reader r = ReaderT r Identity

  Поэтому, если импортировать Reader из mtl и посмотреть его определение, то там
  не будет определения через newtype, которое мы делали в качестве примера для Writer

    Control.Monad.Reader> :i Reader
    type Reader r = ReaderT r Data.Functor.Identity.Identity :: * -> *

  Т.к. Reader, Writer, State и т.д. это паттерн, реализовать который можно по-разному
  (мы уже знаем как минимум два способа) для каждого паттерна есть свой класс типов.
  Например, MonadReader — класс типов, который обобщает паттерн Reader'а.

    class Monad m => MonadReader r m | m -> r where
      ask :: m r
      local :: (r -> r) -> m a -> m a

  Он определён в библиотеке mtl и доступен после import'а Control.Monad.Reader

  Конечно, есть аналогичные трансформеры WriterT, StateT, IdentityT и т.д.
  и вместе с ними аналогичные классы типов MonadWriter, MonadState и т.д.
-}

-- <Задачи для самостоятельного решения>
-- Вы найдёте в src/Lecture11/PersonT.hs
-- </Задачи для самостоятельного решения>

{- Ссылки

  - https://en.wikibooks.org/wiki/Haskell/Monad_transformers
  - https://wiki.haskell.org/Monad_Transformers
  - https://wiki.haskell.org/Monad_Transformer_Library
  - http://book.realworldhaskell.org/read/monad-transformers.html
  - https://web.cecs.pdx.edu/~mpj/pubs/springschool95.pdf
  - https://www.youtube.com/watch?v=8t8fjkISjus
  - https://www.seas.upenn.edu/~cis552/19fa/lectures/stub/Transformers.html
  - https://blog.jle.im/entry/mtl-is-not-a-monad-transformer-library.html
-}
