{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lecture12 where

import Data.List as List
import Data.Char as Char
import Data.Text as Text

{-
  12: Web-application
-}

{- String или ...?

  Мы уже знаем, что одна из особенностей стандартных структур данных
  в Haskell — наличие для каждой из них двух реализаций: строгой и ленивой.
  Есть и ещё одна особенность (или даже странность) — множество разных типов
  для представления строк. Посмотрим на несколько из них поймём, почему
  их так много и чем String в "реальном мире" нас не устраивает.


  type String = [Char]

  Базовый тип для представления строк, синоним списка символов. Главный плюс
  такого представления — можно использовать всё богатство функция для работы
  со списками, вплоть до do-notation.
  Это же и главный недостаток, а именно — неэффективность такого представления,
  из-за неизменяемости списков и накладных расходов на хранение указателей
  для каждого символа.
-}

-- При вызове этой функции будет создано 3 новых строки, не считая аргумента
processString :: String -> String
processString x = List.map Char.toLower $ sort (x ++ "*")

{- Data.Text[.Lazy]

  Text — эффективная реализация unicode-строк. В частности, при использовании
  этого типа в примере выше будет создана всего одна строка, т.к. одну переменную
  типа Text можно "слить" со второй, не создавая промежуточные представления.
  Для этого в Text используется так называемый stream fusion (см. ссылки).
  Если String хранит строку в виде связного списка, то Text указывает на блок
  памяти с символами в Unicode.

  Чтобы преобразовать String в Text и обратно есть специальные функции:

    pack :: String -> Text
    unpack :: Text -> String

  Вместо использования этих функций можно воспользоваться расширением OverloadedStrings,
  которое позволит делать так:

    > text = "Text" :: Text

  Что невозможно просто так, т.к. по умолчанию Haskell считает все строковые литералы
  списком Char'ов:

    > text = "Text" :: Text
    <interactive>:53:8: error:
        * Couldn't match expected type `Text' with actual type `[Char]'

  Это работает для всех типов с instance'ом IsString:

    Data.String> :i IsString
    class IsString a where
      fromString :: String -> a
-}

-- Как вы видите в Data.Text реализованы привычные функции для списков
processStringEffectively :: Text.Text -> Text.Text
processStringEffectively x = Text.map Char.toLower $ Text.append x (Text.pack "*")

{- Data.ByteString[.Lazy]

  Библиотека для low-lewel представления строк. Именно это представление чаще
  всего используется в network(web)-библиотеках, а так в других местаъ, где нужна
  сериализация.
  В основе ByteString лежит уже не Char а Word8 — 8-битное безнаковое челое число,
  другими словами ByteString — просто массив байт.

  Здесь есть совершенно аналогичные функции для конвертации между ByteString и String:

    pack :: String -> ByteString
    unpack :: ByteString -> String

  Data.Text.Encoding содержит функции для конвертации между Text и ByteString.
  Конечно, раз мы имеем дело с low-level представлением строки, для её преобразования
  нам нужно знать кодировку.

    encodeUtf8 :: Text -> ByteString
    decodeUtf8 :: ByteString -> Text

  + Различные вариации для Utf16 и Utf32: encodeUtf16LE, decodeUtf16LE, ..., где
  LE = Little Endian format, BE = Big Endian.
  Эти функции в случае неудачи кидают исключение, если вас это не устраивает, то можно
  использовать их безопасные версии

    decodeUtf8' :: ByteString -> Either UnicodeException Text
    ...
-}

{- О трансформерах

  Трансформеры составляют основу функционального приложения. Именно с помощью
  трансформеров можно комбинировать различные вычисления с эффектами.

  Однако, не все трансформеры одинаково полезны. Reader действительно очень часто
  используется в production приложениях, а вот Writer, State — нет. (см. ссылки)
-}

{- Польза newtype'ов

  В лекции мы рассказывали, что newtype можно использовать для того, чтобы
  отделить разные по смысле объекту с одинаковым типом друг от друга. Это оказывается
  особенно полезным в приложениях. Сравним две функции:

    UpdateDocument :: Id -> Id -> String
    UpdateDocument :: UserId -> DocumentId -> String

  В первом случае очень легко перепутать два Id во время вызова функции и получить ошибку,
  которую получится найти в лучшем случае только при тестировании, а худшем — у пользователя.
  Во втором же случае код, в котором эти два аргумента перепутаны, просто не скомпилируется.
-}

-- <Задачи для самостоятельного решения>
-- src/Lecture12/Readme.md
-- </Задачи для самостоятельного решения>

{- Ссылки

  - Untangling Haskell's Strings              https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings
  - Data.Text                                 http://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html
  - Data.ByteString                           http://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString.html
  - Stream Fusion                             http://fun.cs.tufts.edu/stream-fusion.pdf
  - Faster Haskell lists using stream fusion  https://hackage.haskell.org/package/stream-fusion
  - Writer Monads and Space Leaks             https://journal.infinitenegativeutility.com/writer-monads-and-space-leaks
  -}