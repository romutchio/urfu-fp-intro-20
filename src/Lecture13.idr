module Lecture13

-- Idris v1.3.2
-- cabal install idris
-- Источник: https://gist.github.com/chrisdone/672efcd784528b7d0b7e17ad9c115292

{-
  Тип для представления строки форматирования.
-}
data Format
  = FInt Format
  | FString Format
  | FOther Char Format
  | FEnd

{-
  Примеры представления строки форматирования в виде дерева выражений:

  - format "%d" ~> FInt FEnd
  - format "%d%s" ~> FInt (FString FEnd)
  - format "%d %s" ~> FInt (FOther ' ' (FString FEnd))

  Note:
    Haskell особенный язык, потому что в нем поменяли местами символы `::` и `:`.

    `:` в Idris тоже самое, что `::` в Хаскеле, а `::` это тоже самое что `:` (cons для списка)
    и так почти во всех ML языках, так как `:` — стандартная математическая нотация
    для объявления типов: `t : T`, `λx.x : a -> a`.
-}
format : List Char -> Format
format ('%' :: 'd' :: cs ) = FInt ( format cs )
format ('%' :: 's' :: cs ) = FString ( format cs )
format ( c :: cs )         = FOther c ( format cs )
format []                  = FEnd

-- По заданному нам формату генерируем тип для функции `printf`:
buildPrintfType : Format -> Type
buildPrintfType (FInt f)     = Int -> buildPrintfType f
buildPrintfType (FString f)  = String -> buildPrintfType f
buildPrintfType (FOther _ f) = buildPrintfType f
buildPrintfType FEnd         = String

{-
  Функция для создания терма для printf.

  По заданному формату `fmt` и строки-аккумулятора генерирует
  функцию типа `buildPrintfType fmt`, который вычисляется на основе
  заданного формата `fmt`.

  В Idris для анонимных функций используется немного другой синтаксис:
    \i => ...
  который соответствует привычному
    \i -> ...
  
  `singleton : Char -> String` позволяет создать список из одного символа
  
  idris — строгий язык (strict evaluation), поэтому пишем хвостовую рекурсию
-}
buildPrintfTerm : (fmt : Format) -> String -> buildPrintfType fmt
buildPrintfTerm ( FInt f ) a     = \i => buildPrintfTerm f ( a ++ show i )
buildPrintfTerm ( FString f ) a  = \s => buildPrintfTerm f ( a ++ s )
buildPrintfTerm ( FOther c f ) a = buildPrintfTerm f ( a ++ singleton c )
buildPrintfTerm FEnd a           = a

{-
  В Idris String — это не синоним [Char], а как `Text` в Haskell. То есть упакованная строка.
  Поэтому `formatString` использует функцию `unpack` для удобства использования в `printf`.
-}
formatString : String -> Format
formatString s = format ( unpack s )

{-
  Функция `printf` принимает строку форматирования и возвращает функцию,
  количество аргументов которой зависит от строки форматирования:

  -  printf "%d" : Int -> String
  -  printf "%d %s" : Int -> String -> String
  -  printf "%d %s %d" : Int -> String -> Int -> String
-}
printf : (s : String) -> buildPrintfType ( formatString s )
printf s = buildPrintfTerm ( formatString s ) ""
