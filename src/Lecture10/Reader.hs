module Lecture10.Reader where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Prelude hiding (id)

-- <Задачи для самостоятельного решения>

{-
  Задача: по имеющейся базе данных сфомировать набор рекламных писем.
  Для неженатого(-ой) персоны письмо должно иметь вид:

  Для мужчины:

"""
  Уважаемый Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Для женщины:

"""
  Уважаемая Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Семейным парам шлется одно письмо вида

"""
  Уважаемые Имя_мужа Отчество_мужа и Имя_жены Отчество_жены!
  Разрешите предложить вам наши услуги.
"""

-}

data Sex = Male | Female deriving (Show, Eq, Ord)

type PersonId = Int

data Person = Person
  { id :: Int
  , family :: String
  , name :: String
  , surname :: String
  , sex :: Sex
  , marriedBy :: Maybe Int
  } deriving (Show, Eq, Ord)

persons :: [Person]
persons =
  [ Person 1 "Иванов" "Иван" "Иванович" Male Nothing
  , Person 2 "Петров" "Петр" "Петрович" Male (Just 7)
  , Person 3 "Соловьева" "Алия" "Фаридовна" Female Nothing
  , Person 4 "Кузнецова" "Мария" "Ивановна" Female (Just 8)
  , Person 5 "Гринько" "Юлия" "Владимировна" Female Nothing
  , Person 6 "Кабанов" "Александр" "Романович" Male Nothing
  , Person 7 "Петрова" "Екатерина" "Алексеевна" Female (Just 2)
  , Person 8 "Кузнецов" "Евгений" "Семёнович" Male (Just 4)
  , Person 9 "Антонов" "Юрий" "Васильевич" Male Nothing
  ]

-- Поиск персоны по номеру
findById :: PersonId -> Reader [Person] (Maybe Person)
findById pId = do
        _persons <- ask
        return $ find (\p -> id p == pId) _persons

processSingle :: Person -> String
processSingle (Person _ _ name surname sex _) = case sex of
                            Male -> "Уважаемый " ++ name ++ " " ++ surname ++ "!" ++ "\n" ++ serviceOfferSingle
                            Female -> "Уважаемая " ++ name ++ " " ++ surname ++ "!" ++ "\n" ++ serviceOfferSingle

processPair :: Person -> Person -> String
processPair (Person hId _ hn hs _ (Just mwId)) (Person wId _ wn ws _ (Just mhId)) 
                                      | mwId == wId && mhId == hId =
                                         "Уважаемые "++ hn ++ " " ++ hs ++ " и " ++ wn ++ " " ++ ws ++ "!" ++ "\n" ++ serviceOfferPlural
processPair _ _ = error "wrong pair"

processPerson :: PersonId -> Reader [Person] (Maybe String)
processPerson pId = do
          f <- findById pId
          s <- case f of
            Just (Person _ _ _ _ _ (Just id)) -> findById id
            _ -> return $ Nothing
          return $ case (f,s) of
            (Just h@(Person _ _ _ _ Male _), Just w@(Person _ _ _ _ Female _)) -> Just (processPair h w)
            (Just w@(Person _ _ _ _ Female _), Just h@(Person _ _ _ _ Male _)) -> Just (processPair h w)
            (Just p, _) -> Just (processSingle p)
            _ -> Nothing

processPersons :: [PersonId] -> [Maybe String]
processPersons personIds = do
              pId <- personIds
              return (runReader (processPerson pId) persons)


------helpers-------

serviceOfferSingle :: String
serviceOfferSingle = "Разрешите предложить Вам наши услуги."

serviceOfferPlural :: String
serviceOfferPlural = "Разрешите предложить вам наши услуги."

-- </Задачи для самостоятельного решения>
