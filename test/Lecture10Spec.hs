module Lecture10Spec where

import Test.Hspec

import Lecture10
import Lecture10.Reader

testCartesianProduct :: ([Int] -> [Char] -> [(Int,Char)]) -> String -> SpecWith ()
testCartesianProduct f fname = do
  it (fname ++ " [1,2,3] ['a', 'b'] ~> [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]") $
      f [1,2,3] ['a', 'b'] `shouldBe` [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
  it (fname ++ " [] ['a'] ~> []") $
      f [] ['a'] `shouldBe` []

spec :: Spec
spec = do
  describe "list do notation" $ do
    testCartesianProduct cartesianProduct           "cartesianProduct          "
    testCartesianProduct cartesianProductMonad      "cartesianProductMonad     "
    testCartesianProduct cartesianProductDoNotation "cartesianProductDoNotation"
  describe "Optional" $ do
    it "pure (\\x -> x + 1) <*> Some 3 ~> Some 4" $
      pure (\x -> x + 1) <*> Some 3 `shouldBe` (Some 4 :: Optional Int)
    it "pure (\\x -> x + 1) <*> None ~> None" $
      pure (\x -> x + 1) <*> None `shouldBe` (None :: Optional Int)
    it "None <*> None ~> None" $
      None <*> (None :: Optional Int) `shouldBe` (None :: Optional Int)
    it "None <*> (Some 3) ~> None" $
      None <*> (Some 3 :: Optional Int) `shouldBe` (None :: Optional Int)
    it "Some 2 >>= \\_ -> None ~> None" $
      ((Some 2 :: Optional Int) >>= \_ -> None) `shouldBe` (None :: Optional Int)
    it "Some 2 >>= \\x -> Some (x + 1) ~> Some 3" $
      ((Some 2 :: Optional Int) >>= \x -> Some $ x + 1) `shouldBe` (Some 3 :: Optional Int)
  describe "List" $ do
    it "pure (\\x -> x + 1) <*> 3 :. Nil ~> 4 :. Nil" $
      pure (\x -> x + 1) <*> 3 :. Nil `shouldBe` (4 :. Nil :: List Int)
    it "(\\x -> x + 1) :. (\\x -> x + 2) :. Nil <*> 1 :. 2 :. Nil ~> 2 :. 3 :. 3 :. 4 :. Nil" $
      (\x -> x + 1) :. (\x -> x + 2) :. Nil <*>
        1 :. 2 :. Nil `shouldBe` (2 :. 3 :. 3 :. 4 :. Nil :: List Int)
    it "(\\x -> x + 1) :. (\\x -> x + 2) :. Nil <*> 10 :. 20 :. Nil ~> 11 :. 21 :. 12 :. 22 :. Nil" $
      (\x -> x + 1) :. (\x -> x + 2) :. Nil <*>
        10 :. 20 :. Nil `shouldBe` (11 :. 21 :. 12 :. 22 :. Nil :: List Int)
    it "1 :. 2 :. 3 :. Nil >>= \\x -> x + 10 :. x + 20 :. Nil ~> 11 :. 21 :. 12 :. 22 :. 13 :. 23 :. Nil" $
      (1 :. 2 :. 3 :. Nil >>= \x -> (x + 10) :. (x + 20) :. Nil) `shouldBe`
        (11 :. 21 :. 12 :. 22 :. 13 :. 23 :. Nil :: List Int)
  describe "Reader" $ do
    it "processPersons [1,2,3]" $ 
      processPersons [1,2,3] `shouldBe`
        [ Just $ "Уважаемый Иван Иванович!\n" ++ 
        "Разрешите предложить Вам наши услуги."
        , Just $ "Уважаемые Петр Петрович и Екатерина Алексеевна!\n" ++
        "Разрешите предложить вам наши услуги."
        , Just $ "Уважаемая Алия Фаридовна!\n" ++
        "Разрешите предложить Вам наши услуги."
        ]
    it "processPersons [9,7,10]" $ 
      processPersons [9,7,10] `shouldBe`
        [ Just $ "Уважаемый Юрий Васильевич!\n" ++ 
        "Разрешите предложить Вам наши услуги."
        , Just $ "Уважаемые Петр Петрович и Екатерина Алексеевна!\n" ++
        "Разрешите предложить вам наши услуги."
        , Nothing
        ]