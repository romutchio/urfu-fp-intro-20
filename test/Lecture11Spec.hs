module Lecture11Spec where

import Test.Hspec
import Lecture11.PersonsT
import Lecture10.Reader (Person (..), Sex (..))

spec :: Spec
spec = do
  describe "findById" $ do
    it "fst $ fst $ runPersons (findById  1) ~> Person 1 Иванов ..." $
      (fst $ fst $ runPersons (findById 1)) `shouldBe` Just (Person 1 "Иванов" "Иван" "Иванович" Male Nothing)
  describe "processPerson" $ do
    it "fst $ fst $ runPersons (processPerson  4) doesn't throw an exception" $
      (fst $ fst $ runPersons (processPerson 4)) `shouldSatisfy` \_ -> True