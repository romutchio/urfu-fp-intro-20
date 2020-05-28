{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Lecture13Spec where

import Test.Hspec
import Lecture13


toList :: Vec a n -> [a]
toList VNil = []
toList (VCons x xs) = x : (toList xs)

spec :: Spec
spec = do
  describe "instance Eq (HList xs)" $ do
    it "HCons 3 $ HCons \"string\" $ HCons True $ HNil == HCons 3 $ HCons \"string\" $ HCons True $ HNil" $
      (HCons (3::Int) $ HCons ("string"::[Char]) $ HCons True $ HNil) == (HCons 3 $ HCons "string" $ HCons True $ HNil)
        `shouldBe` True
    it "HNil == HNil" $
      HNil == HNil `shouldBe` True
    it "HCons True $ HNil /= HCons False $ HNil" $
      (HCons True $ HNil) == (HCons False $ HNil) `shouldBe` False
  describe "vzip" $ do
    it "vzip VNil VNil ~ VNil" $
      toList (vzip (VNil :: Vec Int 'Zero) (VNil :: Vec Char 'Zero)) `shouldBe` ([] :: [(Int, Char)])
    it "vzip [123, 0, 3.1] ['a', 'e', 'c'] ~ [(123, 'a'), (0, 'e'), (3.1, 'c')]" $
      toList (vzip (VCons (123::Double) $ VCons 0   $ VCons 3.1 $ VNil)
              (VCons 'a' $ VCons 'e' $ VCons 'c' $ VNil) )
      `shouldBe` [(123, 'a'), (0, 'e'), (3.1, 'c')]