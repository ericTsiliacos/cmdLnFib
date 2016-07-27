module FibSpec (spec) where

import           Test.Hspec

import           Fib

spec :: Spec
spec = do
  describe "fib" $ do
    it "returns 0 when given 0" $ do
      fib 0 `shouldBe` 0
    it "returns 1 when given 1" $ do
      fib 1 `shouldBe` 1
    it "returns 2 when given 1" $ do
      fib 2 `shouldBe` 1
    it "returns 3 when given 2" $ do
      fib 3 `shouldBe` 2
