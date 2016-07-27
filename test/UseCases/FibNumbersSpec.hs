{-# LANGUAGE FlexibleInstances #-}

module UseCases.FibNumbersSpec (spec) where

import           Control.Monad.State (State, get, put, runState)
import           Test.Hspec

import           UseCases.FibNumbers

spec :: Spec
spec = do
  describe "fibNumbersUseCase" $ do

    describe "when the user inputs a valid number" $ do
      it "ouputs a list of fib numbers up until the provided number" $ do
        (getOutput . forInput) ["1"] `shouldBe` ["[0,1]"]

    describe "when the user inputs a non integer value" $ do
      it "asks the user to provide a number" $ do
        (getOutput . forInput) ["a"] `shouldBe` ["Please provide a number."]


forInput :: [String] -> ((), FakeState)
forInput i = runScript preRecordedScript fibNumbersUseCase
  where
    preRecordedScript = createInputScript i

getOutput :: ((), FakeState) -> [String]
getOutput (_, output) = fsWrittenLines output

createInputScript :: [String] -> FakeState
createInputScript input = FakeState {
  fsWrittenLines = [],
  fsReadLine = return <$> input
}

type FakeIO = State FakeState

data FakeState = FakeState
  { fsWrittenLines :: [String]
  , fsReadLine     :: [FakeIO String]
  }

instance CommandLine (State FakeState) where
  writeLine s = do
    st <- get
    let oldLines = fsWrittenLines st
    put st { fsWrittenLines = s:oldLines }

  readLine = do
    st <- get
    let (readLineAction:xs) = fsReadLine st
    readLineAction

runScript :: b -> State b a -> (a, b)
runScript = flip runState
