module Main where

import           UseCases.FibNumbers

instance CommandLine IO where
  writeLine = putStrLn
  readLine = getLine

main :: IO ()
main = fibNumbersUseCase
