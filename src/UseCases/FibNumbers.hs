module UseCases.FibNumbers where

import           Data.Char
import           Fib
import           Text.Read

fibNumbersUseCase :: CommandLine m => m ()
fibNumbersUseCase = do
  arg <- readLine
  let n = readMaybe arg :: Maybe Int
  case n of
    Just a -> (writeLine . show) $ fib <$> [0..a]
    Nothing -> writeLine "Please provide a number."

class Monad m => CommandLine m where
  writeLine :: String -> m ()
  readLine :: m String
