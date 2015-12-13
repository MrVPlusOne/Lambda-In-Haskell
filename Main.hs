module Main where

import System.Environment (getArgs)
import HtmlPrint
import Parse

main :: IO ()
main = do
  input <- head <$> getArgs
  putStrLn $ case tryParseTypedTerm input of
    Right result -> uncurry inferConstraintHtml result
    Left e -> show e
