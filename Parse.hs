module Parse
    (
      tryParseExpr, parseExpr
    ) where

import MyOps
import LambdaTerm
import Text.ParserCombinators.Parsec

parseTerm :: CharParser () Term
parseTerm = foldl1 Apply <$> (spaces *> subTerms `sepEndBy1` spaces)
  where
    subTerms = termInParens <|> parseAbstr <|> parseConst <|> parseVar
    termInParens = char '(' *> parseTerm <* char ')'
    parseName = many1 (noneOf sepChar)
    parseConst = Atom <$> (char '@' *> parseName)
    parseVar = Var <$> parseName
    parseAbstr =
      Abstr <$> (char 'λ' *> mayInSpaces parseName <* char '.')
            <*> (spaces *> parseTerm)


mayInSpaces :: CharParser () a -> CharParser () a
mayInSpaces x = spaces *> x <* spaces

sepChar :: String
sepChar = " \n@.*()/:"

{- | Parse lamdba expression string into 'Term'
prop> tryParseExpr "a" == Right (Var "a")
prop> tryParseExpr "@abc" == Right (Atom "abc")
prop> tryParseExpr " a λx. w v " == Right (Var "a" # lambda "x" (_w # _v))
prop> tryParseExpr (prettyShow e) == Right e
-}
tryParseExpr :: String -> Either ParseError Term
tryParseExpr = parse (parseTerm <* eof) ""

parseExpr :: String -> Term
parseExpr s =
  case tryParseExpr s of
    Right ok -> ok
    Left e -> error $ show e
