module Parse
    (
      tryParseExpr, parseExpr, (<~)
    ) where

import MyOps
import LambdaTerm
import Text.ParserCombinators.Parsec

parseTerm :: CharParser () Term
parseTerm = try(substitute <$> parseSubst <*> parseApply)
        <|> parseApply
  where
    parseSubst = do
      replace <- spaces *> char '[' *> parseApply
      v <- char '/' *> spaces *> parseName <* char ']'
      return (v, replace)
    parseApply = foldl1 Apply <$> (subTerms `sepSurround1` spaces)
    subTerms = termInParens <|> parseAbstr <|> parseVar
    termInParens = char '(' *> parseTerm <* char ')'
    parseName = many1 (noneOf sepChar)
    parseVar = Var <$> parseName
    parseAbstr = do
      vars <- char 'λ' *> parseName `sepSurround1` spaces <* char '.'
      body <- parseTerm
      return (foldr Abstr body vars)
    sepSurround1 sub sep = sep *> sub `sepEndBy1` sep


mayInSpaces :: CharParser () a -> CharParser () a
mayInSpaces x = spaces *> x <* spaces

sepChar :: String
sepChar = " \n@.*()[]/:"

{- | Parse lamdba expression string into 'Term'
prop> tryParseExpr "a" == Right (Var "a")
prop> tryParseExpr " a λx. w v " == Right (Var "a" # lambda "x" (_w # _v))
prop> tryParseExpr (prettyShow e) == Right e
prop> tryParseExpr "λx y. x y y" == Right (λ "x" (λ "y" (_x # _y # _y)))
-}
tryParseExpr :: String -> Either ParseError Term
tryParseExpr = parse (parseTerm <* eof) ""

parseExpr :: String -> Term
parseExpr s =
  case tryParseExpr s of
    Right ok -> ok
    Left e -> error $ show e

(<~) :: (Term -> a) -> String -> a
f <~ str = f (parseExpr str)
