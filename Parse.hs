module Parse
    (
    ) where

import MyOps
import LambdaTerm
import Text.ParserCombinators.Parsec

parseTerm :: CharParser () Term
parseTerm = accumulate []
  where
    accumulate ts =
      do  single <- termInParens <|> parseAbstr <|> parseConst <|> parseVar
          try (spaces >> eof *> return (foldr1 (flip Apply) (single:ts)))
            <|> (many1 (char ' ') *> accumulate (single:ts))
    parseName = many1 (noneOf keyChar)
    parseConst = Atom <$> (char '@' *> parseName)
    parseVar = Var <$> parseName
    parseAbstr =
      Abstr <$> (char 'λ' *> mayInSpaces parseName <* char '.')
            <*> (spaces *> parseTerm <* eof)
    termInParens = do
      _ <- char '('
      input <- getInput
      let (inside, left) = findCloseParen input
      case parseExpr inside of
        Right e -> e <$ setInput left
        Left _ -> pzero


mayInSpaces :: CharParser () a -> CharParser () a
mayInSpaces x = spaces *> x <* spaces

keyChar :: String
keyChar = " \n@.*()/:"

mkApply :: [Term] -> Term
mkApply = foldl1 Apply

findCloseParen :: String -> (String, String)
findCloseParen = loop (1::Int) []
  where
    loop 0 acc t = (reverse acc, t)
    loop n acc (t:ts) =
      let n' = n + delta t in
        if n' == 0 then (reverse acc, ts)
        else loop n' (t:acc) ts
    loop n acc _ = error $ "n=" ++ show n ++ "; acc=" ++ show acc
    delta '(' = 1
    delta ')' = -1
    delta _ = 0

    

{- | Parse lamdba expression string into 'Term'
prop> parseExpr "a" == Right (Var "a")
prop> parseExpr "@abc" == Right (Atom "abc")
prop> parseExpr " a λx. w v " == Right (Var "a" # lambda "x" (_w # _v))
prop> parseExpr (prettyShow e) == Right e
-}
parseExpr :: [Char] -> Either ParseError Term
parseExpr = parse (spaces *> parseTerm <* eof) ""
