module Parse
    (
      tryParseExpr, parseExpr, (<~),
      tryParseTypedTerm, parseTypedTerm
    ) where

import MyOps
import LambdaTerm
import Text.ParserCombinators.Parsec
import TypedTerm

parseName :: CharParser () String
parseName = many1 (noneOf sepChar)

type TypedData = TermShape (VarName, Maybe NamedType)

typedTermParser :: CharParser () (Term, TyConstraintTree)
typedTermParser = extractData <$> parseApply

parseApply :: CharParser () TypedData
parseApply = foldl1 Apply <$> (subTerm `sepSurround1` spaces)
  where
    subTerm = termInParens <|> parseAbstr <|> (Var <$> parseVarData)
    termInParens = char '(' *> parseApply <* char ')'
    parseVarData :: CharParser () (VarName, Maybe NamedType)
    parseVarData = mayInBraces $ do
      varName <- parseName <* spaces
      let typedVar =  do
          t <- char ':' *> parseType
          return (varName, Just t)
      typedVar <|> return (varName, Nothing)
    mayInBraces :: CharParser () a -> CharParser () a
    mayInBraces p = spaces *> ((char '{' *> p <* char '}') <|> p)
    parseAbstr = do
      vars <- char 'λ' *> parseVarData `sepSurround1` spaces <* char '.'
      body <- parseApply
      return (foldr Abstr body vars)
    sepSurround1 sub sep = sep *> sub `sepEndBy1` sep

extractData :: TypedData -> (Term, TyConstraintTree)
extractData (Var (v,t)) = (Var v, Var t)
extractData (Apply a b) = let
  (a1,a2) = extractData a
  (b1,b2) = extractData b
  in (Apply a1 b1, Apply a2 b2)
extractData (Abstr (v,t) body) = let
  (bv,bt) = extractData body
  in (Abstr v bv, Abstr t bt)


parseType :: CharParser () NamedType
parseType = spaces *> parseArrow <* spaces
  where
    parseArrow = foldr1 (~>)  <$> ((spaces *> subTypes <* spaces) `sepBy1` arrow)
    arrow = string "->" <|> string "→"
    subTypes = typeInParens <|> parseTyVar <?> "type term"
    typeInParens = char '(' *> parseType <* char ')'
    parseTyVar = tyVar <$> parseName <?> "type variable"

sepChar :: String
sepChar = " \n@.*()[]{}/:-><→"

useParser :: CharParser () a -> String -> Either ParseError a
useParser p = parse (p <* eof) ""

getEither :: Show a => Either a t -> t
getEither x =
  case x of
    Right y -> y
    Left e -> error $ show e

{- | Parse lamdba expression string into 'Term'
prop> tryParseExpr "a" == Right (Var "a")
prop> tryParseExpr " a λx. w v " == Right (Var "a" # lambda "x" (_w # _v))
prop> tryParseExpr (prettyShow e) == Right e
prop> tryParseExpr "λx y. x y y" == Right (λ "x" (λ "y" (_x # _y # _y)))
-}
tryParseExpr :: String -> Either ParseError Term
tryParseExpr = useParser (fst <$> typedTermParser)

parseExpr :: String -> Term
parseExpr = getEither . tryParseExpr

tryParseTypedTerm :: String -> Either ParseError (Term, TyConstraintTree)
tryParseTypedTerm = useParser typedTermParser

parseTypedTerm :: String -> (Term, TyConstraintTree)
parseTypedTerm = getEither . tryParseTypedTerm

(<~) :: (Term -> a) -> String -> a
f <~ str = f (parseExpr str)
