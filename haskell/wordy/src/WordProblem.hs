module WordProblem where


import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void ( Void )

type Parser = Parsec Void String

answer :: String -> Maybe Integer
answer problem = case exprToInt <$> parse (whatIsParser *> pExpr) "" problem of
                    Right v -> v
                    Left _ -> Nothing


whatIsParser :: Parser String
whatIsParser = lexeme $ string' "What is "

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-}" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

data Expr =  Var String | Int Integer | Negation Expr
           | Sum Expr Expr | Subtr Expr Expr | Product Expr Expr
           | Div Expr Expr deriving (Eq, Ord, Show)

pVariable :: Parser Expr
pVariable = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInt :: Parser Expr
pInt = Int <$> lexeme L.decimal <* notFollowedBy pInt

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = parens pExpr <|> pVariable <|> pInt <?> "number"

pExpr :: Parser Expr
pExpr = makeExprParser pTerm opsTable <* notFollowedBy letterChar

opsTable :: [[Operator Parser Expr]]
opsTable = [ [prefix "-" Negation , prefix "+" id]
           , [binary "divided by" "/" Div]
           , [binary "plus" "+" Sum      , binary "minus" "-" Subtr]
           , [binary "multiplied by" "*" Product ] ]

binary :: String -> String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary word sym f = InfixL (f <$ try (symbol sym <|> lexeme (string' word)))

prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

exprToInt :: Expr -> Maybe Integer
exprToInt (Int i) = pure i
exprToInt (Sum v1 v2) = (+) <$> exprToInt v1 <*> exprToInt v2
exprToInt (Product v1 v2) = (*) <$> exprToInt v1 <*> exprToInt v2
exprToInt (Div v1 v2)
 | exprToInt v2 == Just 0 = Nothing
 | otherwise = div <$> exprToInt v1 <*> exprToInt v2
exprToInt (Negation v1) = negate <$> exprToInt v1
exprToInt (Subtr v1 v2) = subtract <$> exprToInt v2 <*> exprToInt v1
exprToInt _ = Nothing


