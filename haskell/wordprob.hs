module WordProblem (answer) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

type Value = Int
data Expr = Value | Expres Value Expr Value
data Operator = Plus | Mult | Div | Subtr | Exp

data CalcExpr = Numb Int
              | Neg CalcExpr
              | Sum CalcExpr CalcExpr
              | Diff CalcExpr CalcExpr
              | Prod CalcExpr CalcExpr
              | Quot CalcExpr CalcExpr
              deriving (Show)

number :: GenParser Char st Int
number = do spaces
            nn <-many1 digit
            spaces
            return (read nn :: Int)

numbExpr :: GenParser Char st CalcExpr
numbExpr = Numb <$> number

operand :: GenParser Char st CalcExpr
operand = try (parenExpr <|> numbExpr)


parenExpr :: GenParser Char st CalcExpr
parenExpr = do keyword "("
               spaces
               ee <- expr
               spaces
               keyword ")"
               spaces
               return ee

opTable = [[prefix "-" Neg],
           [binary "multiplied by" Prod, binary "divided by" Quot],
           [binary "plus" Sum,  binary "minus" Diff]]


binary name fun = Infix (do {keyword name; return fun}) AssocLeft

prefix name fun = Prefix $ do keyword name
                              return fun

keyword kw = do try (string kw) 
                spaces

expr = do ee <- exprParser 
          spaces 
          return ee
    where exprParser = buildExpressionParser opTable operand

fullExpr = do ee <- expr
              eof
              return ee

assumeRight (Right vv) = vv
assumeRight (Left vv) = error (show vv)

parseCalcExpr ss = assumeRight $ parse fullExpr "Parse Error" ss

answer :: [Char] -> CalcExpr
answer = parseCalcExpr

