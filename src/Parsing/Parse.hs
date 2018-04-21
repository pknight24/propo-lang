module Parsing.Parse where

import Parsing.Interpret
import Types.Expressions
import Types.Stately
import Text.Parsec
import Text.Parsec.String
import Control.Monad.State
import qualified Data.Map as M

illegal_chars = " \t\n-1234567890.,:/()"

whitespace :: Parser [Char]
whitespace = many $ oneOf " \t\n"

parseEval :: Parser [Char]
parseEval = do
              whitespace
              string "eval"
              whitespace
              string "("
              e <- try parseExpr <|> many1 (noneOf illegal_chars)
              whitespace
              string ")"
              string "."
              return e

parseEvalExpr :: Vars -> Parser Expr
parseEvalExpr v = try (parseExprStr v) <|> (parseVariable v)

parseVariable :: Vars -> Parser Expr
parseVariable v = do
  name <- many1 $ noneOf illegal_chars
  case (M.lookup name v) of
    Just e -> return e
    Nothing -> return Null 

parseExpr :: Parser [Char]
parseExpr = do 
              string "T" 
              return ("(Atom T)")
            <|> do 
                  string "F"
                  return ("(Atom F)")
            <|> do 
                  string "("
                  first <- parseExpr
                  whitespace
                  comb <- string "and" <|> string "or" <|> try (string "implies") <|> string "iff"
                  whitespace
                  second <- parseExpr
                  string ")"
                  let res = case comb of "and" -> ("(And " ++ first ++ " " ++ second ++ ")")
                                         "or"  -> ("(Or " ++ first ++ " " ++ second ++ ")")
                                         "implies" -> ("(Implies " ++ first ++ " " ++ second ++ ")")
                                         "iff" -> ("(Iff " ++ first ++ " " ++ second ++ ")") 
                  return res
            <|> do
                  string "not"
                  whitespace
                  e <- parseExpr
                  whitespace
                  return ("(Not " ++ e ++ ")")
            <|> many1 (noneOf illegal_chars)
  
parseExprStr :: Vars -> Parser Expr
parseExprStr v = char '(' >> (try (parseAtom v)
                        <|> try (parseAnd v)
                        <|> try (parseOr v)
                        <|> try (parseImplies v)
                        <|> try (parseIff v)
                        <|> try (parseNot v)) >>= (\e -> char ')' >> return e)

parseAtom :: Vars -> Parser Expr
parseAtom v = string "Atom" >> whitespace >> oneOf "TF" >>= (\c -> case c of 'T' -> return (Atom True)
                                                                             'F' -> return (Atom False)) 
parseAnd :: Vars -> Parser Expr
parseAnd v = string "And" >> do
                             whitespace
                             first  <- parseEvalExpr v
                             whitespace
                             second <- parseEvalExpr v
                             whitespace
                             return (And first second) 

parseOr :: Vars -> Parser Expr
parseOr v = string "Or" >> do
                            whitespace
                            first  <- parseEvalExpr v
                            whitespace
                            second <- parseEvalExpr v
                            whitespace
                            return (Or first second)

parseImplies :: Vars -> Parser Expr
parseImplies v = string "Implies" >> do
                                     whitespace
                                     first <- parseEvalExpr v
                                     whitespace
                                     second <- parseEvalExpr v
                                     whitespace
                                     return (Implies first second)

parseIff :: Vars -> Parser Expr
parseIff v = string "Iff" >> do
                              whitespace
                              first <- parseEvalExpr v
                              whitespace
                              second <- parseEvalExpr v
                              whitespace
                              return (Iff first second)
parseNot :: Vars -> Parser Expr
parseNot v = string "Not" >> do
                             whitespace
                             first <- parseEvalExpr v
                             whitespace
                             return (Not first)



parseDefine :: Parser [Char]
parseDefine = do
  whitespace
  string "let"
  whitespace
  name <- many1 $ noneOf illegal_chars
  whitespace
  string "be"
  whitespace
  ex <- try parseExpr <|> many1 (noneOf illegal_chars)
  string "."
  return (name ++ "-" ++ ex)

def :: Vars -> Parser Vars
def s = parseDefine >>= (\d -> let splitTheD = splitDefine d in
                               let val = case (parse (try (parseEvalExpr s)) "" (snd splitTheD)) of Right a -> a
                                                                                                    Left _  -> Null in
                               return $ snd $ runState (addVar (fst splitTheD) val) s)

getVars :: Vars -> Parser Vars
getVars v = do
  stat <- def v
  try (getVars stat) <|> (return stat)

bigParse :: Parser (Maybe Bool)
bigParse = do
  finalState <- getVars initialState
  evalText <- parseEval
  case (parse (parseEvalExpr finalState) "" evalText) of
    Left  _ -> return Nothing
    Right e -> return $ eval_base e
