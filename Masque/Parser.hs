module Masque.Parser where

import Control.Monad
import Control.Applicative (Applicative(..), Alternative(..),
                            (<$>), (<*), (*>))

import Masque.Lexer (Token(..), Shape(..), Direction(..),
                     lexer, offside,
                     keywords, brackets, operators, punctuation)
import Masque.AST (Expr(..))

-- adapted from http://dev.stephendiehl.com/fun/002_parsers.html
newtype Parser i o = Parser { parse :: [i] -> [(o, [i])] }

runParser :: (Show i) => Parser i o -> [i] -> o
runParser p input = case parse p input of
  [(res, [])] -> res
  [(_, rs)] -> error $ "Parse error at: " ++ show (take 3 rs)
  _ -> error "Parse error"  -- TODO: better error messages (with Parsec?)

item :: Parser i i
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

unit :: o -> Parser i o
unit o = Parser (\s -> [(o, s)])

bind :: Parser i o -> (o -> Parser i o') -> Parser i o'
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance Functor (Parser i) where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative (Parser i) where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad (Parser i) where
  return = unit
  (>>=)  = bind

instance MonadPlus (Parser i) where
  mzero = failure
  mplus = combine

instance Alternative (Parser i) where
  empty = mzero
  (<|>) = alt

combine :: Parser i a -> Parser i a -> Parser i a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser i a
failure = Parser (\_cs -> [])

alt :: Parser i a -> Parser i a -> Parser i a
alt  p q = Parser $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res

option :: Parser i a -> Parser i a
option p = p <|> failure

many1 :: Parser i a -> Parser i [a]
many1 p = some_p
  where
    many_p = some_p <|> pure []
    some_p = (:) <$> p <*> many_p

type TokenParser = Parser Token

expression :: TokenParser [Expr]
expression = seq1 expr

seq1 :: TokenParser o -> TokenParser [o]
seq1 p = some_p <* (option semi)
  where
    many_p = (many1 semi) *> some_p <|> pure []
    some_p = (:) <$> p <*> many_p
    semi = tok ";"

expr :: TokenParser Expr
expr =
      assign
      <|> (exit0 <$> exitKw <* (parens <|> semi))
      <|> (exit1 <$> exitKw <*> expr)
      where
        exitKw = (tok "continue") <|> (tok "break") <|> (tok "return")
        parens = (tok "(") *> (tok ")")
        semi = tok ";"
        exit1 kw e = CallExpr (NounExpr kw) "run" [e] [] -- @@
        exit0 kw = CallExpr (NounExpr kw) "run" [] [] -- @@

assign = literal  -- @@

blockExpr =
  hideExpr
  <|> expr
  -- @@ lots more

hideExpr :: TokenParser Expr
hideExpr = do
  es <- (tok "{") *> (seq1 expr) <* (tok "}")
  return $ HideExpr $ case es of
    [e] -> e
    es' -> SequenceExpr es'

literal :: TokenParser Expr
literal = item >>= \t -> case t of
  (TokChar c) -> return $ CharExpr c
  (TokInt i) -> return $ IntExpr i
  (TokDouble d) -> return $ DoubleExpr d
  (TokString s) -> return $ StrExpr s
  _ -> failure

tok :: String -> TokenParser String
tok s = item >>= \t ->
  let
    v =  keywords ++ brackets ++ operators ++ punctuation
  in
    if elem (s, t) v then return s
    else failure


parseSource :: String -> [Expr]
parseSource s = runParser expression tokens
  where
    tokens = offside $ lexer [] s

parseFile :: String -> IO ()
parseFile fileName = do
  code <- readFile fileName
  let e = parseSource code
  print e
