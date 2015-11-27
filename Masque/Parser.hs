module Masque.Parser where

import Control.Monad hiding (guard)
import Control.Applicative (Applicative(..), Alternative(..),
                            (<$>), (<*), (*>))

import Masque.Lexer (Token(..), Direction(..),
                     lexer, offside,
                     bracketsDir,
                     keywords, brackets, operators, punctuation)

import Masque.Parsing (Parser(..), runParser)
import Masque.FullSyntax (Expr, Patt)
import Masque.ParseUtil (seq1)
import Masque.SyntaxDiagrams (expr, blockExpr, pattern)

expression :: (Parser Token) [Expr]
expression = seq1 (blockExpr <|> expr)

parseSource :: String -> [Expr]
parseSource s = runParser expression tokens
  where
    tokens = offside $ lexer [] s

parseFile :: String -> IO ()
parseFile fileName = do
  code <- readFile fileName
  let e = parseSource code
  print e
