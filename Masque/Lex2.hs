module Masque.Lex2 where

import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.IndentParsec(runGIPT)

import Control.Monad.Identity
import Control.Applicative ((<$>))
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.IndentParsec.Token as IT
import qualified Text.Parsec.IndentParsec.Prim as IP

type TagTok = (String, String)

langDef = T.LanguageDef {
  T.commentStart = ""
  , T.commentEnd   = ""
  , T.commentLine  = "#"
  , T.nestedComments = True
  , T.identStart = P.letter   <|> P.char '_'
  , T.identLetter = P.alphaNum <|> P.char '_'
  , T.opStart = P.oneOf "!@$%^&*-+=|.,<>/~"
  , T.opLetter = P.oneOf "!@$%^&*-+=|.,<>/~"
  , T.reservedNames = [ "def" ]
  , T.reservedOpNames = [ ":=" , "+" , "-", "*", "/"]
  , T.caseSensitive = False
  }

tokP :: IT.IndentTokenParser String () Identity
tokP = T.makeTokenParser langDef

{- |
>>> lexSource "def x := 1"
[("def",""),("IDENTIFIER","x"),(":=",""),(".int.","1")]
-}
lexSource :: String -> IO ()
lexSource inp =
          let x = runGIPT monteTokens () "<inp>" inp
              in case runIdentity x of
                      Right toks -> print toks
                      Left e -> do putStr "parse error: "
                                   print e

monteTokens :: IP.IndentParsecT String () Identity [TagTok]
monteTokens = do
  IT.whiteSpace tokP
  P.many monteToken


monteToken :: IP.IndentParsecT String () Identity TagTok
monteToken = kw <|> op <|> ident <|> literal
  where
    kw = go IT.reserved "def"
    go f t = do
      _ <- f tokP t
      return (t, "")
    op = go IT.reservedOp ":="
    ident = tagged "IDENTIFIER" <$> IT.identifier tokP
    tagged t s = (t, s)
    literal = integer
    integer = shown ".int." <$> IT.integer tokP
    shown t x = (t, show x)

-- semiSep = IT.semiSepOrFoldedLines tokP

