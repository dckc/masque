module Masque.Lex2 where

import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.IndentParsec(runGIPT)

import Control.Monad.Identity
import Control.Applicative ((<$>), (<*>))
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.IndentParsec.Token as IT
import qualified Text.Parsec.IndentParsec.Prim as IP

import qualified Masque.Lexer as ML

type TagTok = (String, String)

langDef = T.LanguageDef {
  T.commentStart = ""
  , T.commentEnd   = ""
  , T.commentLine  = "#"
  , T.nestedComments = True
  , T.identStart = P.letter   <|> P.char '_'
  , T.identLetter = P.alphaNum <|> P.char '_'
  , T.opStart = P.oneOf "!@$%^&*-+=|.<>/~"
  , T.opLetter = P.oneOf "!@$%^&*-+=|.<>/~"
  , T.reservedNames = [kw | (kw, _) <- ML.keywords]
  , T.reservedOpNames = [op | (op, _) <- ML.operators]
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
  run
  where
    run =     (++) <$> (wrap "[" "]" <$> IT.brackets tokP run) <*> run
          <|> (++) <$> (wrap "(" ")" <$> IT.parens tokP run) <*> run
          <|> (++) <$> (wrap "{" "}" <$> IT.braces tokP run) <*> run
          <|> (:) <$> monteToken <*> run
          <|> return []
    wrap o c ts = [(o, "")] ++ ts ++ [(c, "")]

monteToken :: IP.IndentParsecT String () Identity TagTok
monteToken = kw <|> op <|> punct <|> ident <|> literal
  where
    kw = go IT.reserved [kw' | (kw', _) <- ML.keywords]
    op = go IT.reservedOp [op' | (op', _) <- ML.operators]
    punct = go IT.reservedOp [p' | (p', _) <- ML.punctuation]
    go _ [] = error "at least 1"
    go f [t1] = go' f t1
    go f (t1:t2:ts) = (go' f t1) <|> (go f (t2:ts))
    go' f t = do
      _ <- f tokP t
      return (t, "")
    ident = tagged "IDENTIFIER" <$> IT.identifier tokP
    tagged t s = (t, s)
    literal = integer <|> str
    str = tagged ".String." <$> IT.stringLiteral tokP
    integer = shown ".int." <$> IT.integer tokP
    shown t x = (t, show x)

-- semiSep = IT.semiSepOrFoldedLines tokP

