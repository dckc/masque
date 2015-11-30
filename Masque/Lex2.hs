module Masque.Lex2 where

import qualified Text.JSON as J
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.IndentParsec(runGIPT)

import Control.Monad.Identity
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.IndentParsec.Token as IT
import qualified Text.Parsec.IndentParsec.Prim as IP

import qualified Masque.Lexer as ML

type TagTok = (String, J.JSValue)

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

tokP :: IT.IndentTokenParser String [Char] Identity
tokP = T.makeTokenParser langDef

{- |
>>> lexSource "def x := 1"
[("def",""),("IDENTIFIER","x"),(":=",""),(".int.","1")]
-}
lexSource :: String -> IO ()
lexSource inp = case lexString inp of
  Right toks -> print toks
  Left e -> do
    putStr "parse error: "
    print e

lexString :: String -> Either P.ParseError [TagTok]
lexString inp =
          let x = runGIPT monteTokens [] "<inp>" inp
              in runIdentity x

type MonteLex a = IP.IndentParsecT String [Char] Identity a

monteTokens :: MonteLex [TagTok]
monteTokens = do
  IT.whiteSpace tokP
  ts <- rest
  P.eof
  return ts
  where
    rest = do
      stack <- P.getState
      case stack of
        ('`':_) -> (++) <$> quasiParts <*> rest
        (_:_) -> (++) <$> scan <*> rest
        [] -> (const [] <$> P.eof) <|> scan

    scan :: MonteLex [TagTok]
    scan =
      (:) <$> quasiStart <*> rest
      <|> (:) <$> (bra "[" ']') <*> rest <|> (:) <$> (ket ']') <*> rest
      <|> (:) <$> (bra "(" ')') <*> rest <|> (:) <$> (ket ')') <*> rest
      <|> (:) <$> (bra "{" '}') <*> rest <|> (:) <$> (ket '}') <*> rest
      <|> (:) <$> monteToken <*> rest
      <|> const [] <$> P.lookAhead (P.oneOf "}])")
    bra :: String -> Char -> MonteLex TagTok
    bra open close = do
      v <- IT.symbol tokP open
      P.modifyState (push close)
      return (v, J.JSNull)
    ket :: Char -> MonteLex TagTok
    ket close = do
      v <- IT.symbol tokP [close]
      stack <- P.getState
      case stack of
        (ch:stack')
          | ch == close
            -> do
              P.setState stack'
              return (v, J.JSNull)
        -- TODO: track position of open
        (expected:_) -> oops (v ++ " rather than " ++ [expected])
        [] -> oops ("extra close: " ++ v)
    oops msg = do
      _ <- P.unexpected msg
      IP.tokeniser $ return ("ERROR", J.showJSON msg)

    quasiStart :: MonteLex TagTok
    quasiStart = do
      _ <- P.char '`'
      P.modifyState (push '`')
      -- TODO: include quasiText in quasi_open
      return ("QUASI_OPEN", J.JSNull)
    push b s = b:s
    pop (_:s) = s
    pop [] = []  -- error? warning?

    quasiParts :: MonteLex [TagTok]
    quasiParts =
      (:) <$> (P.try quasiEnd) <*> return []
      <|> (:) <$> (P.try (hole '$' "DOLLAR_IDENT")
                   <|> P.try (hole '@' "AT_IDENT")) <*> quasiParts
      <|> (\t ts -> collapse (t:ts)) <$> ((\txt -> ("QUASI_TEXT", J.showJSON $ txt)) <$> quasiText) <*> quasiParts
      <|> P.unexpected "quasiText construct"
    collapse :: [TagTok] -> [TagTok]
    collapse (("QUASI_TEXT", J.JSString s1):("QUASI_TEXT", J.JSString s2):toks)
      = ("QUASI_TEXT", J.showJSON $ (J.fromJSString s1) ++ (J.fromJSString s2)):collapse toks
    collapse toks = toks
    quasiText :: MonteLex String
    quasiText =
      (double '`') <|> (double '$') <|> (double '@')
      <|> (P.many1 (P.noneOf "`$@"))
    double :: Char -> MonteLex String
    double c = const [c] <$> P.try ((P.char c) *> (P.char c))

    quasiEnd = do
      _ <- lexeme '`'
      -- TODO: balace ()s
      P.modifyState pop
      return ("QUASI_CLOSE", J.JSNull)
      where
        lexeme c = IT.lexeme tokP $ IP.tokeniser $ sym c
        sym c = (P.char c) <* (P.notFollowedBy (P.char c))

    hole :: Char -> String -> MonteLex TagTok
    hole ch tag = (P.char ch) *> (P.try (pfxIdent tag) <|> pfxOpen ch)
    pfxIdent tag = do
      c0 <- P.satisfy ML.idStart
      c1n <- P.many (P.satisfy ML.idPart)
      return (tag, J.showJSON (c0:c1n))
    pfxOpen ch = do
      _ <- P.char '{'
      P.modifyState (push '}')
      return (ch:'{':[] , J.JSNull)


monteToken :: MonteLex TagTok
monteToken = op <|> literal <|> punct <|> augAssign <|> kw <|> ident
  where
    augAssign = notKw $ P.try $ IT.lexeme tokP opeq
    opeq :: MonteLex String
    opeq = (:) <$> (P.satisfy ML.idStart) <*> (P.many $ P.satisfy ML.idPart) <* (P.char '=')
    notKw :: MonteLex String -> MonteLex TagTok
    notKw p = do
      s <- P.try p
      if elem s kwds then do
        P.unexpected $ "keyword in augmented assignment: " ++ s
        else return ("VERB_ASSIGN", J.showJSON s)
    kw = go IT.reserved kwds P.<?> "keyword"
    op = go IT.reservedOp [op' | (op', _) <- ML.operators] P.<?> "operator"
    punct = go IT.reservedOp [p' | (p', _) <- ML.punctuation]
    go _ [] = error "at least 1"
    go f [t1] = go' f t1
    go f (t1:t2:ts) = (go' f t1) <|> (go f (t2:ts))
    go' f t = do
      _ <- f tokP t
      return (t, J.JSNull)
    ident = tagged "IDENTIFIER" <$> IT.identifier tokP
    tagged t s = (t, J.showJSON s)
    literal = (P.try integer) <|> (P.try str)
    str = tagged ".String." <$> IT.stringLiteral tokP
    integer = shown ".int." <$> IT.integer tokP
    shown t x = (t, J.showJSON x)
    kwds = [kw' | (kw', _) <- ML.keywords]
-- semiSep = IT.semiSepOrFoldedLines tokP

