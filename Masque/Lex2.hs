module Masque.Lex2 where

import qualified Text.JSON as J
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC
import Text.Parsec.IndentParsec(runGIPT)

import Control.Monad.Identity
import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
import qualified Data.List as L
import qualified Data.Map as M
import Numeric (readHex)
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
lexString inp = lexStringIn [] inp

lexStringIn :: [Char] -> String -> Either P.ParseError [TagTok]
lexStringIn stack inp =
          let x = runGIPT monteTokens stack "<inp>" inp
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
        ('`':_) -> (const [] <$> P.eof) -- EOF kludge only for testing?
                   <|> (++) <$> quasiParts <*> rest
        _ -> (const [] <$> P.eof)
             <|> (++) <$> scan <*> rest
        -- [] -> (const [] <$> P.eof) <|> scan

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
      <|> const [] <$> P.eof  -- testing KLUDGE?
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
    op = go IT.reservedOp (sorted ML.operators) P.<?> "operator"
    punct = go IT.reservedOp (sorted ML.punctuation)
    sorted vocab = let
      vs = [spelling | (spelling, _) <- vocab]
      in L.sortBy longestFirst vs
    longestFirst s1 s2 = length s2 `compare` length s1
    go _ [] = error "at least 1"
    go f [t1] = go' f t1
    go f (t1:t2:ts) = (go' f t1) <|> (go f (t2:ts))
    go' f t = do
      _ <- f tokP t
      return (t, J.JSNull)
    ident = tagged "IDENTIFIER" <$> IT.identifier tokP
    tagged t s = (t, J.showJSON s)
    literal = (P.try double) <|> (P.try integer)
              <|> (P.try char) <|> (P.try str)
    str :: MonteLex TagTok
    str = shown ".String." <$> IT.lexeme tokP stringLiteral
    integer = shown ".int." <$> IT.lexeme tokP (
      (P.try hexLiteral) <|> (P.try decLiteral))
    double = shown ".float64." <$> IT.lexeme tokP floatLiteral
    char :: MonteLex TagTok
    char = shown ".char." <$> IT.lexeme tokP ((P.char '\'') *> charConstant <* (P.char '\''))
    shown t x = (t, J.showJSON x)
    kwds = [kw' | (kw', _) <- ML.keywords]
-- semiSep = IT.semiSepOrFoldedLines tokP


{-
decLiteral ::= Ap('(read :: String -> Integer)', P('digits'))
-}
decLiteral = (read :: String -> Integer) <$> digits

{-
digits ::= Ap("filter ((/=) '_')",
  Ap('(:)', P('digit'), Many(Choice(0, P('digit'), Char('_')))))
-}
digits = filter ((/=) '_') <$> digits_1
  where
    digits_1 = (:) <$> digit <*> digits_1_2
    digits_1_2 = P.many digits_1_2_1_1
    digits_1_2_1_1 = digit
      <|> (P.char '_')

{-
digit ::= OneOf('0123456789')
-}
digit = (P.oneOf "0123456789")

{-
hexLiteral ::= Ap('(read :: String -> Integer)',
  Ap('(:)', Char('0'),
    Ap('(:)', Choice(0, Char('x'), Char('X')), P('hexDigits'))))
-}
hexLiteral = (read :: String -> Integer) <$> hexLiteral_1
  where
    hexLiteral_1 = (:) <$> (P.char '0') <*> hexLiteral_1_2
    hexLiteral_1_2 = (:) <$> hexLiteral_1_2_1 <*> hexDigits
    hexLiteral_1_2_1 = (P.char 'x')
      <|> (P.char 'X')

{-
hexDigits ::= Ap("filter ((/=) '_')",
  Ap('(:)', P('hexDigit'), Many(Choice(0, P('hexDigit'), Char('_')))))
-}
hexDigits = filter ((/=) '_') <$> hexDigits_1
  where
    hexDigits_1 = (:) <$> hexDigit <*> hexDigits_1_2
    hexDigits_1_2 = P.many hexDigits_1_2_1_1
    hexDigits_1_2_1_1 = hexDigit
      <|> (P.char '_')

{-
hexDigit ::= OneOf('0123456789abcdefABCDEF')
-}
hexDigit = (P.oneOf "0123456789abcdefABCDEF")


{-
floatLiteral ::= Ap('(read :: String -> Double)',
  Ap('(++)',
    P('digits'),
    Choice(0,
      Ap('(++)',
        Ap('(:)', Char('.'), P('digits')),
        Optional(P('floatExpn'), x='""')),
      P('floatExpn'))))
-}
floatLiteral = (read :: String -> Double) <$> floatLiteral_1
  where
    floatLiteral_1 = (++) <$> digits <*> floatLiteral_1_2
    floatLiteral_1_2 = floatLiteral_1_2_1
      <|> floatExpn
    floatLiteral_1_2_1 = (++) <$> floatLiteral_1_2_1_1 <*> floatLiteral_1_2_1_2
    floatLiteral_1_2_1_1 = (:) <$> (P.char '.') <*> digits
    floatLiteral_1_2_1_2 = P.option "" floatExpn

{-
floatExpn ::= Ap('(:)',
  OneOf("eE"),
  Ap('(++)',
    Optional(Ap('pure', OneOf('-+')), x='""'),
    P('digits')))
-}
floatExpn = (:) <$> floatExpn_1 <*> floatExpn_2
  where
    floatExpn_1 = (P.oneOf "eE")
    floatExpn_2 = (++) <$> floatExpn_2_1 <*> digits
    floatExpn_2_1 = P.option "" floatExpn_2_1_1
    floatExpn_2_1_1 = pure <$> floatExpn_2_1_1_1
    floatExpn_2_1_1_1 = (P.oneOf "-+")


charConstant :: MonteLex Char
charConstant = (charConstant_1 *> charConstant_2 )
  where
    charConstant_1 = P.skipMany charConstant_1_1_1
    charConstant_1_1_1 = P.try $ P.string "\\\n"  -- TODO: add try to syntaxDef.
    charConstant_2 = (P.noneOf "'\\\t")
      <|> charConstant_2_2
    charConstant_2_2 = ((P.char '\\') *> charConstant_2_2_2 )
    charConstant_2_2_2 = charConstant_2_2_2_1
      <|> charConstant_2_2_2_2
    charConstant_2_2_2_1 = hexChar <$> charConstant_2_2_2_1_1
    charConstant_2_2_2_1_1 = charConstant_2_2_2_1_1_1
      <|> charConstant_2_2_2_1_1_2
      <|> charConstant_2_2_2_1_1_3
    charConstant_2_2_2_1_1_1 = ((P.char 'U') *> charConstant_2_2_2_1_1_1_2 )
    charConstant_2_2_2_1_1_1_2 = P.count 8 hexDigit
    charConstant_2_2_2_1_1_2 = ((P.char 'u') *> charConstant_2_2_2_1_1_2_2 )
    charConstant_2_2_2_1_1_2_2 = P.count 4 hexDigit
    charConstant_2_2_2_1_1_3 = ((P.char 'x') *> charConstant_2_2_2_1_1_3_2 )
    charConstant_2_2_2_1_1_3_2 = P.count 2 hexDigit
    charConstant_2_2_2_2 = decodeSpecial (P.oneOf "btnfr\\\'\"")

-- strExpr = StrExpr <$> strExpr_1
stringLiteral :: MonteLex String
stringLiteral = {- StrExpr <$> -} strExpr_1
  where
    strExpr_1 = ((P.char '"') *> strExpr_1_2 )
    strExpr_1_2 = P.manyTill charConstant (P.char '"')


hexChar :: String -> Char
hexChar ds = let [(i, _)] = readHex ds in toEnum i

decodeSpecial :: (MonteLex Char) -> (MonteLex Char)
decodeSpecial pch = let
  codes = M.fromList [
    ('b', '\b'),
    ('t', '\t'),
    ('n', '\n'),
    ('f', '\f'),
    ('r', '\r'),
    ('"', '"'),
    ('\'', '\''),
    ('\\', '\\')]
  in do
    code <- pch
    case M.lookup code codes of
      Just ch -> return ch
      Nothing -> P.unexpected ("character escape: \\" ++ [code])
