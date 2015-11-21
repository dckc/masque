{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Masque.Parser where

import Control.Monad
import Control.Applicative (Applicative(..), Alternative(..),
                            (<$>), (<*), (*>))
import Data.Data
import Text.PrettyPrint.GenericPretty (Generic, Out)

import Masque.Lexer (Token(..), Direction(..),
                     lexer, offside,
                     bracketsDir,
                     keywords, brackets, operators, punctuation)


data Expr = CharExpr Char
          | DoubleExpr Double
          | IntExpr Integer
          | StrExpr String
          | ExitExpr String (Maybe Expr) -- String? only 3 possibilities
          | AssignExpr String Expr
          | BindingExpr String
          | CallExpr Expr String [Expr] [NamedExpr]
          | ListExpr [Expr]
          | MapExpr [(Expr, Expr)]
          | RangeExpr Expr String Expr  -- String?
          | BinaryExpr Expr String Expr  -- String?
          | CompareExpr Expr String Expr  -- String?
          | GetExpr Expr [Expr]
          | QuasiParserExpr (Maybe String) [Either String Expr]
          | DefExpr Patt (Maybe Expr) Expr
          | EscapeOnlyExpr Patt Expr
          | EscapeExpr Patt Expr Patt Expr
          | FinallyExpr Expr Expr
          | HideExpr Expr
          | IfExpr Expr Expr Expr
          | NounExpr String
          | ObjectExpr String Patt Expr [Expr] [Method] [Matcher]
          | SequenceExpr [Expr]
          | TryExpr Expr Patt Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data Matcher = Matcher Patt Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data Method = Method String String [Patt] [NamedPatt] Expr Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data NamedExpr = NamedExpr Expr Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data Patt = IgnorePatt (Maybe Expr)
          | BindPatt String
          | FinalPatt String (Maybe Expr)
          | ListPatt [Patt]
          | VarPatt String (Maybe Expr)
          | ViaPatt Expr Patt
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data NamedPatt = NamedPatt Expr Patt Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

instance Out Expr
instance Out NamedExpr
instance Out Method
instance Out Matcher
instance Out Patt
instance Out NamedPatt


-- adapted from http://dev.stephendiehl.com/fun/002_parsers.html
newtype Parser i o = Parser { parse :: [i] -> [(o, [i])] }

runParser :: (Show i) => Parser i o -> [i] -> o
runParser p input = case parse p input of
  [(res, [])] -> res
  [(_, rs)] -> error $ "Parse error at: " ++ show (take 3 rs)
  (_, rs):_ -> error $ "Parse error at: " ++ show (take 3 rs)
  [] -> error "Parse error TODO"  -- TODO: better error messages (with Parsec?)

item :: Parser i i  -- TODO: rename to anyToken
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

optionMaybe :: Parser i a -> (Parser i (Maybe a))
optionMaybe p =
  Just <$> p
  <|> return Nothing

many1 :: Parser i a -> Parser i [a]
many1 p = some_p
  where
    many_p = some_p <|> pure []
    some_p = (:) <$> p <*> many_p

many0 :: Parser i a -> Parser i [a]
many0 p = many1 p <|> pure []


type TokenParser = Parser Token

expression :: TokenParser [Expr]
expression = seq1 blockExpr

seq1 :: TokenParser o -> TokenParser [o]
seq1 p = some_p <* (optionMaybe semi)
  where
    many_p = ((many1 semi) *> some_p) <|> return []
    some_p = (:) <$> p <*> many_p
    semi = symbol ";"

sepBy :: TokenParser o -> TokenParser x -> TokenParser [o]
sepBy p sep = some_p <|> return []
  where
    many_p = (sep *> some_p) <|> return []
    some_p = (:) <$> p <*> many_p

wrapSequence :: TokenParser Expr -> TokenParser x -> TokenParser Expr
wrapSequence p sep = do
  exprs <- sepBy p sep
  return $ case exprs of
    [] -> SequenceExpr []
    [e] -> e
    _ -> SequenceExpr exprs

pair :: a -> b -> (a, b)
pair a b = (a, b)


expr :: TokenParser Expr
expr =
      assign
--      <|> exitExpr

blockExpr :: TokenParser Expr
blockExpr = defExpr
  <|> expr
  <|> failure -- @@ lots more

pattern = finalPatt -- @@

assign :: TokenParser Expr
assign = order  -- @@

prefix = prim -- @@


-- #################################################

{-
guardOpt ::= Maybe(Sigil(':',
 Choice(
     0,
     Ap('GetExpr',
        Ap('NounExpr', 'IDENTIFIER'),
        Brackets('[', SepBy(NonTerminal('expr'), ','), ']')),
     Ap('NounExpr', 'IDENTIFIER'),
     Brackets('(', NonTerminal('expr'), ')'))))
-}
guardOpt = optionMaybe guardOpt_1
  where
    guardOpt_1 = ((symbol ":") *> guardOpt_1_2)
    guardOpt_1_2 = guardOpt_1_2_1
      <|> guardOpt_1_2_2
      <|> guardOpt_1_2_3
    guardOpt_1_2_1 = GetExpr <$> guardOpt_1_2_1_1 <*> guardOpt_1_2_1_2
    guardOpt_1_2_1_1 = NounExpr <$> parseIdentifier
    guardOpt_1_2_1_2 = ((bra "[") *> guardOpt_1_2_1_2_2 <* (ket "]"))
    guardOpt_1_2_1_2_2 = (sepBy expr (symbol ","))
    guardOpt_1_2_2 = NounExpr <$> parseIdentifier
    guardOpt_1_2_3 = ((bra "(") *> expr <* (ket ")"))

{-
DefExpr ::= Ap('DefExpr', Sigil('def', NonTerminal('pattern')),
         Maybe(Sigil("exit", NonTerminal('order'))),
         Sigil(":=", NonTerminal('assign')))
-}
defExpr = DefExpr <$> defExpr_1 <*> defExpr_2 <*> defExpr_3
  where
    defExpr_1 = ((symbol "def") *> pattern)
    defExpr_2 = optionMaybe defExpr_2_1
    defExpr_2_1 = ((symbol "exit") *> order)
    defExpr_3 = ((symbol ":=") *> assign)

{-
order ::= Choice(0,
  NonTerminal('BinaryExpr'),
  NonTerminal('RangeExpr'),
  NonTerminal('CompareExpr'),
  NonTerminal('prefix'))
-}
order = binaryExpr
  <|> rangeExpr
  <|> compareExpr
  <|> prefix

{-
BinaryExpr ::= Choice(0,
  Ap('BinaryExpr', NonTerminal('prefix'),
           "**", NonTerminal('order')),
  Ap('BinaryExpr', NonTerminal('prefix'),
           Choice(0, "*", "/", "//", "%"), NonTerminal('order')),
  Ap('BinaryExpr', NonTerminal('prefix'),
           Choice(0, "+", "-"), NonTerminal('order')),
  Ap('BinaryExpr', NonTerminal('prefix'),
           Choice(0, "<<", ">>"), NonTerminal('order')))
-}
binaryExpr = binaryExpr_1
  <|> binaryExpr_2
  <|> binaryExpr_3
  <|> binaryExpr_4
  where
    binaryExpr_1 = BinaryExpr <$> prefix <*> (symbol "**") <*> order
    binaryExpr_2 = BinaryExpr <$> prefix <*> binaryExpr_2_2 <*> order
    binaryExpr_2_2 = (symbol "*")
      <|> (symbol "/")
      <|> (symbol "//")
      <|> (symbol "%")
    binaryExpr_3 = BinaryExpr <$> prefix <*> binaryExpr_3_2 <*> order
    binaryExpr_3_2 = (symbol "+")
      <|> (symbol "-")
    binaryExpr_4 = BinaryExpr <$> prefix <*> binaryExpr_4_2 <*> order
    binaryExpr_4_2 = (symbol "<<")
      <|> (symbol ">>")

{-
CompareExpr ::= Ap('CompareExpr', NonTerminal('prefix'),
         Choice(0, ">", "<", ">=", "<=", "<=>"), NonTerminal('order'))
-}
compareExpr = CompareExpr <$> prefix <*> compareExpr_2 <*> order
  where
    compareExpr_2 = (symbol ">")
      <|> (symbol "<")
      <|> (symbol ">=")
      <|> (symbol "<=")
      <|> (symbol "<=>")

{-
RangeExpr ::= Ap('RangeExpr', NonTerminal('prefix'),
         Choice(0, "..", "..!"), NonTerminal('order'))
-}
rangeExpr = RangeExpr <$> prefix <*> rangeExpr_2 <*> order
  where
    rangeExpr_2 = (symbol "..")
      <|> (symbol "..!")

{-
prim ::= Choice(
 0,
 NonTerminal('LiteralExpr'),
 NonTerminal('quasiliteral'),
 NonTerminal('NounExpr'),
 Brackets("(", NonTerminal('expr'), ")"),
 NonTerminal('HideExpr'),
 NonTerminal('MapComprehensionExpr'),
 NonTerminal('ListComprehensionExpr'),
 NonTerminal('MapExpr'),
 NonTerminal('ListExpr'))
-}
prim = literalExpr
  <|> quasiliteral
  <|> nounExpr
  <|> prim_4
  <|> hideExpr
-- @@  <|> mapComprehensionExpr
--   <|> listComprehensionExpr
  <|> mapExpr
  <|> listExpr
  where
    prim_4 = ((bra "(") *> expr <* (ket ")"))

{-
HideExpr ::= Ap('HideExpr',
   Brackets("{", SepBy(NonTerminal('expr'), ';', fun='wrapSequence'), "}"))
-}
hideExpr = HideExpr <$> hideExpr_1
  where
    hideExpr_1 = ((bra "{") *> hideExpr_1_2 <* (ket "}"))
    hideExpr_1_2 = (wrapSequence expr (symbol ";"))

{-
NounExpr ::= Ap('NounExpr', Choice(0, "IDENTIFIER", Sigil("::", ".String.")))
-}
nounExpr = NounExpr <$> nounExpr_1
  where
    nounExpr_1 = parseIdentifier
      <|> nounExpr_1_2
    nounExpr_1_2 = ((symbol "::") *> parseString)

{-
FinalPatt ::= Ap('FinalPatt', Choice(0, "IDENTIFIER", Sigil("::", ".String.")),
         NonTerminal('guardOpt'))
-}
finalPatt = FinalPatt <$> finalPatt_1 <*> guardOpt
  where
    finalPatt_1 = parseIdentifier
      <|> finalPatt_1_2
    finalPatt_1_2 = ((symbol "::") *> parseString)

{-
quasiliteral ::= Ap('QuasiParserExpr',
 Maybe(Terminal("IDENTIFIER")),
 Brackets('`',
 SepBy(
     Choice(0,
       Ap('Left', Terminal('QUASI_TEXT')),
       Ap('Right',
         Choice(0,
           Ap('NounExpr', Terminal('DOLLAR_IDENT')),
           Brackets('${', NonTerminal('expr'), '}'))))),
 '`'))
-}
quasiliteral = QuasiParserExpr <$> quasiliteral_1 <*> quasiliteral_2
  where
    quasiliteral_1 = optionMaybe parseIdentifier
    quasiliteral_2 = ((bra "`") *> quasiliteral_2_2 <* (ket "`"))
    quasiliteral_2_2 = (many0 quasiliteral_2_2_1_1)
    quasiliteral_2_2_1_1 = quasiliteral_2_2_1_1_1
      <|> quasiliteral_2_2_1_1_2
    quasiliteral_2_2_1_1_1 = Left <$> parseQuasiText
    quasiliteral_2_2_1_1_2 = Right <$> quasiliteral_2_2_1_1_2_1
    quasiliteral_2_2_1_1_2_1 = quasiliteral_2_2_1_1_2_1_1
      <|> quasiliteral_2_2_1_1_2_1_2
    quasiliteral_2_2_1_1_2_1_1 = NounExpr <$> parseDollarIdent
    quasiliteral_2_2_1_1_2_1_2 = ((bra "${") *> expr <* (ket "}"))

{-
IntExpr ::= Ap('IntExpr', Terminal(".int."))
-}
intExpr = IntExpr <$> parseint

{-
DoubleExpr ::= Ap('DoubleExpr', Terminal(".float64."))
-}
doubleExpr = DoubleExpr <$> parsefloat64

{-
CharExpr ::= Ap('CharExpr', Terminal(".char."))
-}
charExpr = CharExpr <$> parseChar

{-
StrExpr ::= Ap('StrExpr', Terminal(".String."))
-}
strExpr = StrExpr <$> parseString

{-
ListExpr ::= Ap('ListExpr', Brackets("[", SepBy(NonTerminal('expr'), ','), "]"))
-}
listExpr = ListExpr <$> listExpr_1
  where
    listExpr_1 = ((bra "[") *> listExpr_1_2 <* (ket "]"))
    listExpr_1_2 = (sepBy expr (symbol ","))

{-
MapExpr ::= Ap('MapExpr',
  Brackets("[",
           SepBy(Ap('pair', NonTerminal('expr'),
                            Sigil("=>", NonTerminal('expr'))),
                 ','),
           "]"))
-}
mapExpr = MapExpr <$> mapExpr_1
  where
    mapExpr_1 = ((bra "[") *> mapExpr_1_2 <* (ket "]"))
    mapExpr_1_2 = (sepBy mapExpr_1_2_1_1 (symbol ","))
    mapExpr_1_2_1_1 = pair <$> expr <*> mapExpr_1_2_1_1_2
    mapExpr_1_2_1_1_2 = ((symbol "=>") *> expr)

{-
LiteralExpr ::= Choice(0,
       NonTerminal('StrExpr'),
       NonTerminal('IntExpr'),
       NonTerminal('DoubleExpr'),
       NonTerminal('CharExpr'))
-}
literalExpr = strExpr
  <|> intExpr
  <|> doubleExpr
  <|> charExpr
-- #################################################


parseChar :: TokenParser Char
parseChar = item >>= \t -> case t of
  (TokChar c) -> return $ c
  _ -> failure

parseint :: TokenParser Integer
parseint = item >>= \t -> case t of
  (TokInt i) -> return $ i
  _ -> failure

parsefloat64 :: TokenParser Double
parsefloat64 = item >>= \t -> case t of
  (TokDouble d) -> return $ d
  _ -> failure

-- Isn't there a way to refactor parseString, parseIdentifier etc.?
parseString :: TokenParser String
parseString = item >>= \t -> case t of
  (TokString s) -> return $ s
  _ -> failure

parseIdentifier :: TokenParser String
parseIdentifier = item >>= \t -> case t of
  (TokIDENTIFIER s) -> return $ s
  _ -> failure

parseDollarIdent :: TokenParser String
parseDollarIdent = item >>= \t -> case t of
  (TokDOLLAR_IDENT s) -> return $ s
  _ -> failure

parseQuasiText :: TokenParser String
parseQuasiText = item >>= \t -> case t of
  (TokQUASI_TEXT s) -> return $ s
  _ -> failure

symbol :: String -> TokenParser String
symbol s = item >>= \t ->
  let
    v =  keywords ++ brackets ++ operators ++ punctuation
  in
    if elem (s, t) v then return s
    else failure


bracket :: Direction -> String -> TokenParser String
bracket dir sym = item >>= \t ->
  if elem (sym, t) (bracketsDir dir) then return sym
  else failure
bra :: String -> TokenParser String
bra = bracket Open
ket :: String -> TokenParser String
ket = bracket Close

parseSource :: String -> [Expr]
parseSource s = runParser expression tokens
  where
    tokens = offside $ lexer [] s

parseFile :: String -> IO ()
parseFile fileName = do
  code <- readFile fileName
  let e = parseSource code
  print e
