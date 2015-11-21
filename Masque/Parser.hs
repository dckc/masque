{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Masque.Parser where

import Control.Monad
import Control.Applicative (Applicative(..), Alternative(..),
                            (<$>), (<*), (*>))
import Data.Data
import Text.PrettyPrint.GenericPretty (Generic, Out)

import Masque.Lexer (Token(..),
                     lexer, offside,
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

{-
DefExpr ::= Sequence(Sigil('def', NonTerminal('pattern')),
         Optional(Sigil("exit", NonTerminal('order'))),
         Sigil(":=", NonTerminal('assign')))
-}
defExpr = DefExpr <$> defExpr_1 <*> defExpr_2 <*> defExpr_3
  where
    defExpr_1 = ((symbol "def") *> pattern)
    defExpr_2 = defExpr_2_1
      <|> defExpr_2_2
    defExpr_2_1 = return Nothing
    defExpr_2_2 = Just <$> ((symbol "exit") *> order)
    defExpr_3 = ((symbol ":=") *> assign)


pattern = finalPatt -- @@

guardOpt = return Nothing

{-
FinalPatt ::= Sequence(Choice(0, "IDENTIFIER", Sigil("::", ".String.")),
         NonTerminal('guardOpt'))
-}
finalPatt = FinalPatt <$> finalPatt_1 <*> guardOpt
  where
    finalPatt_1 = parseIdentifier
      <|> finalPatt_1_2
    finalPatt_1_2 = ((symbol "::") *> parseString)

assign :: TokenParser Expr
assign = order  -- @@


-- #################################################

{-
order ::= Choice(0,
  NonTerminal('BinaryExpr'),
  NonTerminal('RangeExpr'),
  NonTerminal('CompareExpr'),
  NonTerminal('prefix'))
-}
order = binaryExpr
  <|> rangeExpr
--  <|> compareExpr
  <|> prefix

prefix = prim -- @@

{-
BinaryExpr ::= Choice(0,
  Sequence(NonTerminal('prefix'),
           "**", NonTerminal('order')),
  Sequence(NonTerminal('prefix'),
           Choice(0, "*", "/", "//", "%"), NonTerminal('order')),
  Sequence(NonTerminal('prefix'),
           Choice(0, "+", "-"), NonTerminal('order')),
  Sequence(NonTerminal('prefix'),
           Choice(0, "<<", ">>"), NonTerminal('order')))
-}
binaryExpr = BinaryExpr <$> prefix <*> (symbol "**") <*> order
  <|> BinaryExpr <$> prefix <*> binaryExpr_2_2 <*> order
  <|> BinaryExpr <$> prefix <*> binaryExpr_3_2 <*> order
  <|> BinaryExpr <$> prefix <*> binaryExpr_4_2 <*> order
  where
    binaryExpr_2_2 = (symbol "*")
      <|> (symbol "/")
      <|> (symbol "//")
      <|> (symbol "%")
    binaryExpr_3_2 = (symbol "+")
      <|> (symbol "-")
    binaryExpr_4_2 = (symbol "<<")
      <|> (symbol ">>")

{-
RangeExpr ::= Sequence(NonTerminal('prefix'),
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
-- @@  <|> quasiliteral
  <|> nounExpr
  <|> prim_4
  <|> hideExpr
-- @@  <|> mapComprehensionExpr
-- @@  <|> listComprehensionExpr
  <|> mapExpr
  <|> listExpr
  where
    prim_4 = ((symbol "(") *> expr <* (symbol ")"))

{-
HideExpr ::= Brackets("{", SepBy(NonTerminal('expr'), ';', fun='wrapSequence'), "}")
-}
hideExpr = HideExpr <$> ((symbol "{") *> hideExpr_2 <* (symbol "}"))
  where
    hideExpr_2 = (wrapSequence expr (symbol ";"))

{-
NounExpr ::= Choice(0, "IDENTIFIER", Sigil("::", ".String."))
-}
nounExpr = NounExpr <$> parseIdentifier
  <|> NounExpr <$> nounExpr_2
  where
    nounExpr_2 = ((symbol "::") *> parseString)

{-
IntExpr ::= Sequence(Terminal(".int."))
-}
intExpr = IntExpr <$> parseint

{-
DoubleExpr ::= Sequence(Terminal(".float64."))
-}
doubleExpr = DoubleExpr <$> parsefloat64

{-
CharExpr ::= Sequence(Terminal(".char."))
-}
charExpr = CharExpr <$> parsechar

{-
StrExpr ::= Sequence(Terminal(".String."))
-}
strExpr = StrExpr <$> parseString

{-
ListExpr ::= Brackets("[", SepBy(NonTerminal('expr'), ','), "]")
-}
listExpr = ListExpr <$> ((symbol "[") *> listExpr_2 <* (symbol "]"))
  where
    listExpr_2 = (sepBy expr (symbol ","))

{-
MapExpr ::= Brackets("[",
         SepBy(Pair(NonTerminal('expr'), "=>", NonTerminal('expr')),
               ','),
         "]")
-}
mapExpr = MapExpr <$> ((symbol "[") *> mapExpr_2 <* (symbol "]"))
  where
    mapExpr_2 = (sepBy mapExpr_2_2_1 (symbol ","))
    mapExpr_2_2_1 = pair <$> expr <*> ((symbol "=>") *> expr)

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


parsechar :: TokenParser Char
parsechar = item >>= \t -> case t of
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

parseString :: TokenParser String
parseString = item >>= \t -> case t of
  (TokString s) -> return $ s
  _ -> failure

parseIdentifier :: TokenParser String
parseIdentifier = item >>= \t -> case t of
  (TokIDENTIFIER s) -> return $ s
  _ -> failure

symbol :: String -> TokenParser String
symbol s = item >>= \t ->
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
