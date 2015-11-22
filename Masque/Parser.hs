{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Masque.Parser where

import Control.Monad hiding (guard)
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
          | AssignExpr LValue Expr
          | VerbAssignExpr LValue Expr
          | BindingExpr String
          | MethodCallExpr Expr String [Expr] -- [NamedExpr]
          | SendExpr Expr String [Expr] -- [NamedExpr]
          | FunCallExpr Expr [Expr] -- [NamedExpr]
          | FunSendExpr Expr [Expr] -- [NamedExpr]
          | GetExpr Expr [Expr]
          | CurryExpr Expr Bool String
          | ListExpr [Expr]
          | MapExpr [(Expr, Expr)]
          | RangeExpr Expr String Expr  -- Stringly typed?
          | PrefixExpr String Expr  -- String?
          | SlotExpr String
          | CoerceExpr Expr Expr
          | BinaryExpr Expr String Expr  -- String?
          | CompareExpr Expr String Expr  -- String?
          | QuasiParserExpr (Maybe String) [Either String Expr]
          | DefExpr Patt (Maybe Expr) Expr
          | EscapeExpr Patt Expr (Maybe (Patt, Expr))
          | FinallyExpr Expr Expr
          | HideExpr Expr
          | IfExpr Expr Expr (Maybe Expr)
          | NounExpr String
          | ObjectExpr Patt [Patt] (Maybe Expr) Expr -- function expr
          -- | ObjectExpr Patt Expr [Expr] [Method] [Matcher] -- TODO: doco
          | SequenceExpr [Expr]
          | TryExpr Expr [(Patt, Expr)] (Maybe Expr)
          | Module [Patt] (Maybe [String]) Expr
          | ListComprehensionExpr Patt Expr (Maybe Expr) Expr
          | MapComprehensionExpr Patt Patt Expr (Maybe Expr) Expr
          | MetaStateExpr
          | MetaContextExpr
          | LambdaExpr [Patt] Expr
          | WhenExpr [Expr] Expr [(Patt, Expr)] (Maybe Expr)
          | WhileExpr Expr Expr (Maybe (Patt, Expr))
          | ForExpr Patt (Maybe Patt) Expr Expr (Maybe (Patt, Expr))
          | ForwardExpr String
          | SwitchExpr Expr [Matcher]
    deriving (Eq, Show, Read, Data, Typeable, Generic)

    -- either e[e1, e2, e3, ...] or name
type LValue = (Either (Expr, [Expr]) String)

data Matcher = Matcher Patt Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data Method = Method String String [Patt] [NamedPatt] Expr Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data NamedExpr = NamedExpr Expr Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data Patt = IgnorePatt (Maybe Expr)
          | BindingPatt String
          | FinalPatt String (Maybe Expr)
          | ListPatt [Patt] (Maybe Patt)
          | VarPatt String (Maybe Expr)
          | ViaPatt Expr Patt
          | SuchThatPatt Patt Expr
          | QuasiliteralPatt (Maybe String) [Either String Patt]
          | SamePatt Expr
          | NotSamePatt Expr
          | MapPatt [(Either Patt (Expr, Patt), Maybe Expr)] (Maybe Patt)
          | BindPatt String (Maybe Expr)
          | SlotPatt String (Maybe Expr)
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

nothing :: b -> (Maybe a)
nothing _ = Nothing

isArrow :: String -> Bool
isArrow s = s == "->"

passExpr :: a -> Expr
passExpr _ = SequenceExpr []

type Message = (Maybe String, [Expr]) -- TODO: namedexpr
type CallOrSend = Either Message Message
type CallSendIndex = Either [Expr] CallOrSend
type Curry = Either String String
callExpr :: Expr ->
            [CallSendIndex] ->
            (Maybe Curry) -> Expr
callExpr rx ts c = maybeCurry c (trailers rx ts)
  where
    trailers :: Expr -> [CallSendIndex] -> Expr
    trailers rx' [] = rx'

    trailers rx' ((Right (Right ((Just vb), args))):ts') =
      trailers (MethodCallExpr rx' vb args) ts'
    trailers rx' ((Right (Right (Nothing, args))):ts') =
      trailers (FunCallExpr rx' args) ts'

    trailers rx' ((Right (Left ((Just vb), args))):ts') =
      trailers (SendExpr rx' vb args) ts'
    trailers rx' ((Right (Left (Nothing, args))):ts') =
      trailers (FunSendExpr rx' args) ts'

    trailers rx' ((Left args):ts') =
      trailers (GetExpr rx' args) ts'

    maybeCurry :: (Maybe (Either String String)) -> Expr -> Expr
    maybeCurry (Just (Right verb)) rx' = CurryExpr rx' True verb
    maybeCurry (Just (Left verb)) rx' = CurryExpr rx' False verb
    maybeCurry Nothing rx' = rx'

-- #################################################

{-
guard ::= Choice(0,
  Ap('GetExpr',
     Ap('NounExpr', 'IDENTIFIER'),
     Brackets('[', SepBy(NonTerminal('expr'), ','), ']')),
  Ap('NounExpr', 'IDENTIFIER'),
  Brackets('(', NonTerminal('expr'), ')'))
-}
guard = guard_1
  <|> guard_2
  <|> guard_3
  where
    guard_1 = GetExpr <$> guard_1_1 <*> guard_1_2
    guard_1_1 = NounExpr <$> parseIdentifier
    guard_1_2 = ((bra "[") *> guard_1_2_2 <* (ket "]"))
    guard_1_2_2 = (sepBy expr (symbol ","))
    guard_2 = NounExpr <$> parseIdentifier
    guard_3 = ((bra "(") *> expr <* (ket ")"))

{-
guardOpt ::= Maybe(Sigil(':', NonTerminal('guard')))
-}
guardOpt = optionMaybe guardOpt_1
  where
    guardOpt_1 = ((symbol ":") *> guard )

{-
interface ::= Sequence(
 "interface",
 NonTerminal('namePattern'),
 Optional(Sequence("guards", NonTerminal('pattern'))),
 Optional(Sequence("extends", OneOrMore(NonTerminal('order'), ','))),
 Comment("implements_@@"), Comment("msgs@@"))
-}
interface = failure -- TODO

{-
FunctionExpr ::= Sequence('def', '(', ZeroOrMore(NonTerminal('pattern'), ','), ')',
  NonTerminal('block'))
-}
functionExpr = failure -- TODO

{-
ObjectExpr ::= Sequence(
 "object",
 Choice(0, Sequence("bind", NonTerminal('name')),
        "_",
        NonTerminal('name')),
 NonTerminal('guardOpt'), Comment("objectExpr"))
-}
objectExpr = failure -- TODO

{-
objectExpr2 ::= Sequence(
 Optional(Sequence('extends', NonTerminal('order'))),
 NonTerminal('auditors'),
 '{', ZeroOrMore(NonTerminal('objectScript'), ';'), '}')
-}
objectExpr2 = failure -- TODO

{-
objectScript ::= Sequence(
 Optional(NonTerminal('doco')),
 Choice(0, "pass", ZeroOrMore("@@meth")),
 Choice(0, "pass", ZeroOrMore(NonTerminal('matchers'))))
-}
objectScript = failure -- TODO

{-
doco ::= Terminal('.String')
-}
doco = failure -- TODO

{-
objectFunction ::= Ap('ObjectExpr',
  Sigil('def', NonTerminal('pattern')),
  Brackets("(", SepBy(NonTerminal("pattern"), ","), ")"),
  NonTerminal('guardOpt'),
  NonTerminal('block'))
-}
objectFunction = ObjectExpr <$> objectFunction_1 <*> objectFunction_2 <*> guardOpt <*> block
  where
    objectFunction_1 = ((symbol "def") *> pattern )
    objectFunction_2 = ((bra "(") *> objectFunction_2_2 <* (ket ")"))
    objectFunction_2_2 = (sepBy pattern (symbol ","))

{-
ForwardExpr ::= Ap('ForwardExpr', 'def', NonTerminal('FinalPattern'))
-}
forwardExpr = ForwardExpr <$> ((symbol "def") *> name)

{-
InterfaceExpr ::= Sequence('@@@@@')
-}
interfaceExpr = failure -- TODO

{-
IfExpr ::= Ap('IfExpr',
  Sigil("if", Brackets("(", NonTerminal('expr'), ")")),
  NonTerminal('block'),
  Maybe(
    Sigil("else",
     Choice(0,
       NonTerminal('IfExpr'),
       NonTerminal('block')))))
-}
ifExpr = IfExpr <$> ifExpr_1 <*> block <*> ifExpr_3
  where
    ifExpr_1 = ((symbol "if") *> ifExpr_1_2 )
    ifExpr_1_2 = ((bra "(") *> expr <* (ket ")"))
    ifExpr_3 = optionMaybe ifExpr_3_1
    ifExpr_3_1 = ((symbol "else") *> ifExpr_3_1_2 )
    ifExpr_3_1_2 = ifExpr
      <|> block

{-
ForExpr ::= Ap('ForExpr',
  Sigil("for", NonTerminal('pattern')),
  Maybe(Sigil("=>", NonTerminal('pattern'))),
  Sigil("in", NonTerminal('comp')),
  NonTerminal('block'),
  Maybe(NonTerminal('catcher')))
-}
forExpr = ForExpr <$> forExpr_1 <*> forExpr_2 <*> forExpr_3 <*> block <*> forExpr_5
  where
    forExpr_1 = ((symbol "for") *> pattern )
    forExpr_2 = optionMaybe forExpr_2_1
    forExpr_2_1 = ((symbol "=>") *> pattern )
    forExpr_3 = ((symbol "in") *> comp )
    forExpr_5 = optionMaybe catcher

{-
catcher ::= Sigil("catch", Ap('pair', NonTerminal('pattern'), NonTerminal('block')))
-}
catcher = ((symbol "catch") *> catcher_2 )
  where
    catcher_2 = pair <$> pattern <*> block

{-
WhileExpr ::= Ap('WhileExpr',
 Sigil("while", Brackets("(", NonTerminal('expr'), ")")),
 NonTerminal('block'),
 Maybe(NonTerminal('catcher')))
-}
whileExpr = WhileExpr <$> whileExpr_1 <*> block <*> whileExpr_3
  where
    whileExpr_1 = ((symbol "while") *> whileExpr_1_2 )
    whileExpr_1_2 = ((bra "(") *> expr <* (ket ")"))
    whileExpr_3 = optionMaybe catcher

{-
SwitchExpr ::= Sequence(
 "switch", Brackets("(", NonTerminal('expr'), ")"),
 Brackets("{", NonTerminal('matchers'), "}"))
-}
switchExpr = failure

{-
matchers ::= SepBy(
  Sigil("match", Ap('pair', NonTerminal('pattern'), NonTerminal('block'))))
-}
matchers = failure -- TODO

{-
EscapeExpr ::= Ap('EscapeExpr',
 Sigil("escape", NonTerminal('pattern')),
 NonTerminal('block'),
 Maybe(NonTerminal('catcher')))
-}
escapeExpr = EscapeExpr <$> escapeExpr_1 <*> block <*> escapeExpr_3
  where
    escapeExpr_1 = ((symbol "escape") *> pattern )
    escapeExpr_3 = optionMaybe catcher

{-
TryExpr ::= Ap('TryExpr',
 Sigil("try", NonTerminal('block')),
 SepBy(NonTerminal('catcher')),
 Maybe(Sigil("finally", NonTerminal('block'))))
-}
tryExpr = TryExpr <$> tryExpr_1 <*> tryExpr_2 <*> tryExpr_3
  where
    tryExpr_1 = ((symbol "try") *> block )
    tryExpr_2 = (many0 catcher)
    tryExpr_3 = optionMaybe tryExpr_3_1
    tryExpr_3_1 = ((symbol "finally") *> block )

{-
WhenExpr ::= Ap('WhenExpr',
  Sigil("when", Brackets("(", SepBy(NonTerminal('expr'), ','), ")")),
  Sigil("->", NonTerminal('block')),
  SepBy(NonTerminal('catcher')),
  Maybe(Sigil("finally", NonTerminal('block'))))
-}
whenExpr = WhenExpr <$> whenExpr_1 <*> whenExpr_2 <*> whenExpr_3 <*> whenExpr_4
  where
    whenExpr_1 = ((symbol "when") *> whenExpr_1_2 )
    whenExpr_1_2 = ((bra "(") *> whenExpr_1_2_2 <* (ket ")"))
    whenExpr_1_2_2 = (sepBy expr (symbol ","))
    whenExpr_2 = ((symbol "->") *> block )
    whenExpr_3 = (many0 catcher)
    whenExpr_4 = optionMaybe whenExpr_4_1
    whenExpr_4_1 = ((symbol "finally") *> block )

{-
LambdaExpr ::= Ap('LambdaExpr',
 Sigil("fn", SepBy(NonTerminal('pattern'), ',')),
 NonTerminal('block'))
-}
lambdaExpr = LambdaExpr <$> lambdaExpr_1 <*> block
  where
    lambdaExpr_1 = ((symbol "fn") *> lambdaExpr_1_2 )
    lambdaExpr_1_2 = (sepBy pattern (symbol ","))

{-
metaExpr ::= Sigil("meta", Sigil(".",
  Choice(0,
    Ap('MetaContextExpr', Sigil("context", Brackets("(", Skip(), ")"))),
    Ap('MetaStateExpr', Sigil("getState", Brackets("(", Skip(), ")"))))))
-}
metaExpr = ((symbol "meta") *> metaExpr_2 )
  where
    metaExpr_2 = ((symbol ".") *> metaExpr_2_2 )
    metaExpr_2_2 = metaExpr_2_2_1
      <|> metaExpr_2_2_2
    metaExpr_2_2_1 = return MetaContextExpr <$> metaExpr_2_2_1_1
    metaExpr_2_2_1_1 = ((symbol "context") *> metaExpr_2_2_1_1_2 )
    metaExpr_2_2_1_1_2 = ((bra "(") *> metaExpr_2_2_1_1_2_2 <* (ket ")"))
    metaExpr_2_2_1_1_2_2 = (return ())
    metaExpr_2_2_2 = return MetaStateExpr <$> metaExpr_2_2_2_1
    metaExpr_2_2_2_1 = ((symbol "getState") *> metaExpr_2_2_2_1_2 )
    metaExpr_2_2_2_1_2 = ((bra "(") *> metaExpr_2_2_2_1_2_2 <* (ket ")"))
    metaExpr_2_2_2_1_2_2 = (return ())

{-
block ::= Brackets("{",
 Choice(0,
   Ap('passExpr', Sigil("pass")),
   Ap('SequenceExpr',
     SepBy(
       Choice(0,
         NonTerminal('blockExpr'),
         NonTerminal('expr')),
       ";"))),
"}")
-}
block = ((bra "{") *> block_2 <* (ket "}"))
  where
    block_2 = block_2_1
      <|> block_2_2
    block_2_1 = passExpr <$> block_2_1_1
    block_2_1_1 = (symbol "pass")
    block_2_2 = SequenceExpr <$> block_2_2_1
    block_2_2_1 = (sepBy block_2_2_1_1_1 (symbol ";"))
    block_2_2_1_1_1 = blockExpr
      <|> expr

{-
blockExpr ::= Choice(
 0,
 NonTerminal('FunctionExpr'),
 NonTerminal('ObjectExpr'),
 NonTerminal('bind'),
 NonTerminal('InterfaceExpr'),
 NonTerminal('IfExpr'),
 NonTerminal('ForExpr'),
 NonTerminal('WhileExpr'),
 NonTerminal('SwitchExpr'),
 NonTerminal('EscapeExpr'),
 NonTerminal('TryExpr'),
 NonTerminal('WhenExpr'),
 NonTerminal('LambdaExpr'),
 NonTerminal('metaExpr'))
-}
blockExpr = functionExpr
  <|> objectExpr
-- @@  <|> bind
  <|> interfaceExpr
  <|> ifExpr
  <|> forExpr
  <|> whileExpr
  <|> switchExpr
  <|> escapeExpr
  <|> tryExpr
  <|> whenExpr
  <|> lambdaExpr
  <|> metaExpr

{-
expr ::= Choice(0,
 NonTerminal('assign'),
 NonTerminal('ExitExpr'))
-}
expr = assign
  <|> exitExpr

{-
ExitExpr ::= Ap('ExitExpr',
   Choice(0, "continue", "break", "return"),
   Choice(0, Ap('nothing', Brackets("(", Skip(), ")")),
   Ap('Just', NonTerminal('blockExpr'))))
-}
exitExpr = ExitExpr <$> exitExpr_1 <*> exitExpr_2
  where
    exitExpr_1 = (symbol "continue")
      <|> (symbol "break")
      <|> (symbol "return")
    exitExpr_2 = exitExpr_2_1
      <|> exitExpr_2_2
    exitExpr_2_1 = nothing <$> exitExpr_2_1_1
    exitExpr_2_1_1 = ((bra "(") *> exitExpr_2_1_1_2 <* (ket ")"))
    exitExpr_2_1_1_2 = (return ())
    exitExpr_2_2 = Just <$> blockExpr

{-
ListComprehensionExpr ::= Brackets("[",
  Ap('ListComprehensionExpr',
    Sigil("for", NonTerminal('pattern')),
    Sigil("in", Brackets("(", NonTerminal('order'), ")")),
    Maybe(Sigil("if", Brackets("(", NonTerminal('expr'), ")"))),
    NonTerminal('expr')),
  "]")
-}
listComprehensionExpr = ((bra "[") *> listComprehensionExpr_2 <* (ket "]"))
  where
    listComprehensionExpr_2 = ListComprehensionExpr <$> listComprehensionExpr_2_1 <*> listComprehensionExpr_2_2 <*> listComprehensionExpr_2_3 <*> expr
    listComprehensionExpr_2_1 = ((symbol "for") *> pattern )
    listComprehensionExpr_2_2 = ((symbol "in") *> listComprehensionExpr_2_2_2 )
    listComprehensionExpr_2_2_2 = ((bra "(") *> order <* (ket ")"))
    listComprehensionExpr_2_3 = optionMaybe listComprehensionExpr_2_3_1
    listComprehensionExpr_2_3_1 = ((symbol "if") *> listComprehensionExpr_2_3_1_2 )
    listComprehensionExpr_2_3_1_2 = ((bra "(") *> expr <* (ket ")"))

{-
MapComprehensionExpr ::= Brackets("[",
  Ap('ListComprehensionExpr',
    Sigil("for", NonTerminal('pattern')),
    Sigil("=>", NonTerminal('pattern')),
    Sigil("in", Brackets("(", NonTerminal('order'), ")")),
    Maybe(Sigil("if", Brackets("(", NonTerminal('expr'), ")"))),
    NonTerminal('expr')),
  "]")
-}

mapComprehensionExpr = ((bra "[") *> mapComprehensionExpr_2 <* (ket "]"))
  where
    mapComprehensionExpr_2 = MapComprehensionExpr <$> mapComprehensionExpr_2_1 <*> mapComprehensionExpr_2_2 <*> mapComprehensionExpr_2_3 <*> mapComprehensionExpr_2_4 <*> expr
    mapComprehensionExpr_2_1 = ((symbol "for") *> pattern )
    mapComprehensionExpr_2_2 = ((symbol "=>") *> pattern )
    mapComprehensionExpr_2_3 = ((symbol "in") *> mapComprehensionExpr_2_3_2 )
    mapComprehensionExpr_2_3_2 = ((bra "(") *> order <* (ket ")"))
    mapComprehensionExpr_2_4 = optionMaybe mapComprehensionExpr_2_4_1
    mapComprehensionExpr_2_4_1 = ((symbol "if") *> mapComprehensionExpr_2_4_1_2 )
    mapComprehensionExpr_2_4_1_2 = ((bra "(") *> expr <* (ket ")"))

{-
module_ ::= Ap('Module',
 Sigil("imports", SepBy(NonTerminal('namedPattern'))),
 Maybe(
   Sigil('exports', Brackets("(", SepBy(NonTerminal('name'), ","), ")"))),
 NonTerminal('block'))
-}
module_ = Module <$> module__1 <*> module__2 <*> block
  where
    module__1 = ((symbol "imports") *> module__1_2 )
    module__1_2 = (many0 namePatt)
    module__2 = optionMaybe module__2_1
    module__2_1 = ((symbol "exports") *> module__2_1_2 )
    module__2_1_2 = ((bra "(") *> module__2_1_2_2 <* (ket ")"))
    module__2_1_2_2 = (sepBy name (symbol ","))

{-
assign ::= Choice(0,
  Ap('DefExpr',
    Sigil("def", NonTerminal("pattern")),
    Maybe(Sigil("exit", NonTerminal("order"))),
    Sigil("::=", NonTerminal("assign"))),
 Ap('DefExpr',
   Choice(0, NonTerminal('VarPattern'), NonTerminal('BindPattern')),
   Ap('Nothing', Skip()),
   Sigil("::=", NonTerminal("assign"))),
 Ap('AssignExpr',
    NonTerminal('lval'),
    Sigil("::=", NonTerminal("assign"))),
 NonTerminal('VerbAssignExpr'),
 NonTerminal('order'))
-}
assign = assign_1
  <|> assign_2
  <|> assign_3
  <|> verbAssignExpr
  <|> order
  where
    assign_1 = DefExpr <$> assign_1_1 <*> assign_1_2 <*> assign_1_3
    assign_1_1 = ((symbol "def") *> pattern )
    assign_1_2 = optionMaybe assign_1_2_1
    assign_1_2_1 = ((symbol "exit") *> order )
    assign_1_3 = ((symbol "::=") *> assign )
    assign_2 = DefExpr <$> assign_2_1 <*> assign_2_2 <*> assign_2_3
    assign_2_1 = varPatt
      <|> bindPatt
    assign_2_2 = return Nothing <$> assign_2_2_1
    assign_2_2_1 = (return ())
    assign_2_3 = ((symbol "::=") *> assign )
    assign_3 = AssignExpr <$> lval <*> assign_3_2
    assign_3_2 = ((symbol "::=") *> assign )

{-
lval ::= Choice(0,
 Ap('Right', NonTerminal('name')),
 Ap('Left', NonTerminal('order'),
            Brackets("[", SepBy(NonTerminal('expr'), ','), "]")))
-}
lval = lval_1
  <|> lval_2
  where
    lval_1 = Right <$> name
    lval_2 = Left <$> lval_2x
    lval_2x = pair <$> order <*> lval_2_2
    lval_2_2 = ((bra "[") *> lval_2_2_2 <* (ket "]"))
    lval_2_2_2 = (sepBy expr (symbol ","))

{-
calls ::= Ap('callExpr',
    NonTerminal('prim'),
    SepBy(
      Choice(0,
        Ap('Right',
          Choice(0,
            Ap('Right', NonTerminal('call')),
            Ap('Left', NonTerminal('send')))),
        Ap('Left', NonTerminal('index')))),
    Maybe(NonTerminal('curryTail')))
-}
calls = callExpr <$> prim <*> calls_2 <*> calls_3
  where
    calls_2 = (many0 calls_2_1_1)
    calls_2_1_1 = calls_2_1_1_1
      <|> calls_2_1_1_2
    calls_2_1_1_1 = Right <$> calls_2_1_1_1_1
    calls_2_1_1_1_1 = calls_2_1_1_1_1_1
      <|> calls_2_1_1_1_1_2
    calls_2_1_1_1_1_1 = Right <$> call
    calls_2_1_1_1_1_2 = Left <$> send
    calls_2_1_1_2 = Left <$> index
    calls_3 = optionMaybe curryTail

{-
call ::= Ap('pair', Maybe(Sigil(".", NonTerminal('verb'))), NonTerminal('argList'))
-}
call :: TokenParser Message
call = pair <$> call_1 <*> argList
  where
    call_1 = optionMaybe call_1_1
    call_1_1 = ((symbol ".") *> verb )

{-
send ::= Sigil("<-", Ap('pair', Maybe(NonTerminal('verb')), NonTerminal('argList')))
-}
send :: TokenParser Message
send = ((symbol "<-") *> send_2 )
  where
    send_2 = pair <$> send_2_1 <*> argList
    send_2_1 = optionMaybe verb

{-
curryTail ::= Choice(0,
  Ap('Right', Sigil(".", NonTerminal('verb'))),
  Ap('Left', Sigil("<-", NonTerminal('verb'))))
-}
curryTail = curryTail_1
  <|> curryTail_2
  where
    curryTail_1 = Right <$> curryTail_1_1
    curryTail_1_1 = ((symbol ".") *> verb )
    curryTail_2 = Left <$> curryTail_2_1
    curryTail_2_1 = ((symbol "<-") *> verb )

{-
index ::= Brackets("[", SepBy(NonTerminal('expr'), ','), "]")
-}
index = ((bra "[") *> index_2 <* (ket "]"))
  where
    index_2 = (sepBy expr (symbol ","))

{-
verb ::= Choice(0, "IDENTIFIER", ".String.")
-}
verb = parseIdentifier
  <|> parseString

{-
argList ::= Brackets("(", SepBy(NonTerminal('expr'), ","), ")")
-}
argList = ((bra "(") *> argList_2 <* (ket ")"))
  where
    argList_2 = (sepBy expr (symbol ","))

{-
comp ::= Sequence(
 NonTerminal('order'),
 Optional(Sequence(Choice(
     0,
     Choice(0, "=~", "!~"),
     Choice(0, "==", "!="),
     "&!",
     Choice(0, "^", "&", "|")
 ), NonTerminal('comp'))))
-}
comp :: TokenParser Expr
comp = failure

{-
logical ::= Sequence(
 NonTerminal('comp'),
 Optional(Sequence(Choice(0, '||', '&&'), NonTerminal('logical'))))
-}
logical = failure

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
VerbAssignExpr ::= Ap('VerbAssignExpr',
   NonTerminal('lval'),
   Sigil("VERB_ASSIGN", NonTerminal("assign")))
-}
verbAssignExpr = VerbAssignExpr <$> lval <*> verbAssignExpr_2
  where
    verbAssignExpr_2 = ((symbol "VERB_ASSIGN") *> assign )

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
  <|> mapComprehensionExpr
  <|> listComprehensionExpr
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
NounExpr ::= Ap('NounExpr', NonTerminal('name'))
-}
nounExpr = NounExpr <$> name

{-
name ::= Choice(0, "IDENTIFIER", Sigil("::", ".String."))
-}
name = parseIdentifier
  <|> name_2
  where
    name_2 = ((symbol "::") *> parseString )

{-
prefix ::= Choice(
 0,
 Ap("PrefixExpr", '-', NonTerminal('prim')),
 Ap("PrefixExpr", Choice(0, "~", "!"), NonTerminal('call')),
 NonTerminal('SlotExpr'),
 NonTerminal('BindingExpr'),
 NonTerminal('CoerceExpr'),
 NonTerminal('call'))
-}
prefix = prefix_1
  <|> prefix_2
  <|> slotExpr
  <|> bindingExpr
  <|> coerceExpr
  <|> calls
  where
    prefix_1 = PrefixExpr <$> (symbol "-") <*> prim
    prefix_2 = PrefixExpr <$> prefix_2_1 <*> calls
    prefix_2_1 = (symbol "~")
      <|> (symbol "!")

{-
SlotExpr ::= Ap('SlotExpr', Sigil('&', NonTerminal('name')))
-}
slotExpr = SlotExpr <$> slotExpr_1
  where
    slotExpr_1 = ((symbol "&") *> name )

{-
BindingExpr ::= Ap('BindingExpr', Sigil('&&', NonTerminal('name')))
-}
bindingExpr = BindingExpr <$> bindingExpr_1
  where
    bindingExpr_1 = ((symbol "&&") *> name )

{-
CoerceExpr ::= Ap("CoerceExpr", NonTerminal('call'), Sigil(":", NonTerminal('guard')))
-}
coerceExpr = CoerceExpr <$> calls <*> coerceExpr_2
  where
    coerceExpr_2 = ((symbol ":") *> guard )

{-
pattern ::= Choice(0,
       NonTerminal('namePatt'),
       Choice(0,
         NonTerminal('SamePatt'),
         NonTerminal('NotSamePatt')),
       NonTerminal('QuasiliteralPatt'),
       NonTerminal('ViaPatt'),
       NonTerminal('IgnorePatt'),
       NonTerminal('ListPatt'),
       NonTerminal('MapPatt'),
       NonTerminal('SuchThatPatt'))
-}
pattern = namePatt
  <|> pattern_2
  <|> quasiliteralPatt
  <|> viaPatt
  <|> ignorePatt
  <|> listPatt
  <|> mapPatt
  <|> suchThatPatt
  where
    pattern_2 = samePatt
      <|> notSamePatt

{-
namePatt ::= Choice(0,
        NonTerminal('FinalPatt'),
        NonTerminal('VarPatt'),
        NonTerminal('BindPatt'),
        NonTerminal('SlotPatt'),
        NonTerminal('BindingPatt'))
-}
namePatt = finalPatt
  <|> varPatt
  <|> bindPatt
  <|> slotPatt
  <|> bindingPatt

{-
FinalPatt ::= Ap('FinalPatt', NonTerminal('name'), NonTerminal('guardOpt'))
-}
finalPatt = FinalPatt <$> name <*> guardOpt

{-
VarPatt ::= Ap('VarPatt', Sigil("var", NonTerminal('name')), NonTerminal('guardOpt'))
-}
varPatt = VarPatt <$> varPatt_1 <*> guardOpt
  where
    varPatt_1 = ((symbol "var") *> name )

{-
BindPatt ::= Ap('BindPatt', Sigil("bind", NonTerminal('name')), NonTerminal('guardOpt'))
-}
bindPatt = BindPatt <$> bindPatt_1 <*> guardOpt
  where
    bindPatt_1 = ((symbol "bind") *> name )

{-
SlotPatt ::= Ap('SlotPatt', Sigil("&", NonTerminal('name')), NonTerminal('guardOpt'))
-}
slotPatt = SlotPatt <$> slotPatt_1 <*> guardOpt
  where
    slotPatt_1 = ((symbol "&") *> name )

{-
BindingPattern ::= Ap('BindingPatt', Sigil("&&", NonTerminal('name')))
-}
bindingPatt = BindingPatt <$> bindingPattern_1
  where
    bindingPattern_1 = ((symbol "&&") *> name )

{-
IgnorePatt ::= Ap('IgnorePatt', Sigil("_", NonTerminal('guardOpt')))
-}
ignorePatt = IgnorePatt <$> ignorePatt_1
  where
    ignorePatt_1 = ((symbol "_") *> guardOpt )

{-
ListPatt ::= Ap('ListPatt',
  Brackets("[", SepBy(NonTerminal('pattern'), ','), ']'),
  Maybe(Sigil("+", NonTerminal('pattern'))))
-}
listPatt = ListPatt <$> listPatt_1 <*> listPatt_2
  where
    listPatt_1 = ((bra "[") *> listPatt_1_2 <* (ket "]"))
    listPatt_1_2 = (sepBy pattern (symbol ","))
    listPatt_2 = optionMaybe listPatt_2_1
    listPatt_2_1 = ((symbol "+") *> pattern )

{-
MapPatt ::= Ap('MapPatt',
  Brackets("[", SepBy(NonTerminal('mapPattItem'), ','), ']'),
  Maybe(Sigil("|", NonTerminal('pattern'))))
-}
mapPatt = MapPatt <$> mapPatt_1 <*> mapPatt_2
  where
    mapPatt_1 = ((bra "[") *> mapPatt_1_2 <* (ket "]"))
    mapPatt_1_2 = (sepBy mapPattItem (symbol ","))
    mapPatt_2 = optionMaybe mapPatt_2_1
    mapPatt_2_1 = ((symbol "|") *> pattern )

{-
mapPattItem ::= Ap('pair',
  Choice(0,
    Ap('Right', Ap('pair',
      Choice(0,
        NonTerminal('LiteralExpr'),
        Brackets("(", NonTerminal('expr'), ")")),
      Sigil("=>", NonTerminal('pattern')))),
    Ap('Left', Sigil("=>", NonTerminal('namePatt')))),
  Maybe(Sigil(":=", NonTerminal('order'))))
-}
mapPattItem = pair <$> mapPattItem_1 <*> mapPattItem_2
  where
    mapPattItem_1 = mapPattItem_1_1
      <|> mapPattItem_1_2
    mapPattItem_1_1 = Right <$> mapPattItem_1_1_1
    mapPattItem_1_1_1 = pair <$> mapPattItem_1_1_1_1 <*> mapPattItem_1_1_1_2
    mapPattItem_1_1_1_1 = literalExpr
      <|> mapPattItem_1_1_1_1_2
    mapPattItem_1_1_1_1_2 = ((bra "(") *> expr <* (ket ")"))
    mapPattItem_1_1_1_2 = ((symbol "=>") *> pattern )
    mapPattItem_1_2 = Left <$> mapPattItem_1_2_1
    mapPattItem_1_2_1 = ((symbol "=>") *> namePatt )
    mapPattItem_2 = optionMaybe mapPattItem_2_1
    mapPattItem_2_1 = ((symbol ":=") *> order )

{-
SamePatt ::= Ap('SamePatt', Sigil("==", NonTerminal('prim')))
-}
samePatt = SamePatt <$> samePatt_1
  where
    samePatt_1 = ((symbol "==") *> prim )

{-
NotSamePatt ::= Ap('NotSamePatt', Sigil("!=", NonTerminal('prim')))
-}
notSamePatt = NotSamePatt <$> notSamePatt_1
  where
    notSamePatt_1 = ((symbol "!=") *> prim )

{-
QuasiliteralPatt ::= Ap('QuasiliteralPatt',
 Maybe(Terminal("IDENTIFIER")),
 Brackets('`',
 SepBy(
     Choice(0,
       Ap('Left', Terminal('QUASI_TEXT')),
       Ap('Right',
         Choice(0,
           Ap('NounExpr', Terminal('AT_IDENT')),
           Brackets('@{', NonTerminal('expr'), '}'))))),
 '`'))
-}
quasiliteralPatt = QuasiliteralPatt <$> quasiliteralPatt_1 <*> quasiliteralPatt_2
  where
    quasiliteralPatt_1 = optionMaybe parseIdentifier
    quasiliteralPatt_2 = ((bra "`") *> quasiliteralPatt_2_2 <* (ket "`"))
    quasiliteralPatt_2_2 = (many0 quasiliteralPatt_2_2_1_1)
    quasiliteralPatt_2_2_1_1 = quasiliteralPatt_2_2_1_1_1
      <|> quasiliteralPatt_2_2_1_1_2
    quasiliteralPatt_2_2_1_1_1 = Left <$> parseQuasiText
    quasiliteralPatt_2_2_1_1_2 = Right <$> quasiliteralPatt_2_2_1_1_2_1
    quasiliteralPatt_2_2_1_1_2_1 = quasiliteralPatt_2_2_1_1_2_1_1
      <|> quasiliteralPatt_2_2_1_1_2_1_2
    quasiliteralPatt_2_2_1_1_2_1_1 = FinalPatt <$> parseAtIdent <*> return Nothing
    quasiliteralPatt_2_2_1_1_2_1_2 = ((bra "@{") *> pattern <* (ket "}"))

{-
ViaPatt ::= Ap('ViaPatt',
  Sigil("via", Brackets("(", NonTerminal('expr'), ')')),
  NonTerminal('pattern'))
-}
viaPatt = ViaPatt <$> viaPatt_1 <*> pattern
  where
    viaPatt_1 = ((symbol "via") *> viaPatt_1_2 )
    viaPatt_1_2 = ((bra "(") *> expr <* (ket ")"))

{-
SuchThatPatt ::= Ap('SuchThatPatt', NonTerminal('pattern'),
   Sigil("?", Brackets("(", NonTerminal('expr'), ")")))
-}
suchThatPatt = SuchThatPatt <$> pattern <*> suchThatPatt_2
  where
    suchThatPatt_2 = ((symbol "?") *> suchThatPatt_2_2 )
    suchThatPatt_2_2 = ((bra "(") *> expr <* (ket ")"))

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
    mapExpr_1_2_1_1_2 = ((symbol "=>") *> expr )

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

parseAtIdent :: TokenParser String
parseAtIdent = item >>= \t -> case t of
  (TokAT_IDENT s) -> return $ s
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
