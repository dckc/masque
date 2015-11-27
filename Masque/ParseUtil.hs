module Masque.ParseUtil where

import Control.Applicative (Applicative(..), Alternative(..),
                            (<$>), (<*), (*>))

import Masque.Parsing (Parser(..),
                       item, failure, optionMaybe, many1)
import Masque.Lexer (Token(..), Direction(..),
                     brackets, bracketsDir, punctuation, operators, keywords)
import Masque.FullSyntax(Expr(..))

type TokenParser = Parser Token

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

sepBy1 :: TokenParser o -> TokenParser x -> TokenParser [o]
sepBy1 p sep = some_p
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
