module Masque.ParseUtil where

import Control.Monad.Identity
import qualified Data.Map as M
import Numeric (readHex)
import qualified Text.Parsec as P
import qualified Text.Parsec.IndentParsec.Prim as IP

import Masque.FullSyntax(Expr(..))

type MonteLex a = IP.IndentParsecT String [Char] Identity a

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
