{-# LANGUAGE OverloadedStrings #-}

module Masque.Lexer where

import Data.Char
import Data.Map as M
import Data.Maybe(fromMaybe)

data Token
     = KW_as
     | KW_bind | KW_break
     | KW_catch | KW_continue
     | KW_def
     | KW_else | KW_escape | KW_exit | KW_extends | KW_exports
     | KW_finally | KW_fn | KW_for
     | KW_guards
     | KW_if | KW_implements | KW_imports | KW_in | KW_interface
     | KW_match | KW_meta | KW_method
     | KW_object
     | KW_pass | KW_pragma
     | KW_return
     | KW_switch
     | KW_to | KW_try
     | KW_var | KW_via
     | KW_when | KW_while
     | TokenIDENTIFIER String
     | TokenChar Char
        deriving (Show, Eq, Ord)

tag :: Token -> String
tag (TokenIDENTIFIER _) = "IDENTIFIER"
tag (TokenChar _) = ".char."
tag tok = case (M.lookup tok kw_encode) of
  (Just s) -> s
  Nothing -> error "???"


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | idStart c = lexId (c:cs)
  where
    lexId :: String -> [Token]
    lexId cs' =
      fromMaybe (TokenIDENTIFIER word) (M.lookup word kw_decode) : lexer rest
      where
        (word, rest) = span idPart cs'

-- lexer ('\'':cs) = charLiteral cs

idStart :: Char -> Bool
idStart '_' = True
idStart c | isAsciiUpper c = True
idStart c | isAsciiLower c = True
idStart _ = False

idPart :: Char -> Bool
idPart c | idStart c = True
idPart c | isDigit c = True
idPart _ = False

data Keyword

keywords :: [(String, Token)]
keywords =
  [("as", KW_as),
   ("bind", KW_bind), ("break", KW_break),
   ("catch", KW_catch), ("continue", KW_continue),
   ("def", KW_def),
   ("else", KW_else), ("escape", KW_escape), ("exit", KW_exit),
   ("extends", KW_extends), ("exports", KW_exports),
   ("finally", KW_finally), ("fn", KW_fn), ("for", KW_for),
   ("guards", KW_guards),
   ("if", KW_if), ("implements", KW_implements),
   ("imports", KW_imports), ("in", KW_in), ("interface", KW_interface),
   ("match", KW_match), ("meta", KW_meta), ("method", KW_method),
   ("object", KW_object),
   ("pass", KW_pass), ("pragma", KW_pragma),
   ("return", KW_return),
   ("switch", KW_switch),
   ("to", KW_to), ("try", KW_try),
   ("var", KW_var), ("via", KW_via),
   ("when", KW_when), ("while", KW_while)]

kw_decode :: M.Map String Token
kw_decode = fromList keywords

kw_encode :: M.Map Token String
kw_encode = fromList [(tok, s) | (s, tok) <- keywords]
