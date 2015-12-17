{-# LANGUAGE OverloadedStrings #-}

module Masque.Lexer where

import Data.Char

-- TODO: get rid of the Token data type
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

     | TokIDENTIFIER String
     | TokDOLLAR_IDENT String
     | TokAT_IDENT String

     | TokChar Char
     | TokInt Integer
     | TokDouble Double
     | TokString String

     | TokQUASI_TEXT String

     | TokBracket Shape Direction
     | TokDollarBracket Shape Direction
     | TokAtBracket Shape Direction
 
     | TokAssign
     | TokVERB_ASSIGN String

     | TokAdd | TokSubtract
     | TokShiftLeft | TokShiftRight
     | TokXor
     | TokMod
     | TokMultiply | TokFloorDivide | TokApproxDivide
     | TokPow
     | TokAnd | TokOr

     | TokComplement | TokNot
     | TokRangeIncl | TokRangeExcl
     | TokAsBigAs | TokLT | TokGT | TokLEq | TokGEq | TokEq | TokNotEq
     | TokMatchBind | TokNotMatchBind
     | TokLogicalAnd | TokLogicalOr | TokButNot
                                       
     | TokSemi | TokComma | TokColon
     | TokStringNoun
     | TokSuchThat | TokIgnore
     | TokCall | TokSend
     | TokArrow | TokFatArrow

     | TokComment String
     | TokNewLine Int
     deriving (Show, Eq, Ord)

data Shape
     = Round   -- ( )
     | Square  -- [ ]
     | Curly   -- { }
     | Quasi   -- ` `
     | Hole    -- ${ }, @{ } $id, @id
     deriving (Show, Eq, Ord, Enum, Bounded)

data Direction
     = Open | Close
     deriving (Show, Eq, Ord, Enum, Bounded)


idStart :: Char -> Bool
idStart c = isAsciiUpper c || isAsciiLower c || c == '_'

idPart :: Char -> Bool
idPart c = idStart c || isDigit c || c == '_'

-- TODO: test case-folding of keywords

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

literals :: [(String, Token)]
literals = [
  (".char.", (TokChar 'a')),
  (".int.", (TokInt 1)),
  (".float64.", (TokDouble 1.0)),
  (".String.", (TokString ""))]

quasiParts :: [(String, Token)]
quasiParts = [
  ("QUASI_TEXT", (TokQUASI_TEXT ""))]

identifiers :: [(String, Token)]
identifiers = [
  ("IDENTIFIER", (TokIDENTIFIER "a")),
  ("AT_IDENT", (TokAT_IDENT "a")),
  ("DOLLAR_IDENT", (TokDOLLAR_IDENT "a"))]

brackets :: [(String, Token)]
brackets = [
  ("(", TokBracket Round Open),
  (")", TokBracket Round Close),
  ("[", TokBracket Square Open),
  ("]", TokBracket Square Close),
  ("`", TokBracket Quasi Open),
  ("`", TokBracket Quasi Close),
  ("${", TokDollarBracket Curly Open),
  ("@{", TokAtBracket Curly Open),
  ("{", TokBracket Curly Open),
  ("}", TokBracket Curly Close)]

assign_ops :: [((String, String), (String, Token))]
assign_ops = [
  (("^=", "xor"), ("^", TokXor)),
  (("+=", "add"), ("+", TokAdd)),
  (("-=", "subtract"), ("-", TokSubtract)),
  (("<<=", "shiftLeft"), ("<<", TokShiftLeft)),
  ((">>=", "shiftRight"), (">>", TokShiftRight)),
  (("**=", "pow"), ("**", TokPow)),
  (("*=", "multiply"), ("*", TokMultiply)),
  (("//=", "floorDivide"), ("//", TokFloorDivide)),
  (("/=", "approxDivide"), ("/", TokApproxDivide)),
  (("%=", "mod"), ("%", TokMod)),
  (("&=", "and"), ("&", TokAnd)),
  (("|=", "or"), ("|", TokOr))
  ]

operators :: [(String, Token)]
operators = [
  ("~", TokComplement),
  ("..", TokRangeIncl),
  ("..!", TokRangeExcl),
  (":=", TokAssign),
  ("<=>", TokAsBigAs),
  ("<", TokLT),
  (">", TokLT),
  ("<=", TokLEq),
  (">=", TokGEq),
  ("==", TokEq),
  ("!=", TokNotEq),
  ("=~", TokMatchBind),
  ("!~", TokNotMatchBind),
  ("!", TokNot),
  ("&&", TokLogicalAnd),
  ("||", TokLogicalOr),
  ("&!", TokButNot)
  ] ++ concat aops
  where
    aops = [[(sym, TokVERB_ASSIGN verb), op] | ((sym, verb), op) <- assign_ops]

punctuation :: [(String, Token)]
punctuation = [
  (";", TokSemi), (",", TokComma), (":", TokColon),
  ("::", TokStringNoun),
  ("?", TokSuchThat), ("_", TokIgnore),
  (".", TokCall),
  ("<-", TokSend),
  ("->", TokArrow), ("=>", TokFatArrow)]
