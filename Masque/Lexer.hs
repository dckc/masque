{-# LANGUAGE OverloadedStrings #-}

module Masque.Lexer where

import Data.Char
import Data.List as L
import Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing, fromJust, maybeToList)
import Numeric (readHex)

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

     | TokQUASI Direction String

     | TokBracket Shape Direction

     | TokAssign
     | TokVERB_ASSIGN String

     | TokAdd | TokSubtract
     | TokShiftLeft | TokShiftRight
     | TokXor
     | TokMod
     | TokMultiply | TokFloorDivide | TokApproxDivide
     | TokPow
     | TokAnd | TokOr

     | TokComplement
     | TokRangeIncl | TokRangeExcl
     | TokAsBigAs | TokLEq | TokGEq | TokEq | TokNotEq
     | TokMatchBind | TokNotMatchBind
     | TokLogicalAnd | TokLogicalOr | TokButNot
                                       
     | TokSemi | TokComma | TokColon
     | TokStringNoun
     | TokSuchThat | TokIgnore
     | TokCall | TokSend
     | TokArrow | TokFatArrow

     deriving (Show, Eq, Ord)

data Shape
     = Round | Square | Curly
     | Dollar  -- ${ }
     | At      -- @{ }
     deriving (Show, Eq, Ord)

data Direction
     = Open | Close
     deriving (Show, Eq, Ord)


-- TODO: monadic lexer for error handling?
-- TODO: line, column number tracking
-- TODO: better error reporting, a la monte_lexer.mt?
-- TODO: tests for '\<newline>x'
-- TODO: optimize String to Data.ByteString.Lazy.UTF8?
lexer :: String -> [Token]
lexer [] = []

lexer ('#':cs) = lexer rest -- Comments
  where
    (_comment, nl_rest) = span (not . (==) '\n') cs
    rest = drop 1 nl_rest  -- drop newline

lexer (sp:cs) | isSpace sp = lexer cs  -- TODO: INDENT / DEDENT

lexer (c:cs) | idStart c = lexId (c:cs)
  where
    lexId :: String -> [Token]
    lexId cs' = tok : lexer rest''
      where
        (word, rest) = span idPart cs'
        try_kw = M.lookup (L.map toLower word) $ decode keywords
        -- e.g. with=
        try_verb_assign = case (word, rest) of
          ("_", _) -> Nothing
          (_, '=':bad:_)
            | elem bad ['=', '>', '~'] -> Nothing
          (_, '=':rest')
            | isNothing try_kw -> Just (TokVERB_ASSIGN $ word ++ ['='], rest')
          _ -> Nothing

               -- ugh... clean up isJust / fromJust
        (tok, rest'') =
          if isJust try_verb_assign then fromJust try_verb_assign
          else (let tok_default =
                      if word == "_" then TokIgnore
                      else (TokIDENTIFIER word)
                in fromMaybe tok_default try_kw,
                rest)

lexer (d:cs) | isDigit d = lexNum (d:cs)
  where
    lexNum ('0':x:h:cs')
      | elem x ['X', 'x'] && isHexDigit h =
          (TokInt $ decodeHex digits) : lexer rest
          where
            (digits, rest) = span isHexDigit (h:cs')
            decodeHex s = toEnum $ fst $ head (readHex s)
    lexNum cs' =
      let
        (whole, more) = span isDigit cs'
        try_frac :: Maybe (String, String)
        try_frac = case more of
          ('.':d':cs'')
            | isDigit d' -> Just $ ('.':frac, rest)
            where
              (frac, rest) = span isDigit (d':cs'')
          _ -> Nothing
        try_expn = case try_frac of
          (Just (_dot:_digit:_, (e:s:cs'')))
            | elem e ['E', 'e'] -> Just $ (sign ++ expn, rest)
            where
              (sign, expn_rest) = if elem s ['+', '-']
                                  then ([s], cs'') else ([], s:cs'')
              (expn, rest) = span isDigit expn_rest
          _ -> Nothing
        try_double :: Maybe [Token]
        try_double = case (try_frac, try_expn) of
          ((Just (dot_frac, _)), (Just (sign_expn, rest))) ->
            Just $ (TokDouble $ read numeral) : lexer rest
            where
              numeral = whole ++ dot_frac ++ sign_expn
          ((Just (dot_frac, rest)), Nothing) ->
            Just $ (TokDouble $ read numeral) : lexer rest
            where
              numeral = whole ++ dot_frac
          _ -> Nothing
        else_tok_int = TokInt $ read whole
      in
        fromMaybe (else_tok_int : lexer more) try_double

lexer ('"':cs) = (TokString chars) : lexer rest
  where
    (chars, quot_rest) = span (not . (==) '"') cs
    rest = drop 1 quot_rest  -- drop closing "

lexer ('\'':cs) = charLiteral cs
  where
    charLiteral ('\\':cs') = loop $ charConstant cs'
      where
        loop :: (Maybe Char, String) -> [Token]
        loop (Nothing, more) = loop $ charConstant more
        loop (Just ch, more) = (TokChar ch) : lexer more
    charLiteral (ch:'\'':cs') = (TokChar ch) : lexer cs'
    charLiteral _ = error "bad character literal"

lexer ('`':cs) = quasiPart cs
  where
    quasiPart cs' = loop [] cs' []
    loop :: String -> String -> [Token] -> [Token]
    loop _ [] _ = error "File ends inside quasiliteral"
    loop buf ('`':rest) parts = parts ++ [(TokQUASI Close buf)] ++ lexer rest
    loop buf ('@':'@':more) parts = loop (buf ++ ['@']) more parts
    loop buf ('$':'$':more) parts = loop (buf ++ ['$']) more parts
    loop buf ('$':'\\':more) parts = loop (buf ++ chars) after parts
      where
        (char01, after) = charConstant more
        chars = maybeToList char01

    loop buf (sigil:p:cs') parts
      | elem sigil ['$', '@'] && idPart p = lexId (p:cs')
      where
        lexId :: String -> [Token]
        lexId cs'' = loop [] rest ((TokQUASI Open buf):tok:parts)
          where
            (word, rest) = span idPart cs''
            tok = case M.lookup (L.map toLower word) $ decode keywords of
              Just _ -> error $ word ++ "is a keyword"
              Nothing -> if sigil == '@' then TokAT_IDENT word
                         else TokDOLLAR_IDENT word
    loop buf (ch:more) parts = loop (buf ++ [ch]) more parts

-- @@isJust/fromJust is a kludge. how to do this right?
lexer (s1:s2:s3:cs)
  | isJust $ try_sym = fromJust try_sym : lexer cs
  where try_sym = symbol_decode (s1:s2:s3:[])
lexer (s1:s2:cs)
  | isJust $ try_sym = fromJust try_sym : lexer cs
  where try_sym = symbol_decode (s1:s2:[])
lexer (s1:cs)
  | isJust $ try_sym = fromJust try_sym : lexer cs
  where try_sym = symbol_decode (s1:[])

lexer _ = undefined -- TODO


idStart :: Char -> Bool
idStart '_' = True
idStart c | isAsciiUpper c = True
idStart c | isAsciiLower c = True
idStart _ = False

idPart :: Char -> Bool
idPart c | idStart c = True
idPart c | isDigit c = True
idPart _ = False

charConstant :: String -> (Maybe Char, String)
charConstant more = charEscape more
  where
    charEscape [] = error "End of input in middle of literal"
    charEscape ('\n':cs) = (Nothing, cs)
    charEscape ('U':h1:h2:h3:h4:h5:h6:h7:h8:cs)
      | (all isHexDigit $ h1:h2:h3:h4:h5:h6:h7:h8:[]) =
        (Just $ decodeHex $ h1:h2:h3:h4:h5:h6:h7:h8:[], cs)
    charEscape ('u':h1:h2:h3:h4:cs)
      | (all isHexDigit $ h1:h2:h3:h4:[]) =
          (Just $ decodeHex $ h1:h2:h3:h4:[], cs)
    charEscape ('x':h1:h2:cs)
      | (all isHexDigit $ h1:h2:[]) =
          (Just $ decodeHex $ h1:h2:[], cs)
    charEscape (esc:cs) = case M.lookup esc esc_decode of
      (Just ch) -> (Just ch, cs)
      Nothing -> error "bad escape in character literal"
    esc_decode = M.fromList [
      ('b', '\b'),
      ('t', '\t'),
      ('n', '\n'),
      ('f', '\f'),
      ('r', '\r'),
      ('"', '"'),
      ('\'', '\''),
      ('\\', '\\')]
    decodeHex s = toEnum $ fst $ head (readHex s)


tag :: Token -> String
tag (TokIDENTIFIER _) = "IDENTIFIER"
tag (TokAT_IDENT _) = "AT_IDENT"
tag (TokDOLLAR_IDENT _) = "DOLLAR_IDENT"
tag (TokQUASI Open _) = "QUASI_OPEN"
tag (TokQUASI Close _) = "QUASI_CLOSE"
tag (TokChar _) = ".char."
tag (TokString _) = ".String."
tag (TokInt _) = ".int."
tag (TokDouble _) = ".float64."
tag (TokVERB_ASSIGN op) =
  case M.lookup op opmap of
  (Just sym) -> sym
  Nothing -> "VERB_ASSIGN"
  where
    opmap = fromList $ [(verb, sym) | ((sym, verb), _) <- assign_ops]
tag tok = case (M.lookup tok $ encode vocabulary) of
  (Just s) -> s
  Nothing -> error "missing tag for" ++ (show tok)
  where
    vocabulary = keywords ++ punctuation ++ operators ++ brackets


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

brackets :: [(String, Token)]
brackets = [
  ("(", TokBracket Round Open),
  (")", TokBracket Round Close),
  ("[", TokBracket Square Open),
  ("]", TokBracket Square Close),
  ("${", TokBracket Dollar Open),
  ("@{", TokBracket At Open),
  ("{", TokBracket Curly Open),
  ("}", TokBracket Curly Close)]

assign_ops :: [((String, String), (String, Token))]
assign_ops = [
  (("^=", "xor"), ("^", TokXor)),
  (("+=", "add"), ("+", TokAdd)),
  (("-=", "subtract"), ("-", TokSubtract)),
  (("<<=", "shiftLeft"), ("<<", TokShiftLeft)),
  ((">>=", "shiftRight"), ("<<", TokShiftRight)),
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
  ("<=", TokLEq),
  (">=", TokGEq),
  ("==", TokEq),
  ("!=", TokNotEq),
  ("=~", TokMatchBind),
  ("!~", TokNotMatchBind),
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
  ("?", TokSuchThat), (")", TokIgnore),
  (".", TokCall),
  ("<-", TokSend),
  ("->", TokArrow), ("=>", TokFatArrow)]

decode :: [(String, Token)] -> M.Map String Token
decode vocab = fromList vocab

encode :: [(String, Token)] -> M.Map Token String
encode vocab = fromList [(tok, s) | (s, tok) <- vocab]

symbol_decode :: String -> Maybe Token
symbol_decode chars =
  M.lookup chars $ decode symbols
  where symbols = punctuation ++ operators ++ brackets
