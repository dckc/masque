{-# LANGUAGE OverloadedStrings #-}

module Masque.Lexer where

import Control.Applicative ((<|>))
import Data.Char
import Data.List as L
import Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
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


offside :: [Token] -> [Token]
offside toks = track [] $ L.filter notComment toks
  where
    notComment tok = case tok of
      (TokComment _) -> False
      _ -> True
      
    track [] [] = []
    track pending [] = loop pending
      where
        loop [] = []
        loop (_:stack') = (TokBracket Curly Close):loop stack'
    
    track stack ((TokNewLine 0):toks') = (TokSemi) : track stack toks'

    track [] (TokColon:(TokNewLine qty):toks') =
      (TokBracket Curly Open):track (qty:[]) toks'

    track (cur:stack) (TokColon:(TokNewLine indent):toks')
      | indent > cur
        = (TokBracket Curly Open):track (indent:cur:stack) toks'
  
    track (cur:stack) ((TokNewLine dedent):toks')
      | dedent < cur
        = (TokBracket Curly Close) : loop stack
      where
        loop (cur':stack')
          | dedent < cur'
            = (TokBracket Curly Close) : loop stack'
        loop stack' = track stack' toks'
  
    -- TODO: TokArrow

    track stack (tok:toks') = tok:track stack toks'

-- TODO: monadic lexer for error handling?
-- TODO: line, column number tracking
-- TODO: better error reporting, a la monte_lexer.mt?
-- TODO: tests for '\<newline>x'
-- TODO: optimize String to Data.ByteString.Lazy.UTF8?
lexer :: [Shape] -> String -> [Token]
lexer [] [] = []
lexer kets [] = error $ "EOF with brackets pending: " ++ (show kets)

lexer (Quasi:kets) cs = quasiPart kets cs

lexer kets (sigil:p:cs)
  | elem sigil ['$', '@'] && idPart p = lexId (p:cs)
  where
    lexId :: String -> [Token]
    lexId cs' =
      let (idkw, rest) = idOrKeyword cs'
          kets' = case kets of
            Hole:kets'' -> kets''
            _ -> kets
      in
      case idkw of
      (Right word) ->
        (if sigil == '@' then TokAT_IDENT word
         else TokDOLLAR_IDENT word) : lexer kets' rest
      (Left (kw, _)) -> error (kw ++ " is a keyword")

lexer kets (c:cs) | idStart c = lexId (c:cs)
  where
    lexId :: String -> [Token]
    lexId cs' = case idOrKeyword cs' of
      (Left (_, kw), rest) -> kw : lexer kets rest
      (Right "_", rest) -> TokIgnore : lexer kets rest
      (Right word, '=':check:rest)
        | elem check ['=', '>', '~']
          -> (TokIDENTIFIER word) : lexer kets ('=':check:rest)
      (Right word, '=':rest)
        -> (TokVERB_ASSIGN $ word) : lexer kets rest
      (Right word, rest)
        -> (TokIDENTIFIER word) : lexer kets rest

lexer kets ('#':cs) = (TokComment comment) : lexer kets rest
  where
    (comment, rest) = span (not . (==) '\n') cs

lexer kets ('\n':cs) = (TokNewLine $ length s) : lexer kets rest
  where 
    (s, rest) = span ((==) ' ') cs

lexer kets (' ':cs) = lexer kets cs -- space between tokens on a line (TODO)

lexer kets (d:cs) | isDigit d = numLit kets (d:cs)

lexer kets ('"':cs) = (TokString chars) : lexer kets rest
  where
    (chars, quot_rest) = span (not . (==) '"') cs
    rest = drop 1 quot_rest  -- drop closing "

lexer kets ('\'':cs) = charLiteral cs
  where
    charLiteral ('\\':cs') = loop $ charConstant cs'
      where
        loop :: (Maybe Char, String) -> [Token]
        loop (Nothing, more) = loop $ charConstant more
        loop (Just ch, more) = (TokChar ch) : lexer kets more
    charLiteral (ch:'\'':cs') = (TokChar ch) : lexer kets cs'
    charLiteral _ = error "bad character literal"

lexer kets (s1:chars) = lexSym s1 chars
  where
    symbolTokens = punctuation ++ operators ++ brackets
    symbols = L.map fst symbolTokens

    lexSym :: Char -> String -> [Token]
    lexSym '`' _ = (TokBracket Quasi Open) : lexer (Quasi:kets) chars
    lexSym _ (s2:s3:cs)
      | elem (s1:s2:s3:[]) symbols = go kets (s1:s2:s3:[]) cs
    lexSym _ (s2:cs)
      | elem (s1:s2:[]) symbols = go kets (s1:s2:[]) cs
    lexSym _ cs
      | elem (s1:[]) symbols = go kets (s1:[]) cs

    lexSym _ cs = error $ "syntax error? " ++ show (kets, s1:cs)

    go :: [Shape] -> String -> String -> [Token]
    go (shape:kets') [close] cs
      | close == closeBracket shape
        = (TokBracket shape Close) : lexer (drop hole kets') cs
      where hole = case (close, kets') of
              ('}', Hole:_) -> 1
              _ -> 0
    go _ [close] _cs
      | elem close ['}', ']', ')']
        = error $ "unexpected close bracket: " ++ show (close, kets)
    go _ sym rest = tok : lexer (shapes ++ kets) rest
      where
        (tok, shapes) = openBrackets $ M.lookup sym $ decode symbolTokens


idStart :: Char -> Bool
idStart c = isAsciiUpper c || isAsciiLower c || c == '_'

idPart :: Char -> Bool
idPart c = idStart c || isDigit c || c == '_'

idOrKeyword :: String -> (Either (String, Token) String, String)
idOrKeyword cs =
  let (word, rest) = span idPart cs in
  case M.lookup (L.map toLower word) $ decode keywords of
  (Just tok) -> (Left (word, tok), rest)
  Nothing -> (Right word, rest)


quasiPart :: [Shape] -> String -> [Token]
quasiPart kets cs = loop [] cs
  where
    loop :: String -> String -> [Token]
    loop _ [] = error "File ends inside quasiliteral"

    -- stuff that doesn't start with @ or $ passes through
    loop buf (ch:more)
      | not $ elem ch ['@', '$', '`']
      = loop (buf ++ [ch]) more

    -- $$ @@ `` are escapes for @, $, 1
    loop buf (sig1:sig2:more)
      | elem sig1 ['@', '$', '`'] && sig2 == sig1
        = loop (buf ++ [sig1]) more

    -- $\x01 and such
    loop buf ('$':'\\':more) = loop (buf ++ chars) after
      where
        (char01, after) = charConstant more
        chars = maybeToList char01

    loop buf ('`':rest)
      = quasiText buf ((TokBracket Quasi Close) : lexer kets rest)

    loop buf rest -- $ or @
      = quasiText buf (lexer (Hole:Quasi:kets) rest)
    quasiText "" rest = rest
    quasiText buf rest = (TokQUASI_TEXT buf) : rest


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

numLit :: [Shape] -> String -> [Token]
numLit kets cs = lexNum cs
  where
    lexNum ('0':x:h:cs')
      | elem x ['X', 'x'] && isHexDigit h =
          (TokInt $ decodeHex digits) : lexer kets rest
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
               -- TODO: 3e-2 is 0.03
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
            Just $ (TokDouble $ read numeral) : lexer kets rest
            where
              numeral = whole ++ dot_frac ++ sign_expn
          ((Just (dot_frac, rest)), Nothing) ->
            Just $ (TokDouble $ read numeral) : lexer kets rest
            where
              numeral = whole ++ dot_frac
          _ -> Nothing
        else_tok_int = TokInt $ read whole
      in
        fromMaybe (else_tok_int : lexer kets more) try_double


unlex :: Token -> String
unlex tok = fromMaybe ".???." $ space <|> ident <|> literal <|> simple
  where
    space = case tok of
      (TokBracket Curly Open) -> Just $ "{ "
      (TokBracket Curly Close) -> Just $ "} "
      (TokNewLine qty) -> Just $ "\n" ++ (replicate qty ' ')
      (TokComment s) -> Just $ "#" ++ s
      _ -> Nothing
    ident = case tok of
      (TokIDENTIFIER i)   -> Just $ i ++ " "
      (TokDOLLAR_IDENT i) -> Just $ "$" ++ i ++ " "
      (TokAT_IDENT i)     -> Just $ "@" ++ i ++ " "
      (TokVERB_ASSIGN v)  -> Just $ v ++ "= "
      _ -> Nothing
    literal = case tok of
      (TokChar '\'') -> Just "'\\'' "
      (TokChar c) -> Just $ "'" ++ [c] ++ "' "
      (TokInt i) -> Just $ (show i) ++ " "
      (TokDouble d) -> Just $ (show d) ++ " "
      (TokString s) | not $ elem '"' s -> Just $ "\"" ++ s ++ "\" "
      (TokString s) -> Just $ "`" ++ s ++ "` "  -- TODO: finish
      (TokQUASI_TEXT s) -> Just $ s  -- TODO: escaping.
      _ -> Nothing
    symbols = keywords ++ brackets ++ operators ++ punctuation
    simple = fmap (\s -> s ++ " ") (M.lookup tok $ encode symbols)

tag :: Token -> String
tag (TokIDENTIFIER _) = "IDENTIFIER"
tag (TokAT_IDENT _) = "AT_IDENT"
tag (TokDOLLAR_IDENT _) = "DOLLAR_IDENT"
tag (TokQUASI_TEXT _) = "QUASI_TEXT"
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
-- TODO: test/finish tag?


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

bracketsDir :: Direction -> [(String, Token)]
bracketsDir dir = L.filter rightSide brackets
  where
    rightSide symtk = case symtk of
      (_, (TokBracket _ dir')) | dir' == dir -> True
      (_, (TokDollarBracket _ dir')) | dir' == dir -> True
      (_, (TokAtBracket _ dir')) | dir' == dir -> True
      _ -> False


closeBracket :: Shape -> Char
closeBracket Curly = '}'
closeBracket Square = ']'
closeBracket Round = ')'
closeBracket _ = '*'

openBrackets :: Maybe Token -> (Token, [Shape])
openBrackets (Just (TokBracket shape Open))
  = ((TokBracket shape Open), [shape])
openBrackets (Just (TokDollarBracket shape Open))
  = ((TokDollarBracket shape Open), [shape])
openBrackets (Just (TokAtBracket shape Open))
  = ((TokAtBracket shape Open), [shape])
openBrackets (Just tok) = (tok, [])
openBrackets _ = error "don't call openBrackets with Nothing"

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

decode :: [(String, Token)] -> M.Map String Token
decode vocab = fromList vocab

encode :: [(String, Token)] -> M.Map Token String
encode vocab = fromList [(tok, s) | (s, tok) <- vocab]

-- happy tokens decl
tokensDecl :: String
tokensDecl =
  let
    symbols = brackets ++ operators ++ punctuation
    quote sym = "'" ++ sym ++ "'"
                -- project (Char 'a') = "Char $$"
    project tok = (unwords $ init $ words (show tok)) ++ " $$"
    col2 = 16
    pad s = "  " ++ s ++ (replicate (col2 - length s) ' ')
    decl f g (s, tok) = pad (f s) ++ " { " ++ (g tok) ++ " } "
    toLines f g items = [decl f g item | item <- items]
    lines_kw = toLines id show keywords
    lines_sym = toLines quote show symbols
    lines_lit = toLines quote project literals
    lines_other = toLines id project (identifiers ++ quasiParts)
  in
    "%token\n" ++ unlines (lines_kw ++ lines_sym ++ lines_lit ++ lines_other)

