{
module Masque.Parser where

import Masque.Lexer(Token(..), Shape(..), Direction(..),
                    lexer, offside)
import Masque.AST(Expr(..))

  }

%name parseMonteExpr
%tokentype { Token }
%error { parseError }

%token
  as               { KW_as } 
  bind             { KW_bind } 
  break            { KW_break } 
  catch            { KW_catch } 
  continue         { KW_continue } 
  def              { KW_def } 
  else             { KW_else } 
  escape           { KW_escape } 
  exit             { KW_exit } 
  extends          { KW_extends } 
  exports          { KW_exports } 
  finally          { KW_finally } 
  fn               { KW_fn } 
  for              { KW_for } 
  guards           { KW_guards } 
  if               { KW_if } 
  implements       { KW_implements } 
  imports          { KW_imports } 
  in               { KW_in } 
  interface        { KW_interface } 
  match            { KW_match } 
  meta             { KW_meta } 
  method           { KW_method } 
  object           { KW_object } 
  pass             { KW_pass } 
  pragma           { KW_pragma } 
  return           { KW_return } 
  switch           { KW_switch } 
  to               { KW_to } 
  try              { KW_try } 
  var              { KW_var } 
  via              { KW_via } 
  when             { KW_when } 
  while            { KW_while } 
  '('              { TokBracket Round Open } 
  ')'              { TokBracket Round Close } 
  '['              { TokBracket Square Open } 
  ']'              { TokBracket Square Close } 
  '${'             { TokDollarBracket Curly Open } 
  '@{'             { TokAtBracket Curly Open } 
  '{'              { TokBracket Curly Open } 
  '}'              { TokBracket Curly Close } 
  '~'              { TokComplement } 
  '..'             { TokRangeIncl } 
  '..!'            { TokRangeExcl } 
  ':='             { TokAssign } 
  '<=>'            { TokAsBigAs } 
  '<='             { TokLEq } 
  '>='             { TokGEq } 
  '=='             { TokEq } 
  '!='             { TokNotEq } 
  '=~'             { TokMatchBind } 
  '!~'             { TokNotMatchBind } 
  '&&'             { TokLogicalAnd } 
  '||'             { TokLogicalOr } 
  '&!'             { TokButNot } 
  '^='             { TokVERB_ASSIGN "xor" } 
  '^'              { TokXor } 
  '+='             { TokVERB_ASSIGN "add" } 
  '+'              { TokAdd } 
  '-='             { TokVERB_ASSIGN "subtract" } 
  '-'              { TokSubtract } 
  '<<='            { TokVERB_ASSIGN "shiftLeft" } 
  '<<'             { TokShiftLeft } 
  '>>='            { TokVERB_ASSIGN "shiftRight" } 
  '>>'             { TokShiftRight } 
  '**='            { TokVERB_ASSIGN "pow" } 
  '**'             { TokPow } 
  '*='             { TokVERB_ASSIGN "multiply" } 
  '*'              { TokMultiply } 
  '//='            { TokVERB_ASSIGN "floorDivide" } 
  '//'             { TokFloorDivide } 
  '/='             { TokVERB_ASSIGN "approxDivide" } 
  '/'              { TokApproxDivide } 
  '%='             { TokVERB_ASSIGN "mod" } 
  '%'              { TokMod } 
  '&='             { TokVERB_ASSIGN "and" } 
  '&'              { TokAnd } 
  '|='             { TokVERB_ASSIGN "or" } 
  '|'              { TokOr } 
  ';'              { TokSemi } 
  ','              { TokComma } 
  ':'              { TokColon } 
  '::'             { TokStringNoun } 
  '?'              { TokSuchThat } 
  '_'              { TokIgnore } 
  '.'              { TokCall } 
  '<-'             { TokSend } 
  '->'             { TokArrow } 
  '=>'             { TokFatArrow } 
  '.char.'         { TokChar $$ } 
  '.int.'          { TokInt $$ } 
  '.float64.'      { TokDouble $$ } 
  '.String.'       { TokString $$ } 
  IDENTIFIER       { TokIDENTIFIER $$ } 
  AT_IDENT         { TokAT_IDENT $$ } 
  DOLLAR_IDENT     { TokDOLLAR_IDENT $$ } 
  QUASI_OPEN       { TokQUASI Open $$ } 
  QUASI_CLOSE      { TokQUASI Close $$ } 

%%

Expr : literal ';'          { $1 :: Expr }

literal : '.char.'           { CharExpr $1 }
	| '.int.'	     { IntExpr $1 }
	| '.float64.'	     { DoubleExpr $1 }
	| '.String.'	     { StrExpr $1 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"


parseFile :: String -> IO ()
parseFile fileName = do
  code <- readFile fileName
  let tokens = offside $ lexer [] code
  let expr = parseMonteExpr tokens
  print expr

}
