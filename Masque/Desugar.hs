module Masque.Desugar where

import Data.Maybe (fromJust)
import qualified Data.Map as M

import qualified Masque.Parser as S  -- with sugar
import qualified Masque.AST as K     -- kernel
import qualified Masque.Lexer as L

desugar :: S.Expr -> K.Expr
desugar = modPow . kExpr -- @@ . ifAnd . ifOr
  where
    modPow :: K.Expr -> K.Expr
    modPow (K.CallExpr (K.CallExpr x "pow" [e] []) "mod" [m] [])
      = (K.CallExpr x "modPow" [e, m] [])
    modPow ex = ex

    kExpr :: S.Expr -> K.Expr
    kExpr (S.CharExpr c) = (K.CharExpr c)
    kExpr (S.DoubleExpr d) = (K.DoubleExpr d)
    kExpr (S.IntExpr i) = (K.IntExpr i)
    kExpr (S.StrExpr s) = (K.StrExpr s)

    kExpr (S.NounExpr s) = (K.NounExpr s)

    kExpr (S.BinaryExpr lhs op rhs)
      | (elem op binaryOps) = (K.CallExpr (desugar lhs) verb [kExpr rhs] [])
      where
        pairs = [(op', verb') | ((_, verb'), (op', _)) <- L.assign_ops]
        binaryOps = [op' | (op', _) <- pairs]
        toVerb = M.fromList pairs
        verb = fromJust $ M.lookup op toVerb

    kExpr (S.DefExpr patt ex assign) =
      -- TODO: there's more to this.
      (K.DefExpr (kPatt patt) (fmap kExpr ex) (kExpr assign))

    kExpr e = error ("not implemented: desugar for: " ++ show e)

    kPatt :: S.Patt -> K.Patt
    kPatt (S.FinalPatt s g) = (K.FinalPatt s (fmap kExpr g))
    kPatt (S.IgnorePatt g) = (K.IgnorePatt (fmap kExpr g))
    kPatt (S.BindPatt s) = (K.BindPatt s)
    kPatt (S.ListPatt ps) = (K.ListPatt (map kPatt ps))
    kPatt (S.VarPatt s g) = (K.VarPatt s (fmap kExpr g))
    kPatt (S.ViaPatt ex p) = (K.ViaPatt (kExpr ex) (kPatt p))
