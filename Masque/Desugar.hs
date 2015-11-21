module Masque.Desugar where

import Masque.Parser as S  -- with sugar
import Masque.AST as K     -- kernel

desugar :: S.Expr -> K.Expr
desugar (S.CharExpr c) = (K.CharExpr c)
desugar (S.DoubleExpr d) = (K.DoubleExpr d)
desugar (S.IntExpr i) = (K.IntExpr i)
desugar (S.StrExpr s) = (K.StrExpr s)
desugar _ = undefined
