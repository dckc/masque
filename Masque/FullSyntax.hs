{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Masque.FullSyntax where

import Data.Data
import Text.PrettyPrint.GenericPretty (Generic, Out)

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
