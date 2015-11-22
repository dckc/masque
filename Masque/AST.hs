{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Masque.AST where

import Text.PrettyPrint.GenericPretty (Generic, Out)
import Data.Data

data Expr = CharExpr Char
          | DoubleExpr Double
          | IntExpr Integer
          | StrExpr String
          | AssignExpr String Expr
          | BindingExpr String
          | CallExpr Expr String [Expr] [NamedExpr]
          | DefExpr Patt (Maybe Expr) Expr
          | EscapeOnlyExpr Patt Expr
          | EscapeExpr Patt Expr Patt Expr
          | FinallyExpr Expr Expr
          | HideExpr Expr
          | IfExpr Expr Expr (Maybe Expr)
          | NounExpr String
          | ObjectExpr String Patt Expr [Expr] [Method] [Matcher]
          | SequenceExpr [Expr]
          | TryExpr Expr Patt Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data Matcher = Matcher Patt Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data Method = Method String String [Patt] [NamedPatt] Expr Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data NamedExpr = NamedExpr Expr Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data Patt = IgnorePatt (Maybe Expr)
          | BindingPatt String
          | FinalPatt String (Maybe Expr)
          | ListPatt [Patt]
          | VarPatt String (Maybe Expr)
          | ViaPatt Expr Patt
    deriving (Eq, Show, Read, Data, Typeable, Generic)

data NamedPatt = NamedPatt Expr Patt Expr
    deriving (Eq, Show, Read, Data, Typeable, Generic)

instance Out Expr
instance Out NamedExpr
instance Out Method
instance Out Matcher
instance Out Patt
instance Out NamedPatt
