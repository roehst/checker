module Expr where

import qualified Syn as S

type Name = S.Name

data Expr
  = App Expr Expr
  | Abs Name Expr
  | FreeVar Name
  | BoundVar Int Name

precApp :: Int
precApp = 1

precLam :: Int
precLam = 2

instance Show Expr where
  showsPrec _ (FreeVar name) = showString name
  showsPrec _ (BoundVar i v) = showString (v ++ "!" ++ show i)
  showsPrec n (Abs name body) =
    showParen (n < precLam) $
      showString "\\"
        . showString name
        . showString "."
        . showsPrec precLam body
  showsPrec n (App f x) =
    showParen (n < precApp) $
      showsPrec precApp f
        . showString " "
        . showsPrec precApp x
