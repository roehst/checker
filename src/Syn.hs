module Syn where

type Name = String

data Syn
  = App Syn Syn
  | Abs Name Syn
  | Var Name

precApp :: Int
precApp = 1

precLam :: Int
precLam = 2

instance Show Syn where
  showsPrec _ (Var v) = showString v
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