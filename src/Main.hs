module Main where

import Control.Monad (forM_, void)
import Data.Functor (($>))
import Data.Maybe
import Debug.Trace
import Text.ParserCombinators.Parsec

type Name = String

data Ty = TyArr Ty Ty | TyNat | TyVar Name deriving (Eq)

type Con = (Ty, Ty)

newtype Subst = Subst [(Ty, Ty)]

instance Show Subst where
  show (Subst substs) =
    let parts = map (\(name, ty) -> show name ++ "/" ++ show ty) substs
     in foldr1 (\a b -> a ++ " * " ++ b) parts

instance Semigroup Subst where
  Subst s1 <> Subst s2 = Subst (s1 ++ s2)

instance Monoid Subst where
  mempty = Subst []

instance Show Ty where
  showsPrec _ TyNat = showString "Nat"
  showsPrec _ (TyVar name) = showString name
  showsPrec p (TyArr a b) =
    showParen (p > 0) $
      showsPrec (p + 1) a
        . showString " -> "
        . showsPrec (p + 1) b

constraints :: [Con]
constraints =
  [ (TyVar "x", TyNat),
    (TyVar "y", TyVar "x")
  ]

substitute :: Subst -> Ty -> Ty
substitute subst TyNat = TyNat
substitute subst (TyArr a b) = TyArr (substitute subst a) (substitute subst b)
substitute (Subst []) (TyVar name) = TyVar name
substitute (Subst [(TyVar name, ty)]) (TyVar name')
  | name == name' = ty
  | otherwise = TyVar name'
substitute (Subst (x : xs)) ty =
  substitute (Subst [x]) (substitute (Subst xs) ty)

unify :: Ty -> Ty -> Maybe Subst
unify a b | a == b = Just $ mempty
unify (TyArr a b) (TyArr c d) = do
  s1 <- unify b d
  s2 <- unify (substitute s1 a) (substitute s1 c)
  return $ s1 <> s2
unify (TyVar name) (TyVar name') = Just $ Subst [(TyVar name, TyVar name')]
unify TyNat a = unify a TyNat
unify (TyVar a) b = Just $ Subst [(TyVar a, b)]
unify (TyArr a b) c' = unify c' (TyArr a b)

present :: Subst -> IO ()
present (Subst subst) =
  forM_ subst $ \(name, ty) -> do
    putStrLn $ show name ++ " => " ++ show ty

parseTy :: Parser Ty
parseTy = (spaces *> parseNat <|> parseVar <|> parens parseTy) `chainl1` parseArr
  where
    lexeme :: String -> Parser ()
    lexeme str = string str >> spaces
    parseNat = lexeme "Nat" >> return TyNat
    parseArr = lexeme "->" $> TyArr
    parseVar = TyVar <$> (many1 letter <* spaces)
    parens p = lexeme "(" *> p <* lexeme ")"

main :: IO ()
main = do
  s1 <- getLine

  case parse parseTy "" s1 of
    Right t1 -> do
      s2 <- getLine
      case parse parseTy "" s2 of
        Right t2 -> 
          case unify t1 t2 of
            Just x -> present $ x