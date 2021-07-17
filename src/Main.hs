{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Functor (($>))
import Data.List (elemIndex)
import qualified Expr as E
import qualified Syn as S
import Text.ParserCombinators.Parsec

type Name = S.Name

type Ctx = [Name]

parens ::
  Parser a ->
  Parser a
parens
  p = string "(" *> spaces *> p <* string ")" <* spaces

parseSyn :: Parser S.Syn
parseSyn =
  let nonApp = try parseAbs <|> parseVar <|> parens parseSyn
   in let app = spaces $> S.App
       in nonApp `chainl1` app
  where
    parseVar = S.Var <$> (many1 letter <* spaces)
    parseAbs = do
      string "lam"
      spaces
      name <- many1 letter
      spaces
      string "."
      spaces
      body <- parseSyn
      spaces
      return $ S.Abs name body

brujinize :: Ctx -> S.Syn -> E.Expr
brujinize ctx = \case
  S.Var name -> case elemIndex name ctx of
    Nothing -> E.FreeVar name
    Just n -> E.BoundVar n name
  S.App f x -> E.App (brujinize ctx f) (brujinize ctx x)
  S.Abs name body -> E.Abs name (brujinize (name : ctx) body)

substitute :: Int -> E.Expr -> E.Expr -> E.Expr
substitute index substitution expr =
  case expr of
    E.FreeVar name -> expr
    E.BoundVar index' name
      | index' == index -> substitution
      | otherwise -> expr
    E.App f x ->
      E.App
        (substitute index substitution f)
        (substitute index substitution x)
    E.Abs name body ->
      E.Abs name (substitute (index + 1) substitution body)

eval :: E.Expr -> E.Expr
eval (E.FreeVar name) = E.FreeVar name
eval b@(E.BoundVar _ _) = b
eval (E.Abs name body) = E.Abs name (eval body)
eval (E.App (E.Abs _ body) x) =
  substitute 0 (eval x) body
eval (E.App f x) = eval $ E.App (eval f) (eval x)

main :: IO ()
main = do
  let s = "((lam f. lam x. f x) x) f"
  case parse parseSyn "" s of
    Right term -> do
      print term
      print $ brujinize [] term
      print $ eval $ brujinize [] term
    Left err -> print err
