{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module LCalc where

import Network.CGI.Protocol
import System.IO

greeting :: String
greeting = "Welcome to Divesh's Untyped Lambda Calculus."

interface :: String
interface =
  unlines
    [ "data Term where"
    , "  Iden :: Var -> Term"
    , "  Lambda :: Var -> Term -> Term"
    , "  App :: Term -> Term -> Term"
    , "  Def :: String -> Term -- user made bindings"
    , "data Var = Var String"
    , "data Bind = String := Term"
    ]

main :: IO ()
main = do
  putStrLn greeting
  loop []

data Bind where
  (:=) :: String -> Term -> Bind
  deriving (Show, Read, Eq)

infixl 1 :=

loop :: Bindings ->  IO ()
loop xs = do
  putStr "> "
  hFlush stdout
  inp <- getLine
  case maybeRead inp :: Maybe Term of
    Nothing ->
      case maybeRead inp :: Maybe Bind of
        Just (s := t) -> loop $ (s,t) : xs
        Nothing -> do
          putStrLn "Parse error on input."
          putStrLn "Either provide a Bind or a Term\n"
          putStrLn interface
          loop xs
    Just t -> do
      print $ evaluate xs t
      hFlush stdout
      loop xs


data Term where
  Iden :: Var -> Term
  Lambda :: Var -> Term -> Term
  App :: Term -> Term -> Term
  Def :: String -> Term -- user made bindings
  deriving (Show, Eq, Read)


data Var where
  Var :: String -> Var
  deriving (Show, Eq, Read)

type Bindings = [(String, Term)]

-- We use normal evaluation.
-- That is, left to right.

-- check it works out.. with this binding alg.

-- does one step if possible
oneStepEval :: Bindings -> Term -> Maybe Term
oneStepEval xs (Def s) = lookup s xs
oneStepEval xs (App (Lambda v t) b) = Just $ bindVar xs (v,b) t
oneStepEval xs (Lambda v t) = do
  next <- oneStepEval xs t
  return $ Lambda v next
oneStepEval _ _ = Nothing

-- helper
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

-- binds var in second term if it applies.
bindVar :: Bindings -> (Var, Term) -> Term -> Term
bindVar xs _ a@(Def s) = fromMaybe a (lookup s xs)
bindVar _ (Var s, b) r@(Iden (Var s'))
  | s == s' = b
  | otherwise = r
bindVar xs context (App t1 t2) =
  App (bindVar xs context t1) (bindVar xs context t2)
bindVar xs cont@(Var s, _) lam@(Lambda (Var s') t)
  | s == s' = lam
  | otherwise = Lambda (Var s') (bindVar xs cont t)




evaluate :: Bindings -> Term -> Term
evaluate xs t = case oneStepEval xs t of
  Just t' -> evaluate xs t'
  Nothing -> t

-- for testing
true :: Term
true = Lambda (Var "x") (Lambda (Var "y") (Iden (Var "x")))
false :: Term
false = Lambda (Var "x") (Lambda (Var "y") (Iden (Var "y")))







