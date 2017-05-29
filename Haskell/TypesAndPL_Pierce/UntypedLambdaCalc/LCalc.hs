{-# LANGUAGE GADTs, DataKinds #-}
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
    , ""
    , "data Var where"
    , "  Var :: String -> Var"
    ]


main :: IO ()
main = do
  putStrLn greeting
  loop

loop :: IO ()
loop = do
  putStr "> "
  hFlush stdout
  inp <- getLine
  case (maybeRead inp) :: Maybe Term of
    Nothing -> do
      putStrLn "Parse error on input."
      putStrLn "This is the interface: \n"
      putStrLn interface
      loop
    Just t -> do
      print $ evaluate t
      hFlush stdout
      loop


data Term where
  Iden :: Var -> Term
  Lambda :: Var -> Term -> Term
  App :: Term -> Term -> Term
  deriving (Show, Eq, Read)


data Var where
  Var :: String -> Var
  deriving (Show, Eq, Read)

-- We use normal evaluation.
-- That is, left to right.


-- does one step if possible
oneStepEval :: Term -> Maybe Term
oneStepEval (App (Lambda v t) b) = Just $ bindVar (v,b) t
oneStepEval (Lambda v t) = do
  next <- oneStepEval t
  return $ Lambda v next
oneStepEval _ = Nothing



-- binds var in second term if it applies.
bindVar :: (Var, Term) -> Term -> Term
bindVar (Var s, b) r@(Iden (Var s'))
  | s == s' = b
  | otherwise = r
bindVar context (App t1 t2) =
  App (bindVar context t1) (bindVar context t2)
bindVar cont@(Var s, _) lam@(Lambda (Var s') t)
  | s == s' = lam
  | otherwise = Lambda (Var s') (bindVar cont t)




evaluate :: Term -> Term
evaluate t = case oneStepEval t of
  Just t' -> evaluate t'
  Nothing -> t

-- for testing
true :: Term
true = Lambda (Var "x") (Lambda (Var "y") (Iden (Var "x")))
false :: Term
false = Lambda (Var "x") (Lambda (Var "y") (Iden (Var "y")))







