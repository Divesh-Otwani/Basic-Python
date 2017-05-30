{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module LCalc where

import Prelude hiding ( fst, snd, pred )
import Network.CGI.Protocol
import System.IO

greeting :: String
greeting = "Welcome to Divesh's Untyped Lambda Calculus." ++
  "\nHere is the interface:\n\n" ++ interface
interface :: String
interface =
  unlines
    [ "A Term is one of"
    , "  Iden String"
    , "  String :>> Term -- lambda"
    , "  Term :@ Term -- application"
    , "  Def String -- user made bindings"
    , "String := Term -- is how you bind"
    , "-- Note, you need all the parenthesis :("
    ]

main :: IO ()
main = do
  putStrLn greeting
  loop givenDefinitions -- see bottom

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

type IdSet = String

data Term where
  Iden :: IdSet -> Term
  (:>>) :: IdSet -> Term -> Term -- Lambda
  (:@) :: Term -> Term -> Term
  Def :: String -> Term -- user made bindings
  deriving (Show, Eq, Read)


infixr 6 :>>
infixl 6 :@

-- note that
lambda :: IdSet -> Term -> Term
lambda = (:>>)
app :: Term -> Term -> Term
app = (:@)

type Bindings = [(String, Term)]

-- We use normal evaluation.
-- That is, left to right.

-- check it works out.. with this binding alg.

-- does one step if possible
oneStepEval :: Bindings -> Term -> Maybe Term
oneStepEval xs (Def s) = lookup s xs
oneStepEval xs ((v :>> t) :@ b) = Just $ bindVar xs (v,b) t
oneStepEval xs (Def s :@ t) = do
  lam <- lookup s xs
  return $ lam :@ t
oneStepEval xs (a :@ t) = do
  next <- oneStepEval xs a
  return $ next :@ t
oneStepEval xs (v :>> t) = do
  next <- oneStepEval xs t
  return $ v :>> next
oneStepEval _ _ = Nothing


-- binds var in second term if it applies.
bindVar :: Bindings -> (IdSet, Term) -> Term -> Term
bindVar xs b a@(Def s) =
  case lookup s xs of
    Just t -> bindVar xs b t
    Nothing -> a
bindVar _ (s, b) r@(Iden s')
  | s == s' = b
  | otherwise = r
bindVar xs context (t1 :@ t2) =
  bindVar xs context t1 :@ bindVar xs context t2
bindVar xs cont@(s, _) lam@(s' :>> t)
  | s == s' = lam
  | otherwise = s' :>> bindVar xs cont t




evaluate :: Bindings -> Term -> Term
evaluate xs t = case oneStepEval xs t of
  Just t' -> evaluate xs t'
  Nothing -> t



-- for testing
true :: Term
true = "x" :>> "y" :>> Iden "x"
false :: Term
false = "x" :>> "y" :>> Iden "y"
istrue = true :@ true :@ false


alwaysFalse = "x" :>> false
pair = "f" :>> "s" :>> "b" :>> (Iden "b" :@ Iden "f" :@ Iden "s")
fst = "p" :>> (Iden "p" :@ true)
snd = "p" :>> (Iden "p" :@ false)

iszero = "m" :>> (Iden "m" :@ alwaysFalse :@ true)
zero = false
scc =
  "n" :>> "s" :>> "z" :>>
    (Iden "s" :@ (Iden "n" :@ Iden "s" :@ Iden "z"))
one = scc :@ zero
plus =
  "m" :>> "n" :>> "s" :>> "z" :>>
    (Iden "m" :@ Iden "s" :@ (Iden "n" :@ Iden "s"  :@ Iden "z"))
zz = pair :@ zero :@ zero
ss = "p" :>> (pair :@ (snd :@ Iden "p") :@ (plus :@ one :@ (snd :@ Iden "p")))
pred = "m" :>> (fst :@ (Iden "m" :@ ss :@ zz))
ite = "if" :>> "then" :>> "else" :>> (Iden "if" :@ Iden "then" :@ Iden "else")


givenDefinitions = [ ("zero", zero)
                   , ("scc", scc)
                   , ("pred", pred)
                   , ("ite", ite)
                   , ("isZ", iszero)
                   ]



