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
    Just t ->
      case evalBindings xs t of
        Nothing ->
          putStrLn "Used undefined bindings." >>
          loop xs
        Just tt ->
          print (evaluate tt) >>
          hFlush stdout >>
          loop xs

type IdSet = String

data TTerm where
  TIden :: IdSet -> TTerm
  (::>>) :: IdSet -> TTerm -> TTerm
  (::@) :: TTerm -> TTerm -> TTerm
  deriving (Show, Eq, Read)

data Term where
  Iden :: IdSet -> Term
  (:>>) :: IdSet -> Term -> Term -- Lambda
  (:@) :: Term -> Term -> Term
  Def :: String -> Term -- user made bindings
  deriving (Show, Eq, Read)


infixr 6 :>>
infixr 6 ::>>
infixl 6 :@
infixl 6 ::@

-- note that
lambda :: IdSet -> Term -> Term
lambda = (:>>)
app :: Term -> Term -> Term
app = (:@)


type Bindings = [(String, Term)]

-- first, unlimited depth
evalBindings :: Bindings -> Term -> Maybe TTerm
evalBindings xs (Def s) = do
  t <- lookup s xs
  evalBindings xs t
evalBindings _  (Iden s) = Just $ TIden s
evalBindings xs (t1 :@ t2) = do
  tt1 <- evalBindings xs t1
  tt2 <- evalBindings xs t2
  return $ tt1 ::@ tt2
evalBindings xs (s :>> t) = do
  tt <- evalBindings xs t
  return $ s ::>> tt

-- We use normal evaluation.
-- That is, left to right.

-- check it works out.. with this binding alg.

-- does one step if possible
oneStepEval :: TTerm -> Maybe TTerm
oneStepEval ((v ::>> t) ::@ b) = Just $ bindVar (v,b) t
oneStepEval (a ::@ t) = do
  next <- oneStepEval a
  return $ next ::@ t
oneStepEval (v ::>> t) = do
  next <- oneStepEval t
  return $ v ::>> next
oneStepEval _ = Nothing


-- binds var in second term if it applies.
bindVar :: (IdSet, TTerm) -> TTerm -> TTerm
bindVar (s, b) r@(TIden s')
  | s == s' = b
  | otherwise = r
bindVar context (t1 ::@ t2) =
  bindVar context t1 ::@ bindVar context t2
bindVar cont@(s, _) lam@(s' ::>> t)
  | s == s' = lam -- lexical scoping
  | otherwise = s' ::>> bindVar cont t




evaluate :: TTerm -> TTerm
evaluate t = case oneStepEval t of
  Just t' -> evaluate t'
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

--recursion1 = "x" :>> (ite :@ (iszero :@ Iden "x") :@ Iden "x" :@ ((Def "recursion1") :@ (pred :@ Iden "x")))


givenDefinitions = [ ("zero", zero)
                   , ("scc", scc)
                   , ("pred", pred)
                   , ("ite", ite)
                   , ("isZ", iszero)
                   ]



