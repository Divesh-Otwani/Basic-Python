module Tutorial


main : IO ()
main = putStrLn "Hello World."


-- Primitive types

z : Int
z = 42

foo : String
foo = "Sausage machine"

bar : Char
bar = 'Z'

quux : Bool
quux = False

foobar : List Char -- not strings!
foobar = ['a', 'b', 'c']


{-

Idris files are a name, then imports, then definitions.

Programs span modules. Each module has its own namespace

Order of declaration matters in a file!

Each definition must have a type declaration.
(No type inference!)

Typical arithmatic overloaded using interfaces
(like type classes)


-}


data Natural = Zero | Succ Natural;

-- data L a = N | (:>) a (L a)
-- infixr 10 :> -- bug in compiler here.



{-

data types like in traditional haskell.

Functions, data constructors and type constructors,
like in haskell, can be given infix operators.

Symbols allowed: :+-*\/=.?|&><!@$%^~#

but can't make:
   :, =>, ->, <-, =, ?=, |, **, ==>, \, %, ~, ?, and !

no restriction on capitalization of types and functions.

Idris comes with in build, optimized nat defn.
It uses integers at lowest level, but this at compile time.

-}


-- Where clauses:

reverse : List a -> List a
reverse xs = revAcc [] xs where
  revAcc : List a -> List a -> List a
  revAcc acc [] = acc
  revAcc acc (x :: xs) = revAcc (x :: acc) xs


-- cases and nested data declarations

foo2 : Int -> Int
foo2 x = case isLT of
            Yes => x*2
            No => x*4
    where
       data MyLT = Yes | No

       isLT : MyLT
       isLT = if x < 20 then Yes else No


-- Use holes to write code incrementally,
-- often just focus on the interface

even1 : Nat -> Bool
even1 Z = True
even1 (S Z) = False
even1 (S (S k)) = even1 k --?even_rhs

{-

Dependent Types

First Class Types


-}

makeType : Bool -> Type
makeType True = Nat
makeType False = List Nat

-- use in type declaration

makeThing : (x : Bool) -> (makeType x)
makeThing True = Z
makeThing False = []

sum : (b : Bool) -> (makeType b) -> Nat
sum True x = x
sum False [] = Z
sum False (x :: xs) = x + sum False xs

-- length indexed vectors

data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

{-
We say vect is indexed by Nat and
parameterized by Type

-}

(++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) Nil ys = ys
(++) (x :: xs) ys = x :: xs ++ ys



data TypeHold : Type where
  Hold : Type -> TypeHold


Extract : TypeHold -> Type
Extract (Hold t) = t


{-
  Idris is not as advanced as I hoped.

func : (x : TypeHold) -> Maybe (Extract x)
func (Hold t) = case t of
  Int => Nothing
  tp => Nothing
--func x = Nothing

func (Hold t) = case t of
  Int => Just 0
func x = Nothing

-}


-- Fin


data Fin : Nat -> Type where
  FZero : Fin (S n)
  FSucc : Fin n -> Fin (S n)

-- Fin n is a type with n
-- elements


-- Since there is no instance of
-- Fin Z, the second arg can't be nil.
index : Fin n -> Vect n a -> a
index FZero (x :: xs) = x
index (FSucc fn) (x :: xs) =
  index fn xs


--      Implicit Arguments

{-

These are arguments inferred from the other
explicitly given arguments.

Could have expressed the type of index as

index : {a : Type} -> {n : Nat} -> Fin n -> Vect n a -> a

Arguments in {} are implcit; not given in
the application and are inferred from the other args.

In general, any argument can be given a name

index : (i : Fin n) -> (xs : Vect n a) -> a

could happen.

Also, you can pattern match on {}
with {implicitVar = pattern}


-}


-- implicit parttern match

isEmpty : Vect n a -> Bool
isEmpty {n = Z} _   = True
isEmpty {n = (S k)} _ = False


-- Give implicit args types,
-- and force relationships between things

-- The using part makes us avoid the commented
-- implicit args below
using (x:a, y:a, xs: Vect n a)
  data IsElem : a -> Vect n a -> Type where
    Here :
--      {x:a} ->
--      {xs:Vect n a} ->
      IsElem x (x :: xs)
    There :
      -- {x,y:a} ->
      -- {xs:Vect n a} ->
      IsElem x xs ->
      IsElem x (y :: xs)

testV : Vect 4 Int
testV = 3 :: 4 :: 5 :: 6 :: Nil

-- recall lowecase names in type defs are
-- treated as implicit type variables.
-- so we need to qualify testV
inVect : IsElem 5 Tutorial.testV
inVect = There (There Here)



-- In general, dependent ordering
-- can't use definition not defined yet,
-- this is relaxed in mutual blocks

mutual
  even : Nat -> Bool
  even Z     = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd Z     = False
  odd (S k) = even k


-- IO done in IO :: Type -> Type GADT

tostdin : IO ()
tostdin = putStrLn "Hello" -- or putStr

fromstdin : IO String
fromstdin = getLine


-- File interface
-- Will need to check on the monad interface

{-

data File -- abstract
data Mode = Read | Write | ReadWrite

openFile :
  (f : String) -> (m : Mode) -> IO (Either FileError File)

closeFile :
  File -> IO ()

fGetLine :
  (h : File) -> IO (Either FileError String)

fPutStr :
  (h : File) -> (str : String) -> IO (Either FileError ())

fEOF : File -> IO Bool

-}


-- or, could use do notation sugar



-- These two compile but error at runtime
writeHello : IO ()
writeHello = do
  file <- openFile "//home//divesh//test" Append
  case file of
    (Right f) =>
      do
        fPutStr f "Hello"
        closeFile f
    (Left x) => pure ()


wH : IO ()
wH = do
  writeFile "//home//divesh//test" "Hello"
  pure ()


-- Idris is strict, use lazy interface

{-
data Lazy : Type -> Type where
     Delay : (val : a) -> Lazy a

Force : Lazy a -> a
-}

ifThenElse : Bool -> Lazy a -> Lazy a -> a
ifThenElse True t _  = Force t
ifThenElse False _ e = Force e


-- Codata types secretly use Inf :: Type -> Type

-- This makes them lazy

codata MyStream : Type -> Type where
  Cons : (e : a) -> MyStream a -> MyStream a

{-
turns into
data ... where
  Cons : (e : a) -> Inf (Stream a) -> Stream a

-}


ones : MyStream Nat
ones = Cons 1 ones

-- when doing complex examples,
-- it's useful to write out Inf yourself


--      Useful data types


-- List and Vect already seen.

-- as syntactic sugar, any constructor
-- even overloaded, with Nil and ::
-- is turned into [] representation


-- Observe that functions can be overloaded and
-- are disambiguated by the type of their argument

   -- Isn't this stupid? Shouldn't we use typeclasses
   -- Can't you then make really dumb overloads?



-- lambdas are \x => x * 2

foo1 : Maybe Int
foo1 = Just 1


-- has sugar
foo3 : Pair Nat Bool -- or (Nat, Bool)
foo3 = (2, False) -- also, MkPair 2 False

-- a three tuple is just a nested tuple

-- Now, cool stuff.




             --   Dependent pairs


-- In the type sig, you give a var and a predicate: a -> Type

vec : (n : Nat ** Vect n Nat)
vec = (3 ** [1,5,2])











