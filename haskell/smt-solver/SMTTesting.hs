{-# LANGUAGE TypeFamilies, TypeInType,
    GADTs, RecordWildCards, StandaloneDeriving #-}
module SMTTesting where



import qualified SimpleSMT as SMT

{-
( SExpr,Value(..),Result(..),
                                    check, assert, define, push,
                                    pop, command, Solver(..),
                                    newSolver, declare, tInt
                                    )
-}


solverOpts = [ "-smt2", "-in" ]

grabSolver :: IO SMT.Solver
grabSolver = SMT.newSolver "z3" solverOpts Nothing



-- should be sat
testOne :: IO ()
testOne = do
  z3Hook <- grabSolver
  SMT.declare z3Hook "a" SMT.tInt
  SMT.declare z3Hook "b" SMT.tInt
  SMT.assert z3Hook (SMT.add (SMT.Atom "a") (SMT.Atom "b") `SMT.eq` (SMT.int 10))
  SMT.assert z3Hook (SMT.mul (SMT.Atom "a") (SMT.Atom "b") `SMT.eq` (SMT.int 16))
  result <- SMT.check z3Hook
  putStrLn ("The result was: " ++ show result)


-- should be unsat
testTwo :: IO ()
testTwo = do
  z3Hook <- grabSolver
  SMT.declare z3Hook "a" SMT.tInt
  SMT.declare z3Hook "b" SMT.tInt
  SMT.assert z3Hook (SMT.add (SMT.Atom "a") (SMT.Atom "b") `SMT.eq` (SMT.int 5))
  SMT.assert z3Hook (SMT.mul (SMT.Atom "a") (SMT.Atom "b") `SMT.eq` (SMT.int 25))
  result <- SMT.check z3Hook
  putStrLn ("The result was: " ++ show result)



mkMaybe = "(declare-datatypes (T) ((Maybe nothing (just (fromJust T)))))"
(<.>) = SMT.eq

testThree :: IO ()
testThree = do
  z3 <- grabSolver
  SMT.ackCommand z3 $ SMT.Atom mkMaybe
  SMT.push z3
  m1 <- SMT.declare z3 "m1" tArrStrStr
  m2 <- SMT.declare z3 "m2" tArrStrStr
  -- Assert given
  SMT.assert z3 ((SMT.store m1 (mkSLit "1") (mkSVal $ Just "hi")) <.>
    (SMT.store m2 (mkSLit "2") (mkSVal $ Just "hello")))

  -- Assert (fold (OR) $ (map (Not) [Wanted]))
  SMT.assert z3 (SMT.not (m1 <.> (SMT.store m1 (mkSLit "2") (mkSVal $ Just "hello"))))
  result <- SMT.check z3
  putStrLn ("We expect the result to be unsat. The given DOES imply the wanted.")
  putStrLn ("The result was: " ++ show result)
  SMT.pop z3
  SMT.stop z3
  return ()



type SExpr = SMT.SExpr

tArrStrStr = SMT.tArray (SMT.Atom "String") tMaybeStr
tMaybeStr = SMT.Atom "(Maybe String)"

mkSLit :: String -> SExpr
mkSLit s = SMT.Atom $ "\"" ++ s ++ "\""

mkSVal :: Maybe String -> SExpr
mkSVal Nothing = SMT.Atom "nothing"
mkSVal (Just s) = SMT.Atom ("(just " ++ "\"" ++ s ++ "\")")

{-
Skolem space encoding:

1) Check givens are valid by checking sat

2) Want to show G => W1 and W2 ... and Wn
This can be showing if G is sat,
(not W1) or (not W2) or (not W3) ... or (not Wn)

cannot be satisfied.
That is, if G is sat then all the W's are sat.
-}



{-
Goal:
1) Now, the simple case.
Don't think about background theories.



2) Make it work with all background types
(How do I convert two equal types into the same
letter? Do I keep track of state. Probably.)

---
#Process

Initialize, assert all givens and wanteds


---

1) Parse into fm equalities
These have values and key variables.
[((Fm,Fm), Ct)] where Fm includes key and value vars (not maybe value vars)

2) Split into givens and wanteds

----
3) push

3) Sequencing: wanteds non-empty? givens sat?

4) Assert all givens

5) create predicates for all wanteds

6) map not and fold by or into one long assertion

7) Assert part 6) and if it is unsat, then add all wanteds
otherwise add none.
----

#Notes
* no such thing as a nil so
make that a special symbol

* need a helper to take a (fm,fm) and convert it into a
SExpr

For skolem vars at the bottom, use their uniques to make variables.


#How To Do It



-}

















