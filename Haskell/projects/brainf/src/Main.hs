{-# LANGUAGE GADTs #-}
module Main where

import qualified Data.Vector as V
import Text.Read ( readMaybe )
import System.IO (hFlush, stdin, stdout)

data BFInput where
  DoNothing :: BFInput
  Inc :: BFInput
  Dec :: BFInput
  MoveLeft :: BFInput
  MoveRight :: BFInput
  Print :: BFInput
  Input :: BFInput
  Seq :: BFInput  -> BFInput -> BFInput
  Iterate :: BFInput -> BFInput       --  Todo: while loops


type BfList = (V.Vector Int, Int)

main :: IO ()
main = do
  putStrLn "Welcome to Divesh's simplified brainf interpreter"
  putStrLn "This is version 0.1"
  putStrLn ""
  brainf ((V.singleton 0),0)


brainf :: BfList -> IO ()
brainf bflist@(list, pointer) = do
  putStr "simple-brainf$ "
  hFlush stdout
  line <- getLine
  case convertString line of
    Nothing -> do
      putStrLn "Invalid input."
      hFlush stdout
      brainf bflist
    Just validSyntax -> do
      maybeNewList <- evaluate validSyntax bflist
      case maybeNewList of
        Just newList -> brainf newList
        Nothing -> do
          putStrLn "*** Runtime Error: Ran off tape start. ***"
          putStrLn "Interpreter Exiting."
          putStrLn ""
          hFlush stdout

-- invariant: valid pointer
evaluate :: BFInput -> BfList -> IO (Maybe BfList)
evaluate DoNothing x = return (Just x)
evaluate (Seq i j) x = do
  maybeNewList <- evaluate i x
  case maybeNewList of
    Just newList -> evaluate j newList
    Nothing -> return Nothing
evaluate e@(Iterate body) bfl = do
  Just bfl'@(list,pointer) <- evaluate body bfl
  Just currVal <- return $ list V.!? pointer
  case currVal of
    0 -> return $ Just bfl'
    _ -> evaluate e bfl'
evaluate MoveRight (list, pointer) =
  if length list == pointer + 1
  then return $ Just (V.snoc list 0, pointer + 1)
  else return $ Just (list, pointer + 1)
evaluate MoveLeft (list, pointer) =
  if pointer == 0
  then return Nothing
  else return $ Just (list, pointer - 1)
evaluate Inc (list, pointer) =
  return $ do
    currVal <- list V.!? pointer
    Just (list V.// [(pointer, (currVal + 1))], pointer)
evaluate Dec (list, pointer) =
  return $ do
    currVal <- list V.!? pointer
    Just (list V.// [(pointer, (currVal - 1))], pointer)
evaluate Print x@(list, pointer) = do
  putStrLn (show (V.unsafeIndex list pointer))
  hFlush stdout
  return $ Just x
evaluate Input x@(list, pointer) = do
  int <- grabAnInt
  return $ Just (list V.// [(pointer, int)], pointer)


grabAnInt :: IO Int
grabAnInt = do
  putStr "Enter int: "
  hFlush stdout
  line <- getLine
  case readMaybe line :: Maybe Int of
    Nothing -> grabAnInt
    Just int -> return int




convertString :: String -> Maybe BFInput
convertString = seqParse DoNothing []


type CmdInScope = BFInput
type WhileLoopStack = [BFInput -> BFInput]
seqParse :: CmdInScope -> WhileLoopStack -> String -> Maybe BFInput
seqParse cmd [] [] = return cmd
seqParse cmd (_:_) [] = Nothing 
seqParse cmd ls (c:cs) = case c of
  '+' -> seqParse (Seq cmd Inc) ls cs
  '-' -> seqParse (Seq cmd Dec) ls cs
  ',' -> seqParse (Seq cmd Input) ls cs
  '.' -> seqParse (Seq cmd Print) ls cs
  '>' -> seqParse (Seq cmd MoveRight) ls cs
  '<' -> seqParse (Seq cmd MoveLeft) ls cs
  '[' ->
    let
      newLoop :: BFInput -> BFInput
      newLoop loopBody = Seq cmd $ Iterate loopBody
      ls' = newLoop : ls
    in
      seqParse DoNothing ls' cs
  ']' -> case ls of
    []     -> Nothing
    (x:xs) -> seqParse (x cmd) xs cs
  _   -> seqParse cmd ls cs


