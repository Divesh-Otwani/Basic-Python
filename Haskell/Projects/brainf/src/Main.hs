{-# LANGUAGE GADTs #-}
module Main where

import qualified Data.Vector.Persistent as V
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
  Sequence :: BFInput  -> BFInput -> BFInput
--  Iterate :: BFInput -> BFInput       --  Todo: while loops


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
evaluate (Sequence i j) x = do
  maybeNewList <- evaluate i x
  case maybeNewList of
    Just newList -> evaluate j newList
    Nothing -> return Nothing
evaluate MoveRight (list, pointer) =
  if length list == pointer + 1
  then return $ Just (V.snoc list 0, pointer + 1)
  else return $ Just (list, pointer + 1)
evaluate MoveLeft (list, pointer) =
  if pointer == 0
  then return Nothing
  else return $ Just (list, pointer - 1)
evaluate Inc (list, pointer) =
  return $
    Just (V.update pointer ((V.unsafeIndex list pointer) + 1) list, pointer)
evaluate Dec (list, pointer) =
  return $
    Just (V.update pointer ((V.unsafeIndex list pointer) - 1) list, pointer)
evaluate Print x@(list, pointer) = do
  putStrLn (show (V.unsafeIndex list pointer))
  hFlush stdout
  return $ Just x
evaluate Input x@(list, pointer) = do
  int <- grabAnInt
  return $ Just (V.update pointer int list, pointer)

grabAnInt :: IO Int
grabAnInt = do
  putStr "Enter int: "
  hFlush stdout
  line <- getLine
  case readMaybe line :: Maybe Int of
    Nothing -> grabAnInt
    Just int -> return int


convertString :: String -> Maybe BFInput
convertString [] = Just DoNothing
convertString (c:cs) = do
  singleCommand <- convertChar c
  otherCommands <- convertString cs
  return (Sequence singleCommand otherCommands)
  where
    convertChar :: Char -> Maybe BFInput
    convertChar '+' = Just Inc
    convertChar '-' = Just Dec
    convertChar '>' = Just MoveRight
    convertChar '<' = Just MoveLeft
    convertChar '.' = Just Print
    convertChar ',' = Just Input
    convertChar _   = Nothing








