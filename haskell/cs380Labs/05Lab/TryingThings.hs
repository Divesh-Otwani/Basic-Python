module Main where



import System.IO


main = do
  putStrLn "Hello World!"
  putStr "Enter your name: "
  hFlush stdout -- flush buffer
  name <- getLine
  putStrLn("Hello, " ++ name ++ ".")











