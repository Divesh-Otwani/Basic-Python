module FileWrite

main : IO ()
main = putStrLn "Hello World."

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




