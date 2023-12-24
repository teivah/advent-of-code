import System.IO

main :: IO ()
main = do
  res <- withFile "input.txt" ReadMode (\handle -> fn1 handle)
  print res

fn1 handle  = do
  eof <- hIsEOF handle
  if eof
    then return 1
    else do
      line <- hGetLine handle
      print line
      fn1 handle
