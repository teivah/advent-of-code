import System.IO

main :: IO ()
main = do
  content <- readFile "input.txt"
  let res = fn1 content 0
  print res
  let res = fn2 content 0 0
  print res

fn1 [] count = count
fn1 (x:xs) count =
  if x == '('
    then fn1 xs (count + 1)
    else fn1 xs (count - 1)

fn2 [] index count = -1
fn2 (x:xs) index count =
  if count == -1
    then index
    else
      if x == '('
        then fn2 xs (index + 1) (count + 1)
        else fn2 xs (index + 1) (count - 1)
