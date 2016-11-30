import System.Process
import System.IO
import Control.Monad
import Data.List(isPrefixOf,dropWhile,permutations)

main = do
  (Just hi, Just ho, _, _) <- createProcess (proc "./solution" []) { std_in = CreatePipe, std_out = CreatePipe }
  coms <- fmap lines (readFile "steps")
  play hi ho coms
  playManual hi ho

playManual hi ho = getDesc ho >> getLine >>= action hi ho >> playManual hi ho

play hi ho [] = return ()
play hi ho (com:coms) = getDesc ho >> action hi ho com >> play hi ho coms

action hi ho com = putStrLn com >> hPutStrLn hi com >> hFlush hi

getDesc ho = do
  line <- grabLine ho
  if line == "What do you do?"
  then return []
  else do
    lines <- getDesc ho
    return $ line:lines

grabLine ho = do
  line <- hGetLine ho
  putStrLn line
  return line