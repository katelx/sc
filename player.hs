import System.Process
import System.IO
import Control.Monad
import Data.List(isPrefixOf,dropWhile)
import Debug.Trace

data Action = Take String|Use String|Go String

allDirs = mapM (const ["north", "south", "east", "west"]) [1..3]

main = mapM solveSingleMaze [["west", "south", "north"]] >> hFlush stdout

solveSingleMaze directions = do
  (Just hi, Just ho, _, _) <- createProcess (proc "./solution" []) { std_in = CreatePipe, std_out = CreatePipe }
  play hi ho directions

play hi ho directions = do
  tak "tablet" hi ho
  use "tablet" hi ho
  go "doorway" hi ho
  go "north" hi ho
  go "north" hi ho
  go "bridge" hi ho
  go "continue" hi ho
  go "down" hi ho
  go "east" hi ho
  tak "empty lantern" hi ho
  go "west" hi ho
  go "west" hi ho
  go "passage" hi ho
  go "ladder" hi ho
--  putStrLn ("checking directions: " ++ show directions)
--  solveMaze hi ho directions
-- >>= putStrLn . unlines
  go "west" hi ho
  go "south" hi ho
  go "north" hi ho
  tak "can" hi ho
  go "west" hi ho
  go "ladder" hi ho
  use "can" hi ho
  use "lantern" hi ho
  go "darkness" hi ho
  go "continue" hi ho
  go "west" hi ho
  go "west" hi ho
  go "west" hi ho
  go "west" hi ho
  go "north" hi ho
  tak "red coin" hi ho
  go "north" hi ho
--  inv "" hi ho
  getDesc ho

solveMaze hi ho [] = getDesc ho
solveMaze hi ho (dir:dirs) = go dir hi ho >> solveMaze hi ho dirs

tak = action "take"
go = action "go"
use = action "use"
inv = action "inv"

action typ name hi ho = getDesc ho >>= \desc -> putStrLn comm >> hPutStrLn hi comm >> hFlush hi >> return desc
  where comm = (typ ++ " " ++ name)

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