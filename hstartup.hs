import System.IO
import System.Process
import System.Environment
import Control.Monad
import Control.Exception

-- start new processes
-- reads programs out of a text file and starts them
-- can also split parameters for those programs
main::IO ()
main = do
  [s] <- getArgs
  f   <- readFile s
  let paths = lines f 
      x = map head (map (splitOnChar (==',')) paths)
      xs= map (drop 1) (map (splitOnChar (==',')) paths)
      in startAllProcs x xs

splitOnChar::(Char->Bool)->String->[String]
splitOnChar p s = case dropWhile p s of
  "" -> []
  s'-> w : splitOnChar p s''
    where (w, s'') = break p s'

startAllProcs::[String]->[[String]]->IO ()
startAllProcs [] _ = return ()
startAllProcs (pr:prs) (param:params)= do
  startProc pr param
  startAllProcs prs params

try'::IO a -> IO (Either IOException a)
try' = try

startProc::String->[String]->IO ()
startProc pr params= do
  result <- try' $ createProcess (proc pr params)
  case result of
    Left ex -> putStrLn $ "error starting: "++ show ex
    Right (_,_,_,p) -> putStrLn "started ok"
