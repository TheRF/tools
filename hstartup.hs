import System.IO
import System.Process
import System.Environment
import Control.Monad
import Control.Exception
import System.Directory

-- start new processes
-- reads programs out of a text file and starts them
-- can also split parameters for those programs
main::IO ()
main = do
  s <- getArgs
  bf <- doesFileExist $ head' s
  if (length s == 1) && bf
    then readConfig $ head' s
    else printHelp

head'::[String]->String
head' [] = ""
head' (x:xs) = x

readConfig::String->IO ()
readConfig s = do
  f <- readFile s
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

printHelp::IO ()
printHelp = do
  putStrLn "------- hstartup tool -------"
  putStrLn "usage: hstartup <config file>"