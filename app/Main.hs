module Main where

import System.IO
import Data.Char(toUpper)
import Ch8


main :: IO ()
main = do
  inh <- openFile "/home/kayvan/dev/workspaces/workspace-xst/createProject.sh" ReadMode
  outh <- openFile "/home/kayvan/output.txt" WriteMode
  mainloop inh outh
  hClose inh
  hClose outh

mainloop :: Handle -> Handle ->  IO ()
mainloop inh outh = do
  ineof <- hIsEOF inh
  if ineof
    then return ()
    else do
      inpStr <- hGetLine inh
      hPutStrLn outh ( map toUpper inpStr)
      mainloop inh outh
  
