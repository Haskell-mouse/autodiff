module Main (main) where

import Lib
import StagedTests
import Sized.Staged
import Env
import Sized
import UnSized.StagedTest

main :: IO ()
main = do 
  let x = 12.5
  let y = 2
  let tan = (der' 12.5 4 5)
  putStrLn (show tan)

