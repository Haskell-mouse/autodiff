module Main (main) where

import Lib
import StagedTests
import Sized.Staged
import Env
import Sized

main :: IO ()
main = do 
    net <- setupSizedNet
    return $ netFromMap . $$(testAdStaged reverseADEndoStaged) $ net
    print "read the expanded splice!"
