module Tests where

import Test.HUnit
import Main hiding (main)
import Control.Monad (void)


testSimplId :: Test
testSimplId = TestCase $ assertEqual
  "Simplifies the identity"
  (Lambda (Var 1))
  (simplifyall $ App (Lambda (Var 1)) (Lambda (Var 1)))

main :: IO ()            
main = void $ runTestTT $ TestList
       [ TestLabel "Identity" testSimplId
       ]
