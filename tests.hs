import Test.HUnit
import Text.ParserCombinators.Parsec
import Lambda hiding (main)
import Control.Monad

completeparse :: String -> Exp
completeparse str =
  case parse lambdaexp "" str of
    Left _ -> undefined
    Right e -> toBruijn e

testcase1 :: Test
testcase1 = TestCase (assertEqual "for (id.id)"
                       (simplifyall $ completeparse "(\\x.x)(\\y.y)")
                       (completeparse "(\\x.x)")
                     )

main :: IO ()            
main = void (runTestTT (TestList [TestLabel "id.id" testcase1]))
