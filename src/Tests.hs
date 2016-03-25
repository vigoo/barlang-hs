import           Language.Barlang.Test.Parser
import           Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Running checks..."
  check printedTypeIsParsable
  check printedExpressionIsParsable
  check printedStatementIsParsable
