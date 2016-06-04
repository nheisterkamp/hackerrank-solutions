-- http://www.haskell.org/haskellwiki/Parsing_a_simple_imperative_language
-- Describes a subset of the WHILE language.
import System.IO
import Control.Monad
import Text.Parsec

-- import Data.Maybe
-- import Data.List






testExpr :: String
testExpr = "22 * 79 - 21"

main :: IO ()
main = do
  expr <- getContents
  solution $
    if null expr
      then expr
      else testExpr

solution :: String -> IO ()
solution expr = do
  let result = expressionParser expr
  -- let ast = parse $ expr
  --     result = evalStmt ast newScope

  print result
