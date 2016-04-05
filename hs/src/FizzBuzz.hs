import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Monad (guard)
import Control.Applicative ((<$), (<*>))

-- This function should be pretty straightforward
m `divides` x = x `mod` m == 0

(~>) :: Integer -> String -> Integer -> Maybe String
(m ~> str) x = str <$ guard (m `divides` x)

-- A more readable version of this function would read like this
-- (m ~> str) x = if m `divides` x
--     then Just str
--     else Nothing

-- Taking advantage of Monoid instances for functions, Maybe and String (holy god!)
fizzBuzzOrNeither :: Integer -> Maybe String
fizzBuzzOrNeither = 3 ~> "fizz" <> 5 ~> "buzz"

-- Taking advantage of the Applicative instance for functions (I am pretty sure that's what's going on)
integerToFizzBuzzString :: Integer -> String
integerToFizzBuzzString = fromMaybe . show <*> fizzBuzzOrNeither

result = map integerToFizzBuzzString [1..]

main = print $ take 15 result

