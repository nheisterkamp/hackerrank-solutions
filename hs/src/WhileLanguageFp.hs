-- http://www.haskell.org/haskellwiki/Parsing_a_simple_imperative_language
-- Describes a subset of the WHILE language.
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Data.Maybe
import Data.List


-- 3. Data structures
data BBinOp = And | Or
           deriving (Show)
data RBinOp = Greater | Less
           deriving (Show)

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           deriving (Show)

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
           deriving (Show)

data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
           deriving (Show)

-- 4. Lexer
languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                     , "<", ">", "and", "or", "not"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
braces     = Token.braces     lexer -- parses braces {}
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

eol :: Parser ()
eol = void (char '\n') <|> eof


-- 5. Main parser
whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   parens statement
          <|> sequenceOfStmt
          <|> braces statement

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip


-- 6. Expressions
aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft]
             , [Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
             ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
             , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft]
             , [Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)


-- 7. Notes
parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r



data ScopeRecord = ScopeRecord {getKey :: String, getValue :: Integer}
                 deriving (Ord, Eq)
showScopeRecord :: ScopeRecord -> String
showScopeRecord record = getKey record ++ " " ++ (show $ getValue record)
instance Show ScopeRecord where show = showScopeRecord

data Scope = Scope {getScopeRecords :: [ScopeRecord]}
showScope :: Scope -> String
showScope scope = concat $ intersperse "\n" $ map (show) (
    sort $ getScopeRecords scope)
instance Show Scope where show = showScope

newScope :: Scope
newScope = Scope []

getRecordsWithoutKey :: [ScopeRecord] -> String -> [ScopeRecord]
getRecordsWithoutKey records key = filter (\rec -> getKey rec /= key) records

setScopeValue scope key value = Scope $ [ScopeRecord key value] ++ records
  where records = getRecordsWithoutKey records' key
        records' = getScopeRecords scope

getScopeValue :: Scope -> String -> Integer
getScopeValue scope key = getValue $ getScopeRecord' scope key

getScopeRecord :: Scope -> String -> Maybe ScopeRecord
getScopeRecord scope key = find (\rec -> getKey rec == key) (getScopeRecords scope)

getScopeRecord' :: Scope -> String -> ScopeRecord
getScopeRecord' scope key =
  case getScopeRecord scope key of
    Just record -> record
    Nothing -> error ("Var not found in scope: " ++ key)


evalAExpr :: AExpr -> Scope -> Integer
evalAExpr (Var name) scope = getScopeValue scope name
evalAExpr (IntConst contents) _ = contents
evalAExpr (Neg contents) scope = - evalAExpr contents scope
evalAExpr (ABinary op a b) scope = op' a' b'
  where op' = evalABinOp op
        a' = evalAExpr a scope
        b' = evalAExpr b scope

evalBExpr :: BExpr -> Scope -> Bool
evalBExpr (BoolConst True) _ = True
evalBExpr (BoolConst False) _ = False
evalBExpr (Not expr) scope = not $ evalBExpr expr scope
evalBExpr (BBinary op a b) scope = op' a' b'
  where op' = evalBBinOp op
        a' = evalBExpr a scope
        b' = evalBExpr b scope
evalBExpr (RBinary op a b) scope = op' a' b'
  where op' = evalRBinOp op
        a' = evalAExpr a scope
        b' = evalAExpr b scope

evalBBinOp :: BBinOp -> Bool -> Bool -> Bool
evalBBinOp (And) = (&&)
evalBBinOp (Or) = (&&)

evalRBinOp :: RBinOp -> Integer -> Integer -> Bool
evalRBinOp (Greater) = (>)
evalRBinOp (Less) = (<)

evalABinOp :: ABinOp -> (Integer -> Integer -> Integer)
evalABinOp (Add) = (+)
evalABinOp (Subtract) = (-)
evalABinOp (Multiply) = (*)
evalABinOp (Divide) = div

evalStmt :: Stmt -> Scope -> Scope
evalStmt (Seq stmts) scope = foldl (\sc st -> evalStmt st sc) scope stmts
evalStmt (Assign name expr) scope = setScopeValue scope name (evalAExpr expr scope)
evalStmt (If expr a b) scope = evalStmt (if expr' then a else b) scope
  where expr' = evalBExpr expr scope
evalStmt (While expr a) scope =
  if evalBExpr expr scope
    then evalStmt (While expr a) (evalStmt a scope)
    else scope


code :: [String]
code = codeMinMax

codeFact :: [String]
codeFact = [
    "fact := 1 ;"
  , "val := 10000 ;"
  , "cur := val ;"
  , "mod := 1000000007 ;"
  , ""
  , "while ( cur > 1 )"
  , "  do"
  , "   {"
  , "      fact := fact * cur ;"
  , "      fact := fact - fact / mod * mod ;"
  , "      cur := cur - 1"
  , "   } ;"
  , ""
  , "cur := 0"
  ]

codeMinMax :: [String]
codeMinMax = [
    "a := 10 ;"
  , "b := 100 ;"
  , ""
  , "if ( a < b ) then"
  , "    {"
  , "        min := a ;"
  , "        max := b"
  , "    }"
  , "else {"
  , "    min := b ;"
  , "    max := a"
  , "    }"
  , ""
  ]

main :: IO ()
main = do
  --code' <- getContents
  let code' = unlines codeFact
  solution $
    if null code'
      then unlines code
      else code'

solution :: String -> IO ()
solution code' = do
  let ast = parseString $ code'
      result = evalStmt ast newScope

  print result
