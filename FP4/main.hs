module Main where
import Data.List.Split

{-
let lowers = ['a'..'z']
let uppers = ['A'..'Z']
let numbers = [0..9]
let booleanOperators = ["<",">"]
let assignmentOperators = "="

data IntExp
  = IVar Var
  | ICon Int
  | Add IntExp IntExp
  | Sub IntExp IntExp
  | Mul IntExp IntExp
  | Div IntExp IntExp
  deriving (Read, Show)
  
data BoolExp
	 = LT IntExp IntExp
	 | EQ IntExp IntExp
	 | GT IntExp IntExp
	 deriving (Read, Show)

data Stmt
	 = Begin [Decl] [Stmt]
	 | Assign Var IntExp
	 | Read Var IntExp
	 | Write IntExp
	 | IfThenElse BoolExp Stmt Stmt
	 | While BoolExp Stmt
	 deriving (Read, Show)

-}
data Tokens
	= KeyWord String
	| Identifier String
	| BooleanOperator Char
	| AssignmentOperator String
	| Comment String
	| Number Int
	deriving (Read, Show)





lexicalAnalyser :: String -> [Tokens]
lexicalAnalyser [] = []








myDelimiter :: String -> [String]
myDelimiter xs =
	-- Delimit the program (removing the delimiters listed in the first argument of splitOneOf and filtering out black (""))
	--filter (\x -> x /= "")-- 
	filter (\x -> (not (elem x ["\n","\t","\r"," ","",";"]))) (split (oneOf "<=>+-/*;\n\r\t ") xs)



-- IntVar's --

{- These may be either a variable name, an integer or 
arithmetic operation (that should reduce to an integer -}

isIntExp :: String -> Bool
isIntExp str =
  if elem '+' str || elem '-' str || elem '/' str || elem '*' str
       then True
       else False
    
main = do
	 x <- readFile "exampleProg.txt"
     	 putStr (show (myDelimiter x)) 



