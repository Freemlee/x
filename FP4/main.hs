module Main where
import Data.List.Split


--data Var = String
--data Decl = MakeDecl [Var] deriving (Read, Show)

data IntExp
  	= IVar String
  	| ICon Int
  	| Add IntExp IntExp
  	| Sub IntExp IntExp
  	| Mul IntExp IntExp
  	| Div IntExp IntExp
  	deriving (Read, Show)
{-
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
	 deriving (Read, Show) -}


-- ################################################ --
--                 Tree Builder                     --
-- ################################################ --
{-
treeBuilder :: [Tokens] -> Stmt
treeBuilder ((I a):xs) =  

keyWordEval :: String -> [Tokens] -> Stmt
ketWordEval "begin" (x:xs) = Begin x (treeBuilder xs)

baseIntExpEval :: Tokens -> IntExp
baseIntExpEval (Identifier x) = (Ivar x)
baseIntExpEval (Number x) = (Icon x)

opIntExpEval Token -> IntExp -> IntExp
opIntExpEval (MathematicalOperator "*") x y = Mul x y
opIntExpEval (MathematicalOperator "-") x y = Sub x y
opIntExpEval (MathematicalOperator "/") x y = Div x y
opIntExpEval (MathematicalOperator "+") x y = Add x y
-}


{-
stateMentBuilder :: [Tokens] -> [Stmt]
stateMentBuilder ((Keyword "end"):xs) = []
stateMentBuilder ((Keyword "read"):xs) = (Read (Var (xs!!0)) (IntExpEval (getLineArgs (drop 1 xs))))
stateMentBuilder ((Keyword "write"):xs) =  (Write (IntExpEval (getLineArgs xs)))
-}



intExpEval :: [Tokens] -> IntExp
intExpEval (x:xs) 
	| (head xs) == (MathematicalOperator "*") = intExpEvalHelper (Mul (iVarOrICon x) (iVarOrICon (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "/") = intExpEvalHelper (Div (iVarOrICon x) (iVarOrICon (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "-") = intExpEvalHelper (Sub (iVarOrICon x) (iVarOrICon (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "+") = intExpEvalHelper (Add (iVarOrICon x) (iVarOrICon (xs!!1))) (drop 2 xs)

intExpEvalHelper :: IntExp -> [Tokens] -> IntExp
intExpEvalHelper y [] = y
intExpEvalHelper y ((MathematicalOperator "*"):xs) = intExpEvalHelper (Mul y (iVarOrICon (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "/"):xs) = intExpEvalHelper (Div y (iVarOrICon (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "-"):xs) = intExpEvalHelper (Sub y (iVarOrICon (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "+"):xs) = intExpEvalHelper (Add y (iVarOrICon (head xs))) (drop 1 xs)

iVarOrICon :: Tokens -> IntExp
iVarOrICon (Identifier x) = IVar x
iVarOrICon (Number x) = ICon x

{-
getLineArgs :: [Tokens] -> [Tokens]
getLineArgs toks =
	take (getLineArgsHelper toks) toks

getLineArgsHelper :: [Tokens] -> Int
getLineArgsHelper (EndOfLine _:_) = 0
getLineArgsHelper (_:xs) = 1 + getLineArgs xs

-}

-- ################################################ --
--                 Lexical Analyser                 --
-- ################################################ --

data Tokens
	= KeyWord String
	| Array [String]
	| BooleanOperator String
	| MathematicalOperator String
	| AssignmentOperator String
	| Number Int
	| Comment String
	| Identifier String
	| EndOfLine String
	| Parens String
	deriving (Eq, Read, Show)

lexicalAnalyser :: [String] -> [Tokens]
lexicalAnalyser [] = []
lexicalAnalyser (x:xs)
	| elem x ["begin","read","write","end","while","do"] = (KeyWord x) : (lexicalAnalyser xs) 				--KeyWord
	| elem x ["=","<",">"] && nextIsEquals (head xs) = (BooleanOperator (x ++ (head xs))) : (lexicalAnalyser (skip xs))	--BooleanOperater
	| elem x ["<",">"] = (BooleanOperator x) : (lexicalAnalyser xs)								--BooleanOperater
	| elem x ["*","-","+","/"] = (MathematicalOperator x) : (lexicalAnalyser xs)						--MathematicalOperater
	| x == ":" && nextIsEquals (head xs) = (AssignmentOperator (x ++ (head xs))) : (lexicalAnalyser (skip xs))		--AssignmentOperater
	| x == "=" = (AssignmentOperator x) : (lexicalAnalyser xs)								--AssignmentOperater
	| x == ";" = (EndOfLine x) : (lexicalAnalyser xs)
	| x == ")" || x == "(" = (Parens x) : (lexicalAnalyser xs)
	| x == "#" =  Comment (unwords (take ((commentDrop xs) - 1) xs)) : lexicalAnalyser (drop (commentDrop xs) xs)		--Comments
	| elem x (map char2string ['0'..'9']) = (Number (read x :: Int)) : (lexicalAnalyser xs)					--Numbers
	| otherwise = (Identifier x) : (lexicalAnalyser xs)

-- Helpers

commentDrop :: [String] -> Int
commentDrop (x:xs) =
	if x == "#"
		then 1
		else 1 + commentDrop xs

nextIsEquals :: String -> Bool
nextIsEquals xs =
	if (head xs) == '=' then True else False

skip :: [String] -> [String]
skip (x:xs) =
	xs

char2string :: Char -> String
char2string x =
	x : []

myDelimiter :: String -> [String]
myDelimiter xs =
	-- Delimit the program (removing the delimiters listed in the first argument of splitOneOf and filtering out black (""))
	-- Filter (\x -> x /= "")-- 
	filter (\x -> (not (elem x ["\n","\t","\r"," ",""]))) (split (oneOf "#<:=>+-()/*;\n\r\t ") xs)

-- ################################################ --
-- ################################################ --
-- ################################################ --

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
     	 putStr (show (intExpEval (lexicalAnalyser (myDelimiter x))))



