module Main where
import Data.List
import Data.List.Split


data Var = MakeVar String deriving (Read, Show)
data Decl = MakeDecl String deriving (Read, Show)

data IntExp
  	= IVar String
  	| ICon Int
  	| Add IntExp IntExp
  	| Sub IntExp IntExp
  	| Mul IntExp IntExp
  	| Div IntExp IntExp
  	deriving (Read, Show)

data BoolExp
	 = ILT IntExp IntExp
	 | ILTEQ IntExp IntExp
	 | IEQ IntExp IntExp
	 | IGT IntExp IntExp
	 | IGTEQ IntExp IntExp
	 deriving (Read, Show)

data Stmt
	 = Begin String [Stmt]	-- Change String to Decl
	 | Assign Var IntExp
	 | Read Var IntExp
	 | Write IntExp
	 | While BoolExp [Stmt]		-- Orginially a single statement (a begin one I Imagine)
	 deriving (Read, Show) 
{-
	 | IfThenElse BoolExp Stmt Stmt
	 | While BoolExp Stmt
	 deriving (Read, Show) 
-}

-- ################################################ --
--                 Tree Builder                     --
-- ################################################ --
-- ******************* --
-- Building up Bool --
-- ******************* --

boolExpEval :: [Tokens] -> BoolExp -- Boolean Expressions must be between parenthesis. 2 IntExp's separated by a BooleanOperator
boolExpEval (_:xs) =
	boolExpEvalHelper (getBoolArgsHelper2 xs) (intExpEval (getBoolArgs xs)) (intExpEval (getParensArgs(drop ((getBoolArgsHelper xs) + 1) xs)))

boolExpEvalHelper :: String -> IntExp -> IntExp -> BoolExp
boolExpEvalHelper op x y 
	| op == "<" = ILT x y
	| op == "<=" = ILTEQ x y
	| op == "==" = IEQ x y
	| op == ">=" = IGTEQ x y
	| op == ">" = IGT x y


-- ******************* --
-- Building up Stmts --
-- ******************* --

statementBuilder :: [Tokens] -> Stmt
statementBuilder ((KeyWord "begin"):(Identifier x):xs) =
	(Begin x (statementArrayBuilder xs))

statementArrayBuilder :: [Tokens] -> [Stmt]
statementArrayBuilder ((KeyWord "end"):xs) = []
statementArrayBuilder ((KeyWord "begin"):xs) =
	(statementBuilder ((KeyWord "begin"):xs)) : (statementArrayBuilder (getArgsFrom (KeyWord "end") xs))
statementArrayBuilder ((KeyWord "read"): xs) = 				-- For a Read Statement
	(Read (MakeVar "Garry Var") (intExpEval (getLineArgs (drop 1 xs)))) : statementArrayBuilder (drop ((getLineArgsHelper xs) + 1) xs)
statementArrayBuilder ((KeyWord "write"):xs) =  			-- For a Write Statement
	(Write (intExpEval (getLineArgs xs))) : statementArrayBuilder (drop ((getLineArgsHelper xs) + 1) xs)
statementArrayBuilder ((Identifier x):(AssignmentOperator y):xs) = 	-- For am Assign Operation
	(Assign (MakeVar x) (intExpEval (getLineArgs xs))) : statementArrayBuilder (drop ((getLineArgsHelper xs) + 1) xs)


-- ******************* --
-- Building up IntExps --
-- ******************* --

	-- When given a list of tokens (all of which make up an IntExp) will return an IntExp
intExpEval :: [Tokens] -> IntExp
intExpEval (x:[]) = iVarOrICon x
intExpEval (x:xs) 
	| (head xs) == (MathematicalOperator "*") = intExpEvalHelper (Mul (iVarOrICon x) (iVarOrICon (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "/") = intExpEvalHelper (Div (iVarOrICon x) (iVarOrICon (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "-") = intExpEvalHelper (Sub (iVarOrICon x) (iVarOrICon (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "+") = intExpEvalHelper (Add (iVarOrICon x) (iVarOrICon (xs!!1))) (drop 2 xs)

	-- Helper to the above function (allows for intExp's to be part of a bigger intExp) NB. As of yet this doesn't support order of operations
intExpEvalHelper :: IntExp -> [Tokens] -> IntExp
intExpEvalHelper y [] = y
intExpEvalHelper y ((MathematicalOperator "*"):xs) = intExpEvalHelper (Mul y (iVarOrICon (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "/"):xs) = intExpEvalHelper (Div y (iVarOrICon (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "-"):xs) = intExpEvalHelper (Sub y (iVarOrICon (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "+"):xs) = intExpEvalHelper (Add y (iVarOrICon (head xs))) (drop 1 xs)

	-- Used for evaluating lead nodes in an intExp tree (determines whether a value is either a number or a variable
iVarOrICon :: Tokens -> IntExp
iVarOrICon (Identifier x) = IVar x
iVarOrICon (Number x) = ICon x



-- ******************* --(drop ((getLineArgsHelper xs) + 1) xs)
--   General Helpers   --
-- ******************* --

	-- Helper function that returns a small list of tokens (all the tokens up to an end of line character)
getLineArgs :: [Tokens] -> [Tokens]
getLineArgs toks =
	take (getLineArgsHelper toks) toks

	-- Helper function to the above helper function (returns a take index)
getLineArgsHelper :: [Tokens] -> Int
getLineArgsHelper (EndOfLine _:_) = 0
getLineArgsHelper (_:xs) = 1 + getLineArgsHelper xs

getParensArgs :: [Tokens] -> [Tokens]
getParensArgs toks =
	take (getParensArgsHelper toks) toks

getParensArgsHelper :: [Tokens] -> Int
getParensArgsHelper (Parens _:_) = 0
getParensArgsHelper (_:xs) = 1 + getParensArgsHelper xs

getBoolArgs :: [Tokens] -> [Tokens]
getBoolArgs toks =
	take (getBoolArgsHelper toks) toks

getBoolArgsHelper :: [Tokens] -> Int
getBoolArgsHelper ((BooleanOperator _):_) = 0
getBoolArgsHelper (_:xs) = 1 + getBoolArgsHelper xs

getBoolArgsHelper2 :: [Tokens] -> String
getBoolArgsHelper2 ((BooleanOperator x):xs) = x
getBoolArgsHelper2 (_:xs) = getBoolArgsHelper2 xs

getArgsFrom :: Tokens -> [Tokens] -> [Tokens]
getArgsFrom y (x:xs) 
	| x == y = xs
	| otherwise = (getArgsFrom y xs)



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
	| x == ";" = (EndOfLine x) : (lexicalAnalyser xs)									--End of Line Char
	| x == ")" || x == "(" = (Parens x) : (lexicalAnalyser xs)								--Parenthesis
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
	 let xs = (lexicalAnalyser (myDelimiter x))
     	 putStr (show (statementBuilder xs))



