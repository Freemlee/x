module Main where
import Data.List
import Data.Maybe
import Data.List.Split

{- 	Garry Sharp
	0801585s
	Haskell Interpreter for a Small Custom Language (Garry Script)
	
	The following code/documentation is entirely my own work. Other than consulting wiki's or discussing the exercise with peers for general 	 ideas/approaches, the work in its entirity is my own. 
	
	IMPORTANT INFORMATION BEFORE COMPILING

	Before Running please install the Data.List.Split library via cabal (cabal install split), this may require a cabal update to be done first 		(cabal update)

	################################
	Program Grammar
	################################

	There are a few pre-requisites for the language which will be detailed below:

		- Each boolean expression must be kept between parenthesis eg. (x + y < 6/x). Boolean expressions are whitespace insensitive
		- All read and write expressions must be followed by a semi-solon.
		- Variable names may contain numbers but cannot start with them eg. var5 is okay but 5thVar is not.
		- Assignment operators can be written as either = or :=
		- Comments must be surrounded by '#' They may carry onto new lines.
		- Comments cannot be placed within a statement eg while (x < 7) # comment # begin is not allowed. It must be after each of the 				statements args. In this case: while (x < 7) begin #do something#. This is because internally comments are treated as part of 				a sequence of statements (They are a statement in their own right.
		- Any statement/s that are part of a while/ifthenelse statement must be surrounded by begin/end blocks

	################################
	Extensions
	################################

	Here are a list of the extensions I've added:
	
		- Include comments in the code.
		- Doubles are supported in the AST
		
-}

type NewVar = String
data Val = Int | Double
data Var = DecVar String deriving (Read, Show)
data Decl = MakeDecl String deriving (Read, Show)

data IntExp
  	= IVar String
  	| ICon Int
	| IDoub Double 
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
	 = Begin [String] [Stmt]		-- Change String to Decl
	 | Assign Var IntExp
	 | Read Var IntExp
	 | Write IntExp
	 | While BoolExp Stmt		-- Orginially a single statement (a begin one I Imagine)
	 | IfThenElse BoolExp Stmt Stmt
	 deriving (Read, Show) 
{-
	 | IfThenElse BoolExp Stmt Stmt
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
statementBuilder ((KeyWord "begin"):(Array x):xs) =
	(Begin x (statementArrayBuilder xs))
statementBuilder ((KeyWord "begin"):xs) =
	(Begin [] (statementArrayBuilder xs))



statementArrayBuilder :: [Tokens] -> [Stmt]
statementArrayBuilder ((KeyWord "end"):xs) = []
statementArrayBuilder ((KeyWord "begin"):xs) =
	(statementBuilder ((KeyWord "begin"):xs)) : (statementArrayBuilder (getArgsFrom (KeyWord "end") xs))

statementArrayBuilder ((KeyWord "read"):(Identifier x): xs) = 				-- For a Read Statement
	(Read (DecVar x) (intExpEval (getLineArgs xs))) : statementArrayBuilder (drop ((getLineArgsHelper xs) + 1) xs) --Maybe remove + 1 ??

statementArrayBuilder ((KeyWord "write"):xs) =  			-- For a Write Statement
	(Write (intExpEval (getLineArgs xs))) : statementArrayBuilder (drop ((getLineArgsHelper xs) + 1) xs)

statementArrayBuilder ((Identifier x):(AssignmentOperator y):xs) = 	-- For an Assign Operation
	(Assign (DecVar x) (intExpEval (getLineArgs xs))) : statementArrayBuilder (drop ((getLineArgsHelper xs) + 1) xs)

statementArrayBuilder ((KeyWord "while"):xs) =				-- Had to change Parens ")" to KeyWord "begin"
	(While (boolExpEval (getArgsTo (KeyWord "begin") xs)) (statementBuilder (getArgsFrom (Parens ")") xs))) : (statementArrayBuilder (getArgsFrom 	 (KeyWord "end") xs))

statementArrayBuilder ((Comment _):xs) =				-- For a comment
	statementArrayBuilder xs

statementArrayBuilder ((KeyWord "if"):xs) =				-- For an If then else statement
	(IfThenElse (boolExpEval (getArgsTo (KeyWord "begin") xs)) (statementBuilder (getArgsFrom (KeyWord "then") xs)) 
	(statementBuilder (getArgsFrom (KeyWord "else") xs))) : (statementArrayBuilder (getArgsFrom (KeyWord "end") (getArgsFrom (KeyWord "end") xs)))


-- ******************* --
-- Building up IntExps --
-- ******************* --

	-- When given a list of tokens (all of which make up an IntExp) will return an IntExp
intExpEval :: [Tokens] -> IntExp
intExpEval (x:[]) = intExpLeaf x
intExpEval (x:xs) 
	| (head xs) == (MathematicalOperator "*") = intExpEvalHelper (Mul (intExpLeaf x) (intExpLeaf (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "/") = intExpEvalHelper (Div (intExpLeaf x) (intExpLeaf (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "-") = intExpEvalHelper (Sub (intExpLeaf x) (intExpLeaf (xs!!1))) (drop 2 xs)
	| (head xs) == (MathematicalOperator "+") = intExpEvalHelper (Add (intExpLeaf x) (intExpLeaf (xs!!1))) (drop 2 xs)

	-- Helper to the above function (allows for intExp's to be part of a bigger intExp) NB. As of yet this doesn't support order of operations
intExpEvalHelper :: IntExp -> [Tokens] -> IntExp
intExpEvalHelper y [] = y
intExpEvalHelper y ((MathematicalOperator "*"):xs) = intExpEvalHelper (Mul y (intExpLeaf (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "/"):xs) = intExpEvalHelper (Div y (intExpLeaf (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "-"):xs) = intExpEvalHelper (Sub y (intExpLeaf (head xs))) (drop 1 xs)
intExpEvalHelper y ((MathematicalOperator "+"):xs) = intExpEvalHelper (Add y (intExpLeaf (head xs))) (drop 1 xs)

	-- Used for evaluating lead nodes in an intExp tree (determines whether a value is either a number or a variable
intExpLeaf :: Tokens -> IntExp
intExpLeaf (Identifier x) = IVar x
intExpLeaf (Number x) = ICon x
intExpLeaf (Floating x) = IDoub x



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

getArgsTo :: Tokens -> [Tokens] -> [Tokens]
getArgsTo y (x:xs)
	| x == y = []
	| otherwise = x : (getArgsTo y xs)


-- ################################################ --
--                 Lexical Analyser                 --
-- ################################################ --

data Tokens
	= KeyWord String
	| Array [String]	 --putStr (show xs)
	| BooleanOperator String
	| MathematicalOperator String
	| AssignmentOperator String
	| Number Int
	| Floating Double
	| Comment String
	| Identifier String
	| EndOfLine String
	| Parens String
	deriving (Eq, Read, Show)

lexicalAnalyser :: [String] -> [Tokens]
lexicalAnalyser [] = []
lexicalAnalyser (x:xs)
	| elem x ["begin","read","write","end","while","do","if","then","else"] = (KeyWord x) : (lexicalAnalyser xs) 		--KeyWord
	| elem x ["=","<",">"] && nextIsEquals (head xs) = (BooleanOperator (x ++ (head xs))) : (lexicalAnalyser (skip xs))	--BooleanOperater
	| elem x ["<",">"] = (BooleanOperator x) : (lexicalAnalyser xs)								--BooleanOperater
		-- Maybe add for times equals
	| elem x ["*","-","+","/"] = (MathematicalOperator x) : (lexicalAnalyser xs)						--MathematicalOperater
	| x == ":" && nextIsEquals (head xs) = (AssignmentOperator (x ++ (head xs))) : (lexicalAnalyser (skip xs))		--AssignmentOperater
	| x == "=" = (AssignmentOperator x) : (lexicalAnalyser xs)								--AssignmentOperater
	| x == ";" = (EndOfLine x) : (lexicalAnalyser xs)									--End of Line Char
	| x == ")" || x == "(" = (Parens x) : (lexicalAnalyser xs)								--Parenthesis
	| x == "#" =  Comment (unwords (take ((commentDrop xs) - 1) xs)) : lexicalAnalyser (drop (commentDrop xs) xs)		--Comments
	| x == "[" = Array (buildArray xs) : lexicalAnalyser (drop (arrayDrop xs) xs)						--Array declarations
	| elem (head x) ['0'..'9'] && elem '.' x = (Floating (read x :: Double)) : (lexicalAnalyser xs)				--Floating Point
	| elem (head x) ['0'..'9'] = (Number (read x :: Int)) : (lexicalAnalyser xs)						--Numbers
	| otherwise = (Identifier x) : (lexicalAnalyser xs)

-- Helpers

buildArray :: [String] -> [String]
buildArray ("]":_) = []
buildArray (x:xs) = x : buildArray xs

arrayDrop :: [String] -> Int
arrayDrop (x:xs) =
	if x == "]"
		then 1
		else 1 + arrayDrop xs

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
	
int2string :: Int -> String
int2string x =
	show x


myDelimiter :: String -> [String]
myDelimiter xs =
	-- Delimit the program (removing the delimiters listed in the first argument of splitOneOf and filtering out black (""))
	-- Filter (\x -> x /= "")-- 
	filter (\x -> (not (elem x ["\n","\t","\r"," ",",",""]))) (split (oneOf "#<:=>+-()[]/,*;\n\r\t ") xs)
	
-- ******************* --
--     Interpreter	   --
-- ******************* --


type Env = [SubEnv]
type SubEnv = [(NewVar, Int)]

--Interpret Statements (for begin blocks)
{-
interpret :: Stmt -> Env -> IO()
interpret (Begin decs stmts) =


interpretStatement :: Stmt -> Env -> IO()
interpretStatement (Write x) env = do
	if (isJust (reduceIntExp x env))
		then putStr (show (fromJust(reduceIntExp x env)))
		else putStr ("could not be resolved")
-}			

--Reduce a BoolExp

reduceBoolExpHelper :: IntExp -> IntExp -> Env -> Bool
reduceBoolExpHelper x y env
	|(isNothing (reduceIntExp x env)) == True = False
	|(isNothing (reduceIntExp y env)) == True = False
	|otherwise = True

reduceBoolExp :: BoolExp -> Env -> Maybe Bool
reduceBoolExp (ILT x y) env = do
	if (reduceBoolExpHelper x y env) == True
		then Just ((fromJust (reduceIntExp x env)) < (fromJust (reduceIntExp y env)))
		else Nothing
reduceBoolExp (ILTEQ x y) env = do
	if (reduceBoolExpHelper x y env) == True
		then Just ((fromJust (reduceIntExp x env)) <= (fromJust (reduceIntExp y env)))
		else Nothing
reduceBoolExp (IEQ x y) env = do
	if (reduceBoolExpHelper x y env) == True
		then Just ((fromJust (reduceIntExp x env)) == (fromJust (reduceIntExp y env)))
		else Nothing
reduceBoolExp (IGT x y) env = do
	if (reduceBoolExpHelper x y env) == True
		then Just ((fromJust (reduceIntExp x env)) > (fromJust (reduceIntExp y env)))
		else Nothing
reduceBoolExp (IGTEQ x y) env = do
	if (reduceBoolExpHelper x y env) == True
		then Just ((fromJust (reduceIntExp x env)) >= (fromJust (reduceIntExp y env)))
		else Nothing

--Reduce an IntExp

reduceIntExp :: IntExp -> Env -> Maybe Int
reduceIntExp (ICon x) _ = Just x
reduceIntExp (IVar x) myenv = 
	if isNothing(mylookup x myenv)
		then Nothing
		else mylookup x myenv
reduceIntExp (Mul x y) myenv = do
	let x' = reduceIntExp x myenv
	let y' = reduceIntExp y myenv
		in
			if (isNothing x') || (isNothing y')
				then Nothing
				else Just (fromJust(x') * fromJust(y'))
reduceIntExp (Add x y) myenv = do
	let x' = reduceIntExp x myenv
	let y' = reduceIntExp y myenv
		in
			if (isNothing x') || (isNothing y')
				then Nothing
				else Just (fromJust(x') + fromJust(y'))

reduceIntExp (Div x y) myenv = do
	let x' = reduceIntExp x myenv
	let y' = reduceIntExp y myenv
		in
			if (isNothing x') || (isNothing y')
				then Nothing
				else Just (div (fromJust(x')) (fromJust(y')))

reduceIntExp (Sub x y) myenv = do
	let x' = reduceIntExp x myenv
	let y' = reduceIntExp y myenv
		in
			if (isNothing x') || (isNothing y')
				then Nothing
				else Just (fromJust(x') - fromJust(y'))
		
-- Get a value from the environment
mylookup :: String -> Env -> Maybe Int
mylookup x [] = Nothing
mylookup x ([]:zs) =
	mylookup x zs
mylookup x ((y:ys):zs) 
	| x == (fst y) = Just (snd y)
	| otherwise = mylookup x (ys:zs)
	
-- Update the environment
myupdate :: String -> Int -> Env -> Env
myupdate x y (z:zs) =
	if isNothing(mylookup x (z:zs))					-- Checks to see if it needs to add or update a variable/
		then (myupdateNew x y z):zs
		else myupdateExisting x y (z:zs)
		
myupdateNew :: String -> Int -> SubEnv -> SubEnv	-- Adds a new variable to the current scope (head of the environment)
myupdateNew x y z =
	(x,y):z

myupdateExisting :: String -> Int -> Env -> Env		-- Updates the first variable that it comes across (most recent scope)
myupdateExisting x y [] = [[(x,y)]] 
myupdateExisting x y (z:zs) = 
	if (localEnvExists x z)							-- Necessary to stop all variables in all scopes being changes is x used several times.
		then ((myupdateExistingHelper x y z) : zs)
		else (z : (myupdateExisting x y zs))
	
localEnvExists :: String -> SubEnv -> Bool		
localEnvExists x [] = False
localEnvExists x (y:ys) =
	if x == (fst y)
		then True
		else False
		
myupdateExistingHelper :: String -> Int -> SubEnv -> SubEnv		-- Loops through the scopes (SubEnv) and returns an updated scope (SubEnv)
myupdateExistingHelper x y [] = []
myupdateExistingHelper x y (z:zs)
	| x == (fst z) = (x,y) : zs
	| otherwise = z : (myupdateExistingHelper x y zs)

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
	 putStr ("\n\t Program Tokenised..\n" ++ (show xs) ++ "\n")
	 let theEnv = [[("a",9),("b",4)],[("c",20),("d",1)],[("e",16)]] :: Env
	 let testStmt = head (statementArrayBuilder xs)
	 --interpretStatement testStmt theEnv
	 putStr "Cheese"




