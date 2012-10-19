
-- Grammar Definition --

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
	 = Assign Var IntExp
	 | Read Var IntExp
	 | Write IntExp
	 | IfThenElse BoolExp Stmt Stmt
	 | While BoolExp Stmt
	 deriving (Read, Show)

data Program
	 = "begin" [Decl] [Stmt] "end"





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
  	 let tokenedLines = lines x
     in putStr (show tokenedLines)
