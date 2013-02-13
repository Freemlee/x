module Main where

data IntExp
  = IVar String
  | ICon Int
  | Add IntExp IntExp
  | Sub IntExp IntExp
  | Mul IntExp IntExp
  | Div IntExp IntExp
  deriving (Read, Show)

data Tokens
	= KeyWord String
	| BooleanOperator String
	| MathematicalOperator String
	| AssignmentOperator String
	| Number Int
	| Comment String
	| Identifier String
	deriving (Read, Show)

intExpHandler :: [Tokens] -> [Tokens]
intExpHandler (x:xs) =
	(x:xs)

main = intExpHandler (show [Number 1, MathematicalOperator "+", Number 5])

