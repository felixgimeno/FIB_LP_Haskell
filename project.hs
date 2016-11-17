-- follows pdf 2.1
newtype Ident = Ident String;
instance Show(Ident) where
	show (Ident v) = v
data NExpr a = Var Ident | Const a | Plus (NExpr a)  (NExpr a) | Minus (NExpr a)  (NExpr a) | Times (NExpr a) (NExpr a) ;
instance (Show a) => Show (NExpr a) where
	show (Const a) = show a
	show (Var v) = show v
	show (Plus m1 m2) = show m1 ++ " + " ++ show m2 
	show (Minus m1 m2) =  show m1 ++ " - " ++ show m2 
	show (Times m1 m2) = show m1 ++ " * " ++ show m2 
data BExpr a = AND (BExpr a)  (BExpr a) | OR (BExpr a) (BExpr a) | NOT (BExpr a) | 
	Gt (NExpr a) (NExpr a) | Eq (NExpr a) (NExpr a);
instance (Show a) => Show (BExpr a) where
	show (Gt m1 m2) = show m1 ++ " > " ++ show m2
	show (Eq m1 m2) = show m1 ++ " = " ++ show m2
	show (NOT v) = show v
	show (AND m1 m2) = show m1 ++ " AND " ++ show m2 
	show (OR m1 m2) =  show m1 ++ " OR " ++ show m2 
	
data Command a = Assign Ident (NExpr a) | Input Ident | Print Ident | Empty Ident | Push Ident (NExpr a) | 
	Pop Ident Ident | Size Ident Ident | Seq [Command a] | Cond (BExpr a) (Command a) (Command a) | Loop (BExpr a) (Command a);
instance (Show a) => Show (Command a) where
	show (Assign v m1) = show v ++ " := " ++ show m1
	show (Push m1 m2) = "PUSH " ++ show m1 ++ show m2
	show (Pop m1 m2) = "POP " ++ show m1 ++ show m2
	show (Size m1 m2) = "SIZE " ++ show m1 ++ show m2
	show (Input v) = "INPUT " ++ show v
	show (Print v) = "PRINT " ++ show v
	show (Empty v) = "EMPTY " ++ show v
	show (Seq list) = foldl (\y x -> y ++ show x) "" list
	show (Cond b m1 m2) = "IF " ++ show b ++ " THEN " ++ show m1 ++ " ELSE " ++ show m2 ++ " END "
	show (Loop b m1 ) = "WHILE " ++ show b ++ " DO " ++ show m1 ++ " END "
	

main :: IO ()
main = putStr ( show  (( Assign (Ident "X") ( Plus (Const (15::Int)) (Const (16::Int)) ) )::(Command Int)));

-- to do pdf 2.2 -- making show print original program
-- to do pdf 3   -- making pccts parse input to make read into Command

-- follows 4.1 -- better use lists and prelude function of lists of tuples or pairs

data VarType a = General a | Pila [a];
type SymRow a = (Ident , (VarType a));
type SymTable a = [SymRow a];



-- follows 4.2
