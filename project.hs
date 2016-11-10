import qualified Data.Map.Strict as Map

-- follows pdf 2.1
newtype Ident = Ident String deriving (Show);

data NExpr a = Var Ident | Const a | Plus (NExpr a)  (NExpr a) | Minus (NExpr a)  (NExpr a) | 
	Times (NExpr a) (NExpr a) deriving (Show);
	
data BExpr a = AND (NExpr a)  (NExpr a) | OR (NExpr a) (NExpr a) | NOT (NExpr a) | 
	Gt (NExpr a) (NExpr a) | Eq (NExpr a) (NExpr a) deriving (Show);
	
data Command a = Assign Ident (NExpr a) | Input Ident | Print Ident | Empty Ident | Push Ident a | 
	Pop Ident Ident | Size Ident Ident | Seq [Command a] | Cond (BExpr a) (Command a) | Loop (BExpr a) (Command a) deriving (Show);


main :: IO ()
main = putStr ( show  (( Assign (Ident "X") ( Plus (Const (15::Int)) (Const (16::Int)) ) )::(Command Int)));

-- to do pdf 2.2 -- making show print original program
-- to do pdf 3   -- making pccts parse input to make read into Command

-- follows 4.1 -- better use lists and prelude function of lists of tuples or pairs

data VarType a = General a | Pila [a];
type SymRow a = (Ident , (VarType a));
type SymTable a = [SymRow a];



-- follows 4.2
