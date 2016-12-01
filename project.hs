-- follows pdf 2.1
newtype Ident = Ident String;
instance Show(Ident) where
	show (Ident v) = v
instance Eq(Ident) where
	(==) (Ident v) (Ident w) = v == w	
data NExpr a = Var Ident | Const a | Plus (NExpr a)  (NExpr a) | Minus (NExpr a)  (NExpr a) | Times (NExpr a) (NExpr a) ;
instance (Show a) => Show (NExpr a) where
	show (Const a) = show a
	show (Var v) = show v
	show (Plus m1 m2) = show m1 ++ " + " ++ show m2 
	show (Minus m1 m2) =  show m1 ++ " - " ++ show m2 
	show (Times m1 m2) = show m1 ++ " * " ++ show m2 
data BExpr a = And (BExpr a)  (BExpr a) | Or (BExpr a) (BExpr a) | Not (BExpr a) | 
	Gt (NExpr a) (NExpr a) | Eq (NExpr a) (NExpr a);
instance (Show a) => Show (BExpr a) where
	show (Gt m1 m2) = show m1 ++ " > " ++ show m2
	show (Eq m1 m2) = show m1 ++ " = " ++ show m2
	show (Not v) = show v
	show (And m1 m2) = show m1 ++ " AND " ++ show m2 
	show (Or m1 m2) =  show m1 ++ " OR " ++ show m2 
data Command a = Assign Ident (NExpr a) | Input Ident | Print Ident | Empty Ident | Push Ident (NExpr a) | 
	Pop Ident Ident | Size Ident Ident | Seq [Command a] | Cond (BExpr a) (Command a) (Command a) | Loop (BExpr a) (Command a);
{-
instance (Show a) => Show (Command a) where
	show (Assign v m1) = show v ++ " := " ++ show m1
	show (Push m1 m2) = "PUSH " ++ show m1 ++ show m2
	show (Pop m1 m2) = "POP " ++ show m1 ++ show m2
	show (Size m1 m2) = "SIZE " ++ show m1 ++ show m2
	show (Input v) = "INPUT " ++ show v
	show (Print v) = "PRINT " ++ show v
	show (Empty v) = "EMPTY " ++ show v
	show (Seq list) = foldl (\y x -> y ++ show x ++ "\n") "" list
	show (Cond b m1 m2) = "IF " ++ show b ++ " THEN " ++ show m1 ++ " ELSE " ++ show m2 ++ " END "
	show (Loop b m1 ) = "WHILE " ++ show b ++ " DO " ++ show m1 ++ " END "
-}
showC :: Show a => String -> Command a -> String	
showC s (Assign v m1) = s ++ show v ++ " := " ++ show m1
showC s (Push m1 m2) = s ++ "PUSH " ++ show m1 ++ " " ++ show m2
showC s (Pop m1 m2) = s ++ "POP " ++ show m1 ++ " " ++ show m2
showC s (Size m1 m2) = s ++ "SIZE " ++ show m1 ++ show m2
showC s (Input v) = s ++ "INPUT " ++ show v
showC s (Print v) = s ++ "PRINT " ++ show v
showC s (Empty v) = s ++ "EMPTY " ++ show v
showC s (Seq list) = foldl (\y x -> y ++ showC s x ++ "\n") "" list
showC s (Cond b m1 m2) = s ++ "IF " ++ show b ++ " THEN\n" ++ showC (s ++ "  ") m1 ++ s ++ "ELSE\n" ++ showC (s ++ "  ") m2 ++ s ++ "END "
showC s (Loop b m1 ) = s ++ "WHILE " ++ show b ++ " DO\n" ++ showC (s ++ "  ") m1 ++ s ++ "END"
{-
main3 = putStr ( show  ((Seq [ (Input (Ident "X")  ) , ( Input (Ident "Y") ), ( Assign (Ident "X") ( Plus (Const (15::Int)) (Const (16::Int)) ) )])::(Command Int)));
main1 = putStr ( showC ""  ((Seq [  ( Input  (Ident "X")  )  ,  ( Empty  (Ident "P")  )  ,  ( Loop (Or (Gt ( (Var (Ident "X") )  ) ( Const 0) ) ( Eq ( (Var (Ident "X") )  ) ( Const 0)) ) ( Seq [  ( Input  (Ident "Y")  )  ,  ( Push ( (Ident "P")  ) (  (Var (Ident "Y") ) ) )  ] ) )  ,  ( Assign ( (Ident "S")  ) ( Const 0) )  ,  ( Size  (Ident "P")  (Ident "L")  )  ,  ( Loop (Gt ( (Var (Ident "L") )  ) ( Const 0) ) ( Seq [  ( Pop  (Ident "P")  (Ident "Y")  )  ,  ( Assign ( (Ident "S")  ) ( Plus ( (Var (Ident "S") )  ) (  (Var (Ident "Y") ) )) )  ,  ( Assign ( (Ident "L")  ) ( Minus ( (Var (Ident "L") )  ) ( Const 1)) )  ] ) )  ,  ( Print  (Ident "S")  )  ] )::(Command Int)));
main2 = putStr ( showC ""  ((Seq [  ( Input  (Ident "X")  )  ,  ( Input  (Ident "Y")  )  ,  ( Cond (Or (Gt ( (Var (Ident "X") )  ) ( Const 0) ) ( Or (Eq ( (Var (Ident "X") )  ) ( Const 0) ) ( Not (Gt (Const 0 ) (  (Var (Ident "Y") ) )))) ) ( Seq [  ( Assign ( (Ident "Z")  ) ( Const 1) )  ,  ( Loop (Gt ( (Var (Ident "X") )  ) (  (Var (Ident "Y") ) ) ) ( Seq [  ( Assign ( (Ident "X")  ) ( Minus ( (Var (Ident "X") )  ) ( Const 1)) )  ,  ( Assign ( (Ident "Z")  ) ( Times ( (Var (Ident "Z") )  ) (  (Var (Ident "Z") ) )) )  ] ) )  ]  ) ( Seq [  ( Assign ( (Ident "Z")  ) ( Const 0) )  ] ) )  ,  ( Print  (Ident "Z")  )  ] )::(Command Int)));
mainold =
	case ( eval (\x -> Nothing) (Gt ( Plus (Const (15::Int)) (Const (16::Int)) )(Const (17::Int)))) of
		Left x -> putStr x
		Right y -> putStr (show y) 
-}
-- follows 4.1
data VarType a = General a | Pila [a] deriving (Show);
getType :: (VarType a) -> String
getType (General x) = "general"
getType (Pila l) = "pila"
type SymRow a = (Ident , (VarType a));
type SymTable a = [SymRow a];
-- follows 4.2
class Evaluable e where
	eval :: (Num a, Ord a) => (Ident -> Maybe a) -> (e a) -> (Either String a)
	typeCheck :: (Ident -> String) -> (e a) -> Bool
instance Evaluable (NExpr) where
	typeCheck f (Const a) = True
	typeCheck f (Var v) = f v == "general"
	typeCheck f (Plus m1 m2)  = typeCheck f m1 && typeCheck f m2 
	typeCheck f (Minus m1 m2) = typeCheck f m1 && typeCheck f m2 
	typeCheck f (Times m1 m2) = typeCheck f m1 && typeCheck f m2 
	eval f (Const a) = Right a
	eval f (Var v) =  
		case f v of
			Just x -> Right x
			Nothing -> Left "undefined variable"
	eval f (Plus m1 m2) = 
		case (eval f m1, eval f m2) of
			(Right x, Right y) -> Right (x+y)
			(Right x, Left y) -> Left y
			(Left x, _) -> Left x
	eval f (Minus m1 m2) =
		case (eval f m1, eval f m2) of
			(Right x, Right y) -> Right (x-y)
			(Right x, Left y) -> Left y
			(Left x, _) -> Left x
	eval f (Times m1 m2) = 
		case (eval f m1, eval f m2) of
			(Right x, Right y) -> Right (x*y)
			(Right x, Left y) -> Left y
			(Left x, _) -> Left x
instance Evaluable (BExpr) where
	typeCheck f (Not v) = typeCheck f v
	typeCheck f (Gt m1 m2)  = typeCheck f m1 && typeCheck f m2 
	typeCheck f (Eq m1 m2)  = typeCheck f m1 && typeCheck f m2 
	typeCheck f (And m1 m2) = typeCheck f m1 && typeCheck f m2 
	typeCheck f (Or m1 m2)  = typeCheck f m1 && typeCheck f m2 
	eval f (Gt m1 m2) = 
		case (eval f m1, eval f m2) of
			(Right x, Right y) -> Right (if x > y then 1 else 0)
			(Right x, Left y) -> Left y
			(Left x, _) -> Left x
	eval f (Eq m1 m2) = 
		case (eval f m1, eval f m2) of
			(Right x, Right y) -> Right (if x == y then 1 else 0)
			(Right x, Left y) -> Left y
			(Left x, _) -> Left x
	eval f (And m1 m2) = 
		case (eval f m1, eval f m2) of
			(Right 1, Right 1) -> Right 1
			(_, Left y) -> Left y
			(Left x, _) -> Left x
			(_,_) -> Right 0			
	eval f (Or m1 m2) = 
		case (eval f m1, eval f m2) of
			(_, Left y) -> Left y
			(Left x, _) -> Left x
			(Right 1, _) -> Right 1
			(_, Right 1) -> Right 1
			(_,_) -> Right 0						
	eval f (Not v) =
		case (eval f v) of
			(Right x) -> Right (if x == 1 then 0 else 1)
			(Left x) -> Left x
getvalue :: SymTable a -> Ident -> (Either String (VarType a))
getvalue [] i = Left "undefined variable"
getvalue (x:xs) i = if (fst x) == i then (Right (snd x)) else getvalue xs i
gettype :: SymTable a -> Ident -> String
gettype s v = 
	case getvalue s v of
		Left k -> k
		Right k -> (getType k)
setvalue :: SymTable a -> Ident -> (VarType a) -> (SymTable a)
setvalue [] i v = [(i,v)] 
setvalue (x:xs) i v = if (fst x) == i then (i,v) : xs else x : (setvalue xs i v)

gettoppila :: VarType a -> a
gettoppila (Pila (x:xs)) = x
poppila :: VarType a -> VarType a
poppila (Pila (x:xs)) = Pila xs
pushpila :: VarType a -> a -> VarType a
pushpila (Pila xs) m = Pila (m :xs) 
lengthpila :: Num a => VarType a -> a
lengthpila (Pila (x:xs)) = 1 + lengthpila (Pila xs)

-- follows 4.3
evalaux :: (Num a, Ord a) => SymTable a -> (Ident -> Maybe a)
evalaux s x = case getvalue s x of
    Left err -> Nothing
    Right (General v) -> Just v
    _ -> Nothing
add :: (Num a, Ord a) => [a] -> ((Either String [a]),SymTable a, [a]) -> ((Either String [a]),SymTable a, [a])
add xs (Right o, s , l) = (Right (xs ++ o), s, l)
add xs (Left err, _ , _) = (Left err, [], [])
interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> ((Either String [a]),SymTable a, [a])
interpretCommand s ls (Assign v m1) =
	case ((typeCheck (\x -> gettype s x) m1) , (eval (evalaux s) m1)) of
		(True , Right ret) -> (Right [], (setvalue s v (General ret)), ls)
		(True , Left str) -> (Left str, [],[])
		(False , _) -> (Left "type error", [],[])
interpretCommand s l (Empty v) = (Left "", s,[]) -- to do
interpretCommand s l (Push m1 m2) = (Left "", s,[]) -- to do
interpretCommand s l (Pop m1 m2) = (Left "", s,[]) -- to do
interpretCommand s l (Size m1 m2) = 
	case (gettype s m1 , (getvalue s m1))of
	("pila", Right p) -> (Right [],setvalue s m2 (General (lengthpila p)),l)
	("pila", Left p) -> (Left p, [],[])
	("general",_) -> (Left "type error", [],[])
interpretCommand s [] (Input v) = (Left "empty stack", s, [])
interpretCommand s (x:xs) (Input v) = (Right [], setvalue s v (General x), xs)
interpretCommand s l (Print v) = 
	case (getvalue s v, gettype s v) of
		(Right (General out), "general") -> (Right [out], s, l)
		(Right out, err) -> (Left err , [],[])
		(Left err, _) -> (Left err , [],[])
interpretCommand s l (Seq []) = (Right [], s,l)      
interpretCommand s l (Seq (x:xs)) = case interpretCommand s l x of
    (Left err, _, _) -> (Left err, [], [])
    (Right out, sym, input) -> add out (interpretCommand sym input (Seq xs))
interpretCommand s l (Cond b m1 m2) =
    case ((typeCheck (\x -> gettype s x) b) , (eval (evalaux s) b)) of
    (True, Right 1) -> interpretCommand s l m1
    (True, Right _) -> interpretCommand s l m2
    (False, _) -> (Left "type error", [],[])
interpretCommand s l (Loop b m1 ) = 
    case ((typeCheck (\x -> gettype s x) b) , (eval (evalaux s) b)) of
    (True, Right 1) -> case interpretCommand s l m1 of
        (Left err, _ ,_) -> (Left err, [],[])
        (Right output, symbols, remaining_input) -> case interpretCommand symbols remaining_input (Loop b m1) of
              (Left err, _ ,_) -> (Left err, [],[])
              (Right output2, symbols2, remaining_input2) -> (Right (output ++ output2), symbols2, remaining_input2)
    (True, Right _) -> (Right [], s, l)
    (False, _) -> (Left "type error", [],[])

-- follows 4.4
interpretProgram:: (Num a,Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram input command = 
	case (interpretCommand [] input command) of
		(Left my_error, symbols, remaining_input) -> Left my_error
		(Right output, symbols, remaining_input) -> Right output

main = putStr $ show $ interpretProgram [1,1] ((Seq [  ( Input  (Ident "X")  )  ,  ( Input  (Ident "Y")  )  ,  ( Cond (Or (Gt ( (Var (Ident "X") )  ) ( Const 0) ) ( Or (Eq ( (Var (Ident "X") )  ) ( Const 0) ) ( Not (Gt (Const 0 ) (  (Var (Ident "Y") ) )))) ) ( Seq [  ( Assign ( (Ident "Z")  ) ( Const 1) )  ,  ( Loop (Gt ( (Var (Ident "X") )  ) (  (Var (Ident "Y") ) ) ) ( Seq [  ( Assign ( (Ident "X")  ) ( Minus ( (Var (Ident "X") )  ) ( Const 1)) )  ,  ( Assign ( (Ident "Z")  ) ( Times ( (Var (Ident "Z") )  ) (  (Var (Ident "Z") ) )) )  ] ) )  ]  ) ( Seq [  ( Assign ( (Ident "Z")  ) ( Const 0) )  ] ) )  ,  ( Print  (Ident "Z")  )  ] )::(Command Int));

{-| , que interpretaun AST per una mem` oria i una entrada donada i retorna una tripleta que
cont ́e a la primera component la llista amb totes les impressions o b ́e un
missatge d’error, i a la segona i la tercera component la mem`oria i l’entrada
respectivament despr ́es d’executar el codi.
4. Usant la funció anterior feu una funció 
interpretProgram:: (Num a,Ord a) => [a] -> Command a -> (Either String [a])
 , que avalua un codi complet per a una entrada donada.
Qualsevol programa o expressi ́o que contingui una subexpressi ́o que avalua
a error, tamb ́e avalua a error. S’ha de comunicar quin ha estat l’error:
“undefined variable” o “empty stack” o “type error”.
-}
