
data Type =
     TBool
     | TNat

instance Eq Type where
         TBool == TBool = True
         TNat == TNat = True
         _ == _ = False

data Term = 
     Var Int
     | Abs Term
     | App Term Term
     | TZero
     | TSucc Term
     | TPred Term
     | TIsZero Term
     | TTrue
     | TFalse
     | IfThenElse Term Term Term

instance Show Type where
         show TBool = show "Bool"
         show TNat = show "Nat"

instance Show Term where
         show (Var x) = show x
         show (Abs e) = "\\" ++ "." ++ show e
         show (App l r) = show l ++ " " ++ show r
         show TZero = "[0]"
         show (TSucc s)  = "+" ++ show s
         show (TPred p)  = "-" ++ show p
         show (TIsZero z) = "?" ++ show z
         show TTrue = "True"
         show TFalse = "False"
         show (IfThenElse c t f) = 
              "(if " ++ show c ++ " " ++ show t ++ " " ++ show f ++")"


typeof :: a -> Term -> Type
typeof _ TZero = TNat
typeof _ TTrue = TBool
typeof _ TFalse = TBool
typeof ctx (TSucc t1) | typeof ctx t1 == TNat = TNat
typeof ctx (TPred t1) | typeof ctx t1 == TNat = TNat
typeof ctx (TIsZero t1) | typeof ctx t1 == TNat = TBool
typeof ctx (IfThenElse t1 t2 t3) =
       case typeof ctx t1 of
       TBool -> let t2' = typeof ctx t2 in
                let t3' = typeof ctx t3 in
                    if t2' == t3' then
                       t2'
                    else
                        error "The types of the two branches of the if dont match!"
       _ -> error "The condition is not of type bool"

isnumber _ TZero = True
isnumber _ (TSucc _) = True
isnumber _ _ = False

isval :: a -> Term -> Bool
isval _ (Abs _) = True
isval _ TTrue = True
isval _ TFalse = True
isval ctx a | isnumber ctx a = True
isval _ _ = False

-- ---------------------------------

shift_c :: Int -> Int -> Term -> Term
shift_c c d (Abs e) = Abs (shift_c (1 + c) d e)
shift_c c d (App l r) = App (shift_c c d l) (shift_c c d r)
shift_c c d (Var k) | k < c  = Var k
                    | otherwise = Var (k + d)

shift = shift_c 0


subst :: Int -> Term -> Term -> Term
subst j s (Abs t') = Abs (subst (j + 1) (shift 1 s) t')
subst j s (App l r) = App (subst j s l) (subst j s r)
subst j s (Var k) | k == j = s
                  | otherwise = Var k


termSubstTop s t = shift (-1) (subst 0 (shift 1 s) t)

-- ---------------------------------

eval1 ctx (App (Abs t12) v2) | isval ctx v2 = 
      Just (termSubstTop v2 t12)
eval1 ctx (App v1 t2) | isval ctx v1 = do
      t2' <- eval1 ctx t2;
      return (App v1 t2')
eval1 ctx (App t1 t2) = do
      t1' <- eval1 ctx t1; 
      return (App t1' t2)

eval1 ctx (IfThenElse TTrue t2 t3) = do
      return t2

eval1 ctx (IfThenElse TFalse t2 t3) = do
      return t3

eval1 ctx (IfThenElse t1 t2 t3) = do
      t1' <- eval1 ctx t1; 
      return (IfThenElse t1' t2 t3)

eval1 ctx (TSucc t1) = do
      t1' <- eval1 ctx t1
      return (TSucc t1')

eval1 ctx (TPred TZero) = Just TZero

eval1 ctx (TPred (TSucc n)) = Just n

eval1 ctx (TPred t1) = do
      t1' <- eval1 ctx t1
      return (TPred t1')

eval1 ctx (TIsZero TZero) = Just TTrue
eval1 ctx (TIsZero (TSucc _)) = Just TFalse

eval1 ctx (TIsZero t1) = do
      t1' <- eval1 ctx t1
      return (TIsZero t1')

eval1 _ _ = Nothing


eval ctx t = let t' = eval1 ctx t
             in case t' of
                Nothing -> t
                Just t'' -> eval ctx t''

