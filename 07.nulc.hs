
data Term = 
     Var Int
     | Abs Term
     | App Term Term


instance Show Term where
         show (Var x) = show x
         show (Abs e) = "\\" ++ "." ++ show e
         show (App l r) = show l ++ " " ++ show r

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

isval :: a -> Term -> Bool
isval _ (Abs _) = True
isval _ _ = False

eval1 ctx (App (Abs t12) v2) | isval ctx v2 = 
      Just (termSubstTop v2 t12)
eval1 ctx (App v1 t2) | isval ctx v1 = do
      t2' <- eval1 ctx t2;
      return (App v1 t2')
eval1 ctx (App t1 t2) = do
      t1' <- eval1 ctx t1; 
      return (App t1' t2)
eval1 _ _ = Nothing


eval ctx t = let t' = eval1 ctx t
             in case t' of
                Nothing -> t
                Just t'' -> eval ctx t''

