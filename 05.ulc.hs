
import Data.List

data Term = Var String
          | Abs Term Term
          | App Term Term

instance Show Term where
         show (Var s) = s
         show (Abs v e) = "\\" ++ show v ++ "." ++ show e
         show (App l r) = show l ++ " " ++ show r

instance Eq Term where
         (Var a) == (Var b) = a == b
         _ == _ = False

-- 


is_value (Abs _ _) = True
is_value _ = False

-- 

subst (Var x) val (Var y)             | x == y = val 
subst (Var x) val r@(Var y)           | x /= y = r

subst (Var x) val r@(Abs (Var y) _)   | x == y = r

subst i@(Var x) val (Abs v@(Var y) e) | x /= y && not (elem v (free_vars val)) = Abs v (subst i val e)

subst i@(Var x) val (App l r)                  = App (subst i val l) (subst i val r)

-- 

-- lvars :: Term ([Term], [Term]) -> ([Term], [Term])
free_vars_aux v@(Var _) bounded | not $ elem v bounded = [v]
free_vars_aux v@(Var _) bounded | elem v bounded = []
free_vars_aux (App l r) bounded = union (free_vars_aux l bounded) 
                                        (free_vars_aux r bounded)

free_vars_aux (Abs v e) bounded = free_vars_aux e (union [v] bounded)

free_vars v = free_vars_aux v []

-- ////

class SimpleEval l a where
      eval :: l -> a -> a

data LcCallByValue = LcCallByValue

instance SimpleEval LcCallByValue Term where
      eval _ (App (Abs v e) r) | is_value r = subst v r e
      eval s (App l r) | is_value l = App l (eval s r)
      eval s (App l r) = App (eval s l) r
      -- Fail through
      eval _ a = error $ "No rule to reduce " ++ show a
