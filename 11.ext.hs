import Data.List

data Type =
     TBool
     | TNat
     | TFunc Type Type
     | TA
     | TUnit

instance Eq Type where
         TBool == TBool = True
         TNat == TNat = True
         TFunc a1 a2 == TFunc b1 b2 = a1 == b1 && a2 == b2
         _ == _ = False

data Term = 
     Var Int
     | Abs Type Term
     | App Term Term
     | TZero
     | TSucc Term
     | TPred Term
     | TIsZero Term
     | TTrue
     | TFalse
     | IfThenElse Term Term Term
     | Unit
     | Tas Term Type
     | TLet Term Term

instance Show Type where
         show TBool = "Bool"
         show TNat = "Nat"
         show (TFunc t1 t2) = show t1 ++ "->" ++ show t2

instance Show Term where
         show (Var x) = show x
         show (Abs _ e) = "\\" ++ "." ++ show e
         show (App l r) = show l ++ " " ++ show r
         show TZero = "[0]"
         show (TSucc s)  = "+" ++ show s
         show (TPred p)  = "-" ++ show p
         show (TIsZero z) = "?" ++ show z
         show TTrue = "True"
         show TFalse = "False"
         show (IfThenElse c t f) = 
              "(if " ++ show c ++ " " ++ show t ++ " " ++ show f ++")"

data Binding = 
             NameBind
           | VarBind Type

type Context = [Binding]

addbinding :: Context -> Binding -> Context
addbinding ctx bind = bind:ctx

getbinding :: Context -> Int -> Type
getbinding ctx x = if length ctx > x+1 then
                        error ("The Var " ++ show x ++ " is not defined!")
                     else
                        case ctx !! x of
                        VarBind t -> t
                        otherwise -> error "Not a VarBind!"

typeof :: Context -> Term -> Type
typeof _ Unit = TUnit
typeof _ TZero = TNat
typeof _ TTrue = TBool
typeof _ TFalse = TBool

typeof ctx (Tas x t) = let tx = typeof ctx x in
                       if tx == t then
                          t
                       else
                          error "Ascription fail"

typeof ctx (Var x) = getbinding ctx x

typeof ctx (App l r) = 
                     let tr = typeof ctx r in
                     case typeof ctx l of
                     TFunc la le ->
                           if la == tr then
                              le
                           else
                               error "The type of the right side of the application and the type of the argument of the left side dont match!" 
     
typeof ctx (Abs t1 t2) = TFunc t1 (typeof (addbinding ctx (VarBind t1)) t2)
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
isval _ (Abs _ _) = True
isval _ TTrue = True
isval _ TFalse = True
isval ctx a | isnumber ctx a = True
isval _ _ = False

-- ---------------------------------

shift_c :: Int -> Int -> Term -> Term
shift_c c d (Abs t e) = Abs t (shift_c (1 + c) d e)
shift_c c d (App l r) = App (shift_c c d l) (shift_c c d r)
shift_c c d (Var k) | k < c  = Var k
                    | otherwise = Var (k + d)

shift_c c d (TIsZero e) = TIsZero (shift_c c d e)
shift_c c d (TSucc e) = TSucc (shift_c c d e)
shift_c c d k@TZero = k
-- FIXME we need to handle all the terms

shift_c c d e = error ("Fail: shift_c " ++ show c ++ " " ++ show d ++ " " ++ show e)

-- ---------------------------------

shift = shift_c 0

-- ---------------------------------

subst :: Int -> Term -> Term -> Term
subst j s (Abs t t') = Abs t (subst (j + 1) (shift 1 s) t')
subst j s (App l r) = App (subst j s l) (subst j s r)
subst j s (Var k) | k == j = s
                  | otherwise = Var k

subst j s (TIsZero e) = TIsZero (subst j s e)
subst j s (TSucc e) = TSucc (subst j s e)
subst j s k@TZero = k
-- FIXME we need to handle all the terms

subst j s e = error ("subst " ++ show j ++ " " ++ show s ++ " " ++ show e)

-- ---------------------------------

termSubstTop s t = shift (-1) (subst 0 (shift 1 s) t)

-- ---------------------------------

eval1 :: Context -> Term -> Maybe Term

eval1 ctx (Tas v2 _) | isval ctx v2 = Just v2

eval1 ctx (Tas t2 t) = do
      t2' <- eval1 ctx t2
      return (Tas t2' t)

eval1 ctx (App (Abs _ t12) v2) | isval ctx v2 = 
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

eval1 ctx l@(TLet _ _) = 
      let e = integrate ctx l in
      eval1 ctx e

eval1 _ _ = Nothing

--- Derived forms

integrate :: Context -> Term -> Term

integrate ctx (TLet varvalue expr) = App (Abs (typeof ctx varvalue) expr) varvalue
integrate _ a = a

---

eval ctx t = let t' = eval1 ctx t
             in case t' of
                Nothing -> t
                Just t'' -> eval ctx t''

