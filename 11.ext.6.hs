import Data.List

data Type =
     TBool
     | TNat
     | TFunc Type Type
     | TA
     | TUnit
     | TProd Type Type
     | TTuple [Type]
     | TRecord [(String, Type)]

instance Eq Type where
         TBool == TBool = True
         TNat == TNat = True
         TFunc a1 a2 == TFunc b1 b2 = a1 == b1 && a2 == b2
         TA == TA = True
         TUnit == TUnit = True
         TProd a1 a2 == TProd b1 b2 = a1 == b1 && a2 == b2
         TTuple t1 == TTuple t2 | (length t1) == (length t2) = 
                all id (map (\(l, r) -> l == r) (zip t1 t2))
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
     | Pair Term Term
     | FstProj Term
     | SndProj Term
     | Tuple [Term]
     | Record [(String, Term)]
     | NthProj Int Term
     | LthProj String Term

instance Show Type where
         show TBool = "Bool"
         show TNat = "Nat"
         show (TFunc t1 t2) = show t1 ++ "->" ++ show t2
         show (TProd t1 t2) = show t1 ++ " x " ++ show t2
         show (TTuple t) = "{" ++ (foldl (++) "" (intersperse "," $ map show t)) ++ "}"
         show (TRecord t) = "{" ++ (foldl (++) "" (intersperse "," $ map (\(label, ty)-> label ++ "=" ++ show ty) t)) ++ "}"

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
         show (Pair t1 t2) = 
              "{" ++ show t1 ++ ", " ++ show t2 ++ "}"
         show (FstProj t) =
              show t ++ ".1"
         show (SndProj t) =
              show t ++ ".2"
         show (Tuple t) = "{" ++ (foldl (++) "" (intersperse "," $ map show t)) ++ "}"
         show (Record t) = "{" ++ (foldl (++) "" (intersperse "," $ map (\(label, v)-> label ++ "=" ++ show v) t)) ++ "}"

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

typeof ctx (Pair t1 t2) = TProd (typeof ctx t1) (typeof ctx t2)

typeof ctx (FstProj t) = case typeof ctx t of
                         TProd t1 t2 -> t1
                         _ -> error "Argument to first projection is not a product type!"

typeof ctx (SndProj t) = case typeof ctx t of
                         TProd t1 t2 -> t2
                         _ -> error "Argument to second projection is not a product type!"

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

-- Tuple

typeof ctx (Tuple t) = TTuple (map (typeof ctx) t)

typeof ctx (NthProj i t) = case typeof ctx t of
                                -- This will crash if i isn't at tt
                                TTuple tt -> tt !! i
                                _ -> error "Argument to first projection is not a tuple!"

-- Records 

typeof ctx (Record r) = TRecord (map (\(label, value) -> (label, typeof ctx value)) r)

typeof ctx (LthProj i r) = case typeof ctx r of
                                -- This will crash if i isn't at tt
                                TRecord tt ->
                                        case (lookup i tt) of
                                             Nothing -> error "label not on record!"
                                             Just ty -> ty
                                _ -> error "Argument to first projection is not a tuple!"



isnumber _ TZero = True
isnumber _ (TSucc _) = True
isnumber _ _ = False

isval :: a -> Term -> Bool
isval ctx (Pair v1 v2) | isval ctx v1 && isval ctx v2 = True
isval ctx (Tuple vs) | all (isval ctx) vs = True
isval ctx (Record vs) | all (isval ctx) (map (\(l, v) -> v) vs) = True

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


eval1 ctx (FstProj (Pair v1 v2)) | isval ctx v1 && isval ctx v2 = Just v1
eval1 ctx (SndProj (Pair v1 v2)) | isval ctx v1 && isval ctx v2 = Just v2

eval1 ctx (FstProj p@(Pair _ _)) = do
      p' <- eval1 ctx p
      return (FstProj p')

eval1 ctx (SndProj p@(Pair _ _)) = do
      p' <- eval1 ctx p
      return (SndProj p')

eval1 ctx (Pair t1 t2) | not $ isval ctx t1 = do
      t1' <- eval1 ctx t1
      return (Pair t1' t2)

eval1 ctx (Pair v1 t2) | isval ctx v1 = do
      t2' <- eval1 ctx t2
      return (Pair v1 t2')

-- Tuples

eval1 ctx (NthProj i (Tuple t)) | all (isval ctx) t = do
        if (length t <= i) then
           Nothing
        else
           return (t !! i)

eval1 ctx (NthProj i t) = do
      t' <- (eval1 ctx t)
      return (NthProj i t')

eval1 ctx arg@(Tuple t) = do
      let iofvalue = findIndex (not . (isval ctx)) t in
          case iofvalue of
               Nothing -> return arg
               Just i -> 
                    let (values, evalme:rest) = splitAt i t in do
                    evalme' <- eval1 ctx evalme
                    return (Tuple (foldl (++) [] [values, [evalme'], rest]))


-- Records

eval1 ctx (LthProj i r@(Record t)) | isval ctx r = do
      let v = lookup i t in
          case v of
               Nothing -> Nothing
               Just v -> return v

eval1 ctx (LthProj i t) = do
      t' <- (eval1 ctx t)
      return (LthProj i t')

eval1 ctx arg@(Record t) = do
      let iofvalue = findIndex (\(k, v) -> not $ isval ctx v) t in
          case iofvalue of
               Nothing -> return arg
               Just i -> 
                    let (values, (key, evalme):rest) = splitAt i t in do
                    evalme' <- eval1 ctx evalme
                    return (Record (foldl (++) [] [values, [(key, evalme')], rest]))

--

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

