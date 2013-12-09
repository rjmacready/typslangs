
data Type =
     TBool

--  | TOther

instance Eq Type where
    TBool == TBool = True

--  _ == _ = False

data Term = 
       TTrue
     | TFalse
     | IfThenElse Term Term Term

typeof :: a -> Term -> Type
typeof _ TTrue = TBool
typeof _ TFalse = TBool
typeof ctx (IfThenElse t1 t2 t3) =
       case typeof ctx t1 of
       TBool -> let t2' = typeof ctx t2 in
                let t3' = typeof ctx t3 in
                    if t2' == t3' then
                       t2'
                    else
                        error "The types of the two branches of the if dont match!"
--       _ -> error "The condition is not of type bool"

isval :: a -> Term -> Bool
isval _ TTrue = True
isval _ TFalse = True
isval _ _ = False

eval1 ctx (IfThenElse TTrue t2 t3) = do
      return t2

eval1 ctx (IfThenElse TFalse t2 t3) = do
      return t3

eval1 ctx (IfThenElse t1 t2 t3) = do
      t1' <- eval1 ctx t1; 
      return (IfThenElse t1' t2 t3)

eval1 _ _ = Nothing


eval ctx t = let t' = eval1 ctx t
             in case t' of
                Nothing -> t
                Just t'' -> eval ctx t''

