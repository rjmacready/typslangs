
data Term = 
       TTrue
     | TFalse
     | IfThenElse Term Term Term
     | TZero
     | TSucc Term
     | TPred Term
     | TIsZero Term

isnumber _ TZero = True
isnumber _ (TSucc _) = True
isnumber _ _ = False

isval :: a -> Term -> Bool
isval _ TTrue = True
isval _ TFalse = True
isval ctx a | isnumber ctx a = True
isval _ _ = False

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

eval1 ctx (TPred (TSucc TZero)) = Just TZero

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

