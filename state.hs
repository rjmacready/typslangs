
newtype State s a = State { runState :: s -> (a, s) }

fromStoAndS :: Int -> (String, Int)
fromStoAndS c | c `mod` 5 == 0 = ("foo", c+1)
              | otherwise = ("bar", c+1)

stateIntString :: State Int String
stateIntString = State fromStoAndS

instance Monad (State s) where
         return a = State $ \s -> (a, s)
         m >>= k = State $ \s -> let (a, s') = runState m s
                                 in runState (k a) s'

-- stateFunction :: State [a] ()
-- stateFunction = do
--                         x <- pop
--                         pop
--                         push x
                        
-- push :: State [a] ()
-- push a = State $ \as -> ((), a:as)
