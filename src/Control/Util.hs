module Control.Util where


(<**<) :: (Monad m) => m a -> (a -> m b) -> m a
m <**< m' = do
  r <- m
  m' r
  return r

safeHead :: [a] -> [a]
safeHead [] = []
safeHead (x : _) = [x]

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

tryMaybe :: (a -> Maybe b) -> (a -> b) -> a -> b
tryMaybe fkt1 _ (fkt1 -> Just x) = x
tryMaybe _ fkt2 x = fkt2 x
