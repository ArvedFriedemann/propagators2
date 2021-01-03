module Control.Util where

import "base" Control.Arrow


(<**<) :: Monad m => m a -> (a -> m b) -> m a
m <**< f = m >>= (uncurry (>>) . (f &&& pure))

safeHead :: [a] -> [a]
safeHead [] = []
safeHead (x : _) = [x]

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

tryMaybe :: (a -> Maybe b) -> (a -> b) -> a -> b
tryMaybe fkt1 _ (fkt1 -> Just x) = x
tryMaybe _ fkt2 x = fkt2 x
