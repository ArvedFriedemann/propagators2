module Control.Util where

import "base" Control.Arrow


(<**<) :: Monad m => m a -> (a -> m b) -> m a
m <**< f = m >>= (uncurry (>>) . (f &&& pure))

safeHead :: [a] -> [a]
safeHead [] = []
safeHead (x : _) = [x]
