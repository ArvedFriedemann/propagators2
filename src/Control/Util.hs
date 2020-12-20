module Control.Util where

(<**<) :: (Monad m) => m a -> (a -> m b) -> m a
m <**< m' = do
  r <- m
  m' r
  return r
