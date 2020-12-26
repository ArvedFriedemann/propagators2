{-# LANGUAGE NoImplicitPrelude #-}
module Data.MutList
    ( MutList
    , new
    , add
    , write
    , read
    ) where

import "base" Prelude hiding ( read )
import "base" Data.IORef
import "base" System.IO.Unsafe

import "vector" Data.Vector.Mutable qualified as Vec


newtype MutList a  = MkML (IORef (Int, Vec.IOVector a))

new :: IO (MutList a)
new = do
    v <- Vec.new 4
    MkML <$> newIORef (0, v)

add :: a -> MutList a -> IO Int
add a (MkML ref) = atomicModifyIORef' ref (unsafePerformIO . add')
  where
    add' (lastIndex, v) = do
      let l = Vec.length v
      v' <- if lastIndex >= l
            then Vec.grow v (l*2)
            else pure v
      Vec.write v' lastIndex a
      pure ((succ lastIndex, v'), lastIndex)

write :: Int -> a -> MutList a -> IO (MutList a)
write i a ml@(MkML ref) = atomicModifyIORef' ref (unsafePerformIO . write')
  where
    write' x@(_, v) = (x, ml) <$ Vec.write v i a

read :: Int -> MutList a -> IO a
read i (MkML ref) = (flip Vec.read i =<<) . fmap snd . readIORef $ ref
