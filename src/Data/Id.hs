{-# LANGUAGE StrictData #-}
module Data.Id
    ( Id
    , MonadId(..)
    ) where

import "base" GHC.Generics
import "base" Data.Unique

import "deepseq" Control.DeepSeq


data Id = Id String {-# UNPACK #-} Unique
  deriving (Eq, Ord, Generic)
instance NFData Id

instance Show Id where
    showsPrec _ (Id n u)
        = showString n
        . showString "#"
        . shows (hashUnique u)

class MonadId m where
    newId :: String -> m Id
    newId' :: m Id
    newId' = newId ""

instance MonadId IO where
    newId n = Id n <$> newUnique
