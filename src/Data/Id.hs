{-# LANGUAGE StrictData #-}
module Data.Id
    ( Id
    , MonadId(..)
    ) where

import "base" GHC.Generics
import "base" Data.Unique
import "base" Data.String

import "deepseq" Control.DeepSeq


newtype Id = Id [Either String Unique]
  deriving newtype (Eq, Ord, Semigroup, Monoid, Generic)

instance NFData Id

instance IsString Id where
    fromString = Id . pure . Left

instance Show Id where
    showsPrec d (Id parts)
        = showParen (d >= 10)
        $ showString "Id "
        . shows (either id (show . hashUnique) <$> parts)

class MonadId m where
    newId :: m Id

instance MonadId IO where
    newId = Id . pure . Right <$> newUnique
