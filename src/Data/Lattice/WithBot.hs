{-# LANGUAGE StrictData #-}
module Data.Lattice.WithBot where

import "base" Data.Functor.Classes
import "base" Data.String ( IsString(..) )
import "base" Control.Applicative
import "base" Control.Monad
import "base" GHC.Exts ( IsList(..) )

import "this" Data.Lattice.Class


data WithBot a
    = SynthBot
    | NotBot a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
  
instance Eq1 WithBot where
    liftEq _ SynthBot SynthBot = True
    liftEq f (NotBot a) (NotBot b) = f a b
    liftEq _ _ _ = False
instance Ord1 WithBot where
    liftCompare _ SynthBot SynthBot = EQ
    liftCompare _ SynthBot _ = GT
    liftCompare _ _ SynthBot = LT
    liftCompare f (NotBot a) (NotBot b) = f a b
instance Show1 WithBot where
    liftShowsPrec _ _ _ SynthBot = showString "Bot"
    liftShowsPrec f _ d (NotBot a) = f d a
instance IsList a => IsList (WithBot a) where
    type Item (WithBot a) = Item a
    fromList = NotBot . fromList
    toList SynthBot = []
    toList (NotBot a) = toList a
instance IsString a => IsString (WithBot a) where
    fromString = NotBot . fromString

instance Applicative WithBot where
    pure = NotBot
    NotBot f <*> NotBot a = NotBot $ f a
    _ <*> _ = SynthBot
instance Alternative WithBot where
    empty = SynthBot
    a <|> SynthBot = a
    SynthBot <|> a = a
    _ <|> _ = SynthBot
instance Monad WithBot where
    SynthBot >>= _ = SynthBot
    NotBot a >>= f = f a
instance MonadPlus WithBot

instance Join a => Join (WithBot a) where
    (\/) = liftA2 (\/)
instance HasBot (WithBot a) where
    isBot SynthBot = True
    isBot (NotBot _) = False
    bot = SynthBot
instance Meet a => Meet (WithBot a) where
    (/\) = liftA2 (/\)
instance HasTop a => HasTop (WithBot a) where
    isTop SynthBot = False
    isTop (NotBot a) = isTop a
    top = pure top
instance Join a => BoundedJoin (WithBot a)
instance BoundedMeet a => BoundedMeet (WithBot a)
instance (Meet a, Join a) => Lattice (WithBot a)
instance (BoundedMeet a, Join a) => BoundedLattice (WithBot a)
