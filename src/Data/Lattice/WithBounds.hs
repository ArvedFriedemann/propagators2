{-# LANGUAGE StrictData #-}
module Data.Lattice.WithBounds where

import "base" Data.Functor.Compose
import "base" Data.Functor.Classes
import "base" Control.Applicative
import "base" Control.Monad

import "this" Data.Lattice.Class
import "this" Data.Lattice.WithTop
import "this" Data.Lattice.WithBot


newtype WithBounds a = WithBounds (Compose WithTop WithBot a)
  deriving newtype (Eq, Ord, Functor, Applicative, Eq1, Ord1, Foldable)
instance Show1 WithBounds where
    liftShowsPrec _ _ _ (WithBounds (Compose (SynthTop))) = showString "Top"
    liftShowsPrec _ _ _ (WithBounds (Compose (NotTop SynthBot))) = showString "Bot"
    liftShowsPrec f _ d (WithBounds (Compose (NotTop (NotBot a))))
        = showParen (d >= 10)
        $ showString "Value "
        . f 11 a
instance Show a => Show (WithBounds a) where
    showsPrec = liftShowsPrec showsPrec undefined
instance Monad WithBounds where
    WithBounds (Compose (NotTop (NotBot a))) >>= f = f a
    a >>= _ = undefined <$> a
instance Alternative WithBounds where
    empty = WithBounds . Compose $ SynthTop
    WithBounds (Compose (NotTop (SynthBot))) <|> _ = empty
    _ <|> WithBounds (Compose (NotTop (SynthBot))) = empty
    WithBounds (Compose SynthTop) <|> a = a
    a <|> WithBounds (Compose SynthTop) = a
    _ <|> _ = WithBounds . Compose . NotTop $ SynthBot
instance MonadPlus WithBounds

instance Traversable WithBounds where
    sequenceA (WithBounds (Compose (NotTop (NotBot f)))) = WithBounds . Compose . NotTop . NotBot <$> f
    sequenceA f = pure $ fmap undefined f
