module Data.Terms.TermId where

import "this" Data.Lattice
import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Propagator
import "this" Control.Combinator.Logics

import "base" Control.Monad

data TermId w
    = EMPTY
    | DIRECT w
    | APPLLEFT (TermId w)
    | APPLRIGHT (TermId w)
    | COPY w (TermId w)
    | BOUND w TermConst
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasValue TermId where
    toValue = DIRECT
    fromValue (DIRECT w) = Just w
    fromValue _ = Nothing

class Bound w i | i -> w where
    bound :: w -> TermConst -> i

instance Bound w (TermId w) where
    bound = BOUND

instance Std w => Identifier (TermId w) (TermSet (TermId w))

instance PosTermId (TermId w) where
    appLeft = APPLLEFT
    appRight = APPLRIGHT

instance Std w => CopyTermId w (TermId w) where
    copy = COPY
    copyTermIdContents (COPY w i) = Just (w,i)
    copyTermIdContents _ = Nothing

instance (MonadProp m, Std w) => Promoter (TermId w) (TermSet (TermId w)) m where
  promoteAction s t = void $ promoteTerm s t
