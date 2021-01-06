module Data.Terms.TermId where

import Data.Terms.Terms
import Data.Terms.TermFunctions
import Control.Propagator.Class

data TermId w where
  EMPTY :: TermId w
  DIRECT :: w -> TermId w
  APPLLEFT :: TermId w -> TermId w
  APPLRIGHT :: TermId w -> TermId w
  COPY :: w -> TermId w -> TermId w
  BOUND :: w -> TermConst -> TermId w
  deriving (Eq, Ord, Show)

class Direct f where
  direct :: w -> f w

instance Direct TermId where
  direct = DIRECT

class Bound w i | i -> w where
  bnd :: w -> TermConst -> i

instance Bound w (TermId w) where
  bnd = BOUND

instance (Std w) => Identifier (TermId w) (TermSet (TermId w))

instance PosTermId (TermId w) where
  appLeft = APPLLEFT
  appRight = APPLRIGHT

instance CopyTermId w (TermId w) where
  --copy listId origTerm
  copy = COPY
  copyTermIdContents (COPY w i) = Just (w,i)
  copyTermIdContents _ = Nothing
