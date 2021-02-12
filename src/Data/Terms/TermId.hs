module Data.Terms.TermId where

import "base" Control.Monad

import "hashable" Data.Hashable

import "this" Data.Terms.Terms
import "this" Data.Terms.TermFunctions
import "this" Control.Propagator
import "this" Control.Combinator.Logics
import "this" Data.Typed
import "this" Data.Some
import "this" Data.Lattice


data TermId where
    EMPTY :: TermId
    DIRECT :: forall w. Std w => w -> TermId
    APPLLEFT :: TermId -> TermId
    APPLRIGHT :: TermId -> TermId
    COPY :: forall w. Std w => w -> TermId -> TermId
    BOUND :: forall w. Std w => w -> TermConst -> TermId

instance Hashable TermId where
    hashWithSalt s EMPTY = hashWithSalt s ()
    hashWithSalt s (DIRECT w) = s `hashWithSalt` (1 :: Int) `hashWithSalt` w
    hashWithSalt s (APPLLEFT i) = s `hashWithSalt` (2 :: Int) `hashWithSalt` i
    hashWithSalt s (APPLRIGHT i) = s `hashWithSalt` (3 :: Int) `hashWithSalt` i
    hashWithSalt s (COPY w i) = s `hashWithSalt` (4 :: Int) `hashWithSalt` w `hashWithSalt` i
    hashWithSalt s (BOUND w c) = s `hashWithSalt` (5 :: Int) `hashWithSalt` w `hashWithSalt` c

instance Eq TermId where
    EMPTY == EMPTY = True
    DIRECT w == DIRECT w' = w =~= w'
    APPLLEFT x == APPLLEFT x' = x == x'
    APPLRIGHT x == APPLRIGHT x' = x == x'
    COPY w t == COPY w' t' = w=~=w' && t==t'
    BOUND w t == BOUND w' t' = w=~=w' && t==t'
    _==_ = False

constrOrd :: TermId -> Int
constrOrd EMPTY = 0
constrOrd (DIRECT _) = 1
constrOrd (APPLLEFT _) = 2
constrOrd (APPLRIGHT _) = 3
constrOrd (COPY _ _) = 4
constrOrd (BOUND _ _) = 5

instance Ord TermId where
    compare EMPTY EMPTY = EQ
    compare (DIRECT w)  (DIRECT w') = compareTyped w w'
    compare (APPLLEFT x)  (APPLLEFT x') = compare x x'
    compare (APPLRIGHT x)  (APPLRIGHT x') = compare x x'
    compare (COPY w t)  (COPY w' t') = compareTyped w w' <> compare t t'
    compare (BOUND w t)  (BOUND w' t') = compareTyped w w' <> compare t t'
    compare x y = compare (constrOrd x) (constrOrd y)

instance Show TermId where
    show EMPTY = "EMPTY"
    show (DIRECT x) = "DIRECT "++ show x
    show (APPLLEFT x) = "APPLLEFT "++ show x
    show (APPLRIGHT x) = "APPLRIGHT "++ show x
    show (COPY x y) = "COPY "++ show x ++ " " ++ show y
    show (BOUND x y) = "BOUND "++ show x ++ " " ++ show y


instance HasValue TermId (Some Std) where
    toValue (Some s) = DIRECT s
    fromValue (DIRECT s) = Just (Some s)
    fromValue _ = Nothing
class Bound i where
    bound :: forall w. (Std w) => w -> TermConst -> i

instance Bound TermId where
    bound = BOUND

instance Identifier TermId (TermSet TermId)

instance PosTermId TermId where
    appLeft = APPLLEFT
    appRight = APPLRIGHT

instance CopyTermId TermId where
    copy = COPY
    --copyTermIdContents (COPY w i) = Just (w,i)
    --copyTermIdContents _ = Nothing

instance (MonadProp m) => Promoter TermId (TermSet TermId) m where
  promoteAction t = void $ promoteTerm t
