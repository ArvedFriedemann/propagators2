{-# LANGUAGE StrictData #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Lattice.Class where

import "base" Data.Functor.Identity
import "base" Data.Functor.Compose
import "base" Data.Functor.Classes
import "base" Data.Monoid ( Dual(..), All(..), Any(..) )
import "base" Data.Coerce
import "base" Data.String ( IsString(..) )
import "base" Control.Applicative
import "base" Control.Monad
import "base" GHC.Exts ( IsList(..) )

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set


{- | A meet-semilattice

Instances of 'Meet' should satisfy the following:

[Associativity] @x ⋀ (y ⋀ z) = (x ⋀ y) ⋀ z@
[Commutativity] @x ⋀ y = y ⋀ x@
[Idempotency]  @x ⋀ x = x@
-}
class Meet l where
    (/\) :: l -> l -> l
    (/\) = (⋀)
    (⋀) :: l -> l -> l
    (⋀) = (/\)
    {-# MINIMAL (⋀) | (/\) #-}

class HasBot a where
    isBot :: a -> Bool
    default isBot :: Eq a => a -> Bool
    isBot = (bot ==)
    bot :: a
pattern Bot :: HasBot a => a
pattern Bot <- (isBot -> True)
  where Bot = bot

{- | A Bounded meet-semilattice

Instances of 'BoundedMeet' should satisfy the following:

[Identity] @x ⋀ top = x@
-}
class (Meet l, HasTop l) => BoundedMeet l where


{- | A join-semilattice

Instances of 'Join' should satisfy the following:

[Associativity] @x ⋁ (y ⋁ z) = (x ⋁ y) ⋁ z@
[Commutativity] @x ⋁ y = y ⋁ x@
[Idempotency]  @x ⋁ x = x@
-}
class Join l where
    (\/) :: l -> l -> l
    (\/) = (⋁)
    (⋁) :: l -> l -> l
    (⋁) = (\/)
    {-# MINIMAL (⋁) | (\/) #-}

class HasTop a where
    isTop :: a -> Bool
    default isTop :: Eq a => a -> Bool
    isTop = (top ==)
    top :: a
pattern Top :: HasTop a => a
pattern Top <- (isTop -> True)
  where Top = top

{- | A Bounded join-semilattice

Instances of 'BoundedJoin' should satisfy the following:

[Identity] @x ⋁ bot = x@
-}
class (Join l, HasBot l) => BoundedJoin l where


{- | A Lattice

Instances of 'Lattice' should satisfy the following:

[Absorption] @a ⋁ (a ⋀ b) = a = a ⋀ (a ⋁ b)@
-}
class (Meet l, Join l) => Lattice l

{- | A bounded Lattice

Instances of 'BoundedLattice' should satisfy the following:

[MeetIdentity] @bot ⋀ b = bot@
[JoinIdentity] @top ⋁ b = top@
-}
class (BoundedMeet l, BoundedJoin l, Lattice l) => BoundedLattice l

-- combinators

meetAll :: (Foldable f, BoundedMeet l) => f l -> l
meetAll = foldr (/\) top

joinAll :: (Foldable f, BoundedJoin l) => f l -> l
joinAll = foldr (\/) bot

-- instances

instance Meet l => Join (Dual l) where
    (\/) = coerce @(l -> l -> l) (/\)
instance HasTop l => HasBot (Dual l) where
    isBot = coerce @(l -> Bool) isTop
    bot = coerce @l top
instance Join l => Meet (Dual l) where
    (/\) = coerce @(l -> l -> l)  (\/)
instance HasBot l => HasTop (Dual l) where
    isTop = coerce @(l -> Bool) isBot
    top = coerce @l bot
instance BoundedJoin l => BoundedMeet (Dual l)
instance BoundedMeet l => BoundedJoin (Dual l)
instance Lattice l => Lattice (Dual l)
instance BoundedLattice l => BoundedLattice (Dual l)


newtype Monoidal a = Monoidal
    { getMonoidal :: a
    }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Bounded, Enum, IsList, IsString)
  deriving (Functor, Applicative, Monad) via Identity

instance (Eq a, Semigroup a) => Meet (Monoidal a) where
    (/\) = coerce @(a -> a -> a) (<>)
instance (Eq a, Monoid a) => HasTop (Monoidal a) where
    top = coerce @a mempty
instance (Eq a, Monoid a) => BoundedMeet (Monoidal a)
instance Meet l => Semigroup (Monoidal l) where
    (<>) = coerce @(l -> l -> l) (/\)
instance BoundedMeet l => Monoid (Monoidal l) where
    mempty = coerce @l top


instance Ord a => Meet (Set a) where
    (/\) = Set.intersection
instance Ord a => Join (Set a) where
    (\/) = Set.union
instance Ord a => HasBot (Set a) where
    isBot = Set.null
    bot = Set.empty
instance Ord a => BoundedJoin (Set a)
instance Ord a => Lattice (Set a)


newtype Ordered a = Ordered
    { getOrdered :: a
    }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Bounded, Enum, Num, IsList, IsString)
  deriving (Functor, Applicative, Monad) via Identity

instance Ord a => Meet (Ordered a) where
    (/\) = max
instance (Ord a, Bounded a) => HasTop (Ordered a) where
    top = minBound
instance Ord a => Join (Ordered a) where
    (\/) = min
instance (Ord a, Bounded a) => HasBot (Ordered a) where
    bot = maxBound
instance (Ord a, Bounded a) => BoundedMeet (Ordered a)
instance (Ord a, Bounded a) => BoundedJoin (Ordered a)
instance (Ord a, Bounded a) => Lattice (Ordered a)
instance (Ord a, Bounded a) => BoundedLattice (Ordered a)


deriving via (Monoidal ()) instance Meet ()
deriving via (Monoidal ()) instance HasTop ()
deriving via (Monoidal ()) instance BoundedMeet ()
deriving via (Dual ()) instance Join ()
deriving via (Dual ()) instance HasBot ()
deriving via (Dual ()) instance BoundedJoin ()
instance Lattice ()
instance BoundedLattice ()


instance (Meet a, Meet b) => Meet (a, b) where
    (a, b) /\ (a', b') = (a /\ a', b /\ b')
instance (HasTop a, HasTop b) => HasTop (a, b) where
    isTop (a, b) = isTop a && isTop b
    top = (top, top)
instance (Join a, Join b) => Join (a, b) where
    (a, b) \/ (a', b') = (a \/ a', b \/ b')
instance (HasBot a, HasBot b) => HasBot (a, b) where
    isBot (a, b) = isBot a && isBot b
    bot = (bot, bot)
instance (BoundedMeet a, BoundedMeet b) => BoundedMeet (a, b)
instance (BoundedJoin a, BoundedJoin b) => BoundedJoin (a, b)
instance (Lattice a, Lattice b) => Lattice (a, b)
instance (BoundedLattice a, BoundedLattice b) => BoundedLattice (a, b)

instance (Meet a, Meet b, Meet c) => Meet (a, b, c) where
    (a, b, c) /\ (a', b', c') = (a /\ a', b /\ b', c /\ c')
instance (HasTop a, HasTop b, HasTop c) => HasTop (a, b, c) where
    isTop (a, b, c) = isTop a && isTop b && isTop c
    top = (top, top, top)
instance (Join a, Join b, Join c) => Join (a, b, c) where
    (a, b, c) \/ (a', b', c') = (a \/ a', b \/ b', c \/ c')
instance (HasBot a, HasBot b, HasBot c) => HasBot (a, b, c) where
    isBot (a, b, c) = isBot a && isBot b && isBot c
    bot = (bot, bot, bot)
instance (BoundedMeet a, BoundedMeet b, BoundedMeet c) => BoundedMeet (a, b, c)
instance (BoundedJoin a, BoundedJoin b, BoundedJoin c) => BoundedJoin (a, b, c)
instance (Lattice a, Lattice b, Lattice c) => Lattice (a, b, c)
instance (BoundedLattice a, BoundedLattice b, BoundedLattice c) => BoundedLattice (a, b, c)

deriving via (Ordered Bool) instance Meet Bool
deriving via (Ordered Bool) instance HasTop Bool
deriving via (Ordered Bool) instance BoundedMeet Bool
deriving via (Ordered Bool) instance Join Bool
deriving via (Ordered Bool) instance HasBot Bool
deriving via (Ordered Bool) instance BoundedJoin Bool
instance Lattice Bool
instance BoundedLattice Bool


newtype Applied f a = Applied
    { getApplied :: f a
    }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Bounded, Enum, IsList, IsString, Semigroup, Monoid)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Eq1, Ord1) via f

instance (Applicative f, Meet a) => Meet (Applied f a) where
    (/\) = liftA2 (/\)
instance (Applicative f, Eq (f a), HasTop a) => HasTop (Applied f a) where
    top = pure top
instance (Applicative f, Join a) => Join (Applied f a) where
    (\/) = liftA2 (\/)
instance (Applicative f, Eq (f a), HasBot a) => HasBot (Applied f a) where
    bot = pure bot
instance (Applicative f, Eq (f a), BoundedMeet a) => BoundedMeet (Applied f a) where
instance (Applicative f, Eq (f a), BoundedJoin a) => BoundedJoin (Applied f a) where
instance (Applicative f, Lattice a) => Lattice (Applied f a)
instance (Applicative f, Eq (f a), BoundedLattice a) => BoundedLattice (Applied f a)


instance HasTop a => HasTop (ZipList a) where
    isTop = getAll . foldMap (All . isTop)
    top = pure top
instance HasBot a => HasBot (ZipList a) where
    isBot = getAny . foldMap (Any . isBot)
    bot = pure bot
deriving via (Applied ZipList a) instance Meet a => Meet (ZipList a)
deriving via (Applied ZipList a) instance BoundedMeet a => BoundedMeet (ZipList a)
deriving via (Applied ZipList a) instance Join a => Join (ZipList a)
deriving via (Applied ZipList a) instance BoundedJoin a => BoundedJoin (ZipList a)
instance Lattice a => Lattice (ZipList a)
instance BoundedLattice a => BoundedLattice (ZipList a)

deriving via (Applied IO a) instance Meet a => Meet (IO a)
deriving via (Applied IO a) instance Join a => Join (IO a)
instance Lattice a => Lattice (IO a)


deriving newtype instance Meet (f (g a)) => Meet (Compose f g a)
deriving newtype instance HasTop (f (g a)) => HasTop (Compose f g a)
deriving newtype instance BoundedMeet (f (g a)) => BoundedMeet (Compose f g a)
deriving newtype instance Join (f (g a)) => Join (Compose f g a)
deriving newtype instance HasBot (f (g a)) => HasBot (Compose f g a)
deriving newtype instance BoundedJoin (f (g a)) => BoundedJoin (Compose f g a)
instance (Meet (f (g a)), Join (f (g a))) => Lattice (Compose f g a)
instance (BoundedMeet (f (g a)), BoundedJoin (f (g a))) => BoundedLattice (Compose f g a)
