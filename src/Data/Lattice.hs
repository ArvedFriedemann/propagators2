module Data.Lattice
    {-( Meet(..), BoundedMeet(..)
    , Join(..), BoundedJoin(..)
    , Lattice, BoundedLattice
    , meetAll, joinAll
    , Monoidal(..)
    , Ordered(..)
    , Applied(..)
    , WithTop( NotTop ), pattern Top
    , WithBot( NotBot ), pattern Bot
    )-} where

import "base" Data.Functor.Identity
import "base" Data.Monoid ( Dual(..) )
import "base" Control.Applicative
import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set


{- | A meet-semilattice

Instances of 'Meet' should satisfy the following:

[Associativity] @x /\ (y /\ z) = (x /\ y) /\ z@
[Commutativity] @x /\ y = y /\ x@
[Idempotency]  @x /\ x = x@
-}
class Meet l where
    (/\) :: l -> l -> l
    (⋀) :: l -> l -> l
    (⋀) = (/\)

{- | A Bounded meet-semilattice

Instances of 'BoundedMeet' should satisfy the following:

[Identity] @x /\ top = x@
-}
class Meet l => BoundedMeet l where
    top :: l


{- | A join-semilattice

Instances of 'Join' should satisfy the following:

[Associativity] @x \/ (y \/ z) = (x \/ y) \/ z@
[Commutativity] @x \/ y = y \/ x@
[Idempotency]  @x \/ x = x@
-}
class Join l where
    (\/) :: l -> l -> l
    (⋁) :: l -> l -> l
    (⋁) = (\/)


{- | A Bounded join-semilattice

Instances of 'BoundedJoin' should satisfy the following:

[Identity] @x \/ bot = x@
-}
class Join l => BoundedJoin l where
    bot :: l

{- | A Lattice

Instances of 'Lattice' should satisfy the following:

[Absorption] @a \/ (a /\ b) = a = a /\ (a \/ b)@
-}
class (Meet l, Join l) => Lattice l

{- | A bounded Lattice

Instances of 'BoundedLattice' should satisfy the following:

[MeetIdentity] @bot /\ b = bot@
[JoinIdentity] @top \/ b = top@
-}
class (BoundedMeet l, BoundedJoin l, Lattice l) => BoundedLattice l

-- combinators

meetAll :: (Foldable f, BoundedMeet l) => f l -> l
meetAll = foldr (/\) top

joinAll :: (Foldable f, BoundedJoin l) => f l -> l
joinAll = foldr (\/) bot

-- instances

instance Meet l => Join (Dual l) where
    (\/) = liftA2 (/\)
instance BoundedMeet l => BoundedJoin (Dual l) where
    bot = pure top
instance Join l => Meet (Dual l) where
    (/\) = liftA2 (\/)
instance BoundedJoin l => BoundedMeet (Dual l) where
    top = pure bot
instance Lattice l => Lattice (Dual l)
instance BoundedLattice l => BoundedLattice (Dual l)


newtype Monoidal a = Monoidal
    { getMonoidal :: a
    }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Bounded, Enum)
  deriving (Functor, Applicative, Monad) via Identity

instance Semigroup a => Meet (Monoidal a) where
    (/\) = liftA2 (<>)
instance Monoid a => BoundedMeet (Monoidal a) where
    top = pure mempty

instance Meet l => Semigroup (Monoidal l) where
    (<>) = liftA2 (/\)
instance BoundedMeet l => Monoid (Monoidal l) where
    mempty = pure top


instance Ord a => Meet (Set a) where
    (/\) = Set.intersection
instance (Ord a, Bounded a, Enum a) => BoundedMeet (Set a) where
    top = [minBound .. maxBound]
instance Ord a => Join (Set a) where
    (\/) = Set.union
instance Ord a => BoundedJoin (Set a) where
    bot = Set.empty
instance Ord a => Lattice (Set a)


newtype Ordered a = Ordered
    { getOrdered :: a
    }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Bounded, Enum, Num)
  deriving (Functor, Applicative, Monad) via Identity

instance Ord a => Meet (Ordered a) where
    (/\) = max
instance (Ord a, Bounded a) => BoundedMeet (Ordered a) where
    top = maxBound
instance Ord a => Join (Ordered a) where
    (\/) = min
instance (Ord a, Bounded a) => BoundedJoin (Ordered a) where
    bot = minBound
instance Ord a => Lattice (Ordered a)
instance (Ord a, Bounded a) => BoundedLattice (Ordered a)


deriving via (Monoidal ()) instance Meet ()
deriving via (Monoidal ()) instance BoundedMeet ()
deriving via (Dual ()) instance Join ()
deriving via (Dual ()) instance BoundedJoin ()
instance Lattice ()
instance BoundedLattice ()

instance (Meet a, Meet b) => Meet (a, b) where
    (a, b) /\ (a', b') = (a /\ a', b /\ b')
instance (BoundedMeet a, BoundedMeet b) => BoundedMeet (a, b) where
    top = (top, top)
instance (Join a, Join b) => Join (a, b) where
    (a, b) \/ (a', b') = (a \/ a', b \/ b')
instance (BoundedJoin a, BoundedJoin b) => BoundedJoin (a, b) where
    bot = (bot, bot)
instance (Lattice a, Lattice b) => Lattice (a, b)
instance (BoundedLattice a, BoundedLattice b) => BoundedLattice (a, b)

instance (Meet a, Meet b, Meet c) => Meet (a, b, c) where
    (a, b, c) /\ (a', b', c') = (a /\ a', b /\ b', c /\ c')
instance (BoundedMeet a, BoundedMeet b, BoundedMeet c) => BoundedMeet (a, b, c) where
    top = (top, top, top)
instance (Join a, Join b, Join c) => Join (a, b, c) where
    (a, b, c) \/ (a', b', c') = (a \/ a', b \/ b', c \/ c')
instance (BoundedJoin a, BoundedJoin b, BoundedJoin c) => BoundedJoin (a, b, c) where
    bot = (bot, bot, bot)
instance (Lattice a, Lattice b, Lattice c) => Lattice (a, b, c)
instance (BoundedLattice a, BoundedLattice b, BoundedLattice c) => BoundedLattice (a, b, c)

deriving via (Ordered Bool) instance Meet Bool
deriving via (Ordered Bool) instance BoundedMeet Bool
deriving via (Ordered Bool) instance Join Bool
deriving via (Ordered Bool) instance BoundedJoin Bool
instance Lattice Bool
instance BoundedLattice Bool


newtype Applied f a = Applied
    { getApplied :: f a
    }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Bounded, Enum)
  deriving (Functor, Applicative, Monad) via f

instance (Applicative f, Meet a) => Meet (Applied f a) where
    (/\) = liftA2 (/\)
instance (Applicative f, BoundedMeet a) => BoundedMeet (Applied f a) where
    top = pure top
instance (Applicative f, Join a) => Join (Applied f a) where
    (\/) = liftA2 (\/)
instance (Applicative f, BoundedJoin a) => BoundedJoin (Applied f a) where
    bot = pure bot
instance (Applicative f, Lattice a) => Lattice (Applied f a)
instance (Applicative f, BoundedLattice a) => BoundedLattice (Applied f a)


deriving via (Applied ZipList a) instance Meet a => Meet (ZipList a)
deriving via (Applied ZipList a) instance BoundedMeet a => BoundedMeet (ZipList a)
deriving via (Applied ZipList a) instance Join a => Join (ZipList a)
deriving via (Applied ZipList a) instance BoundedJoin a => BoundedJoin (ZipList a)
instance Lattice a => Lattice (ZipList a)
instance BoundedLattice a => BoundedLattice (ZipList a)

deriving via (Applied IO a) instance Meet a => Meet (IO a)
deriving via (Applied IO a) instance BoundedMeet a => BoundedMeet (IO a)
deriving via (Applied IO a) instance Join a => Join (IO a)
deriving via (Applied IO a) instance BoundedJoin a => BoundedJoin (IO a)
instance Lattice a => Lattice (IO a)
instance BoundedLattice a => BoundedLattice (IO a)

{-
data WithTop a
    = SynthTop
    | NotTop a
  deriving (Eq, Ord, Show, Read, Functor)
{-# COMPLETE Top, NotTop #-}

instance Applicative WithTop where
    pure = NotTop
    NotTop f <*> NotTop a = NotTop $ f a
    _ <*> _ = SynthTop

instance Meet a => Meet (WithTop a) where
    (/\) = liftA2 (/\)
instance Meet a => BoundedMeet (WithTop a) where
    top = SynthTop


data WithBot a
    = SynthBot
    | NotBot a
  deriving (Eq, Ord, Show, Read, Functor)
{-# COMPLETE Bot, NotBot #-}

instance Applicative WithBot where
    pure = NotBot
    NotBot f <*> NotBot a = NotBot $ f a
    _ <*> _ = SynthBot

pattern Bot :: (Eq a, BoundedJoin a) => a
pattern Bot <- ((\x -> x == bot) -> True)
  where Bot = bot

instance Join a => Join (WithBot a) where
    (\/) = liftA2 (\/)
instance Join a => BoundedJoin (WithBot a) where
    bot = SynthBot

pattern Top :: (Eq a, BoundedMeet a) => a
pattern Top <- ((\x -> x == top) -> True)
    where Top = top

-}