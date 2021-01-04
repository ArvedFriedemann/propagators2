{-# LANGUAGE StrictData #-}
module Data.Lattice where

import "base" Data.Functor.Identity
import "base" Data.Functor.Compose
import "base" Data.Functor.Classes
import "base" Data.Monoid ( Dual(..) )
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

{- | A Bounded meet-semilattice

Instances of 'BoundedMeet' should satisfy the following:

[Identity] @x ⋀ top = x@
-}
class Meet l => BoundedMeet l where
    top :: l


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


{- | A Bounded join-semilattice

Instances of 'BoundedJoin' should satisfy the following:

[Identity] @x ⋁ bot = x@
-}
class Join l => BoundedJoin l where
    bot :: l

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
instance BoundedMeet l => BoundedJoin (Dual l) where
    bot = coerce @l top
instance Join l => Meet (Dual l) where
    (/\) = coerce @(l -> l -> l)  (\/)
instance BoundedJoin l => BoundedMeet (Dual l) where
    top = coerce @l bot
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
instance (Eq a, Monoid a) => BoundedMeet (Monoidal a) where
    top = coerce @a mempty

instance Meet l => Semigroup (Monoidal l) where
    (<>) = coerce @(l -> l -> l) (/\)
instance BoundedMeet l => Monoid (Monoidal l) where
    mempty = coerce @l top


instance Ord a => Meet (Set a) where
    (/\) = Set.intersection
instance Ord a => Join (Set a) where
    (\/) = Set.union
instance Ord a => BoundedJoin (Set a) where
    bot = Set.empty
instance Ord a => Lattice (Set a)


newtype Ordered a = Ordered
    { getOrdered :: a
    }
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Bounded, Enum, Num, IsList, IsString)
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
  deriving newtype (Eq, Ord, Bounded, Enum, IsList, IsString, Semigroup, Monoid)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Eq1, Ord1) via f

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


pattern Bot :: (Eq a, BoundedJoin a) => a
pattern Bot <- ((\x -> x == bot) -> True)
  where Bot = bot

pattern Top :: (Eq a, BoundedMeet a) => a
pattern Top <- ((\x -> x == top) -> True)
    where Top = top


data WithTop a
    = SynthTop
    | NotTop a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
{-# COMPLETE Bot, Value :: WithTop #-}

instance Eq1 WithTop where
    liftEq _ SynthTop SynthTop = True
    liftEq f (NotTop a) (NotTop b) = f a b
    liftEq _ _ _ = False
instance Ord1 WithTop where
    liftCompare f (NotTop a) (NotTop b) = f a b
    liftCompare _ (NotTop _) _ = GT
    liftCompare _ _ (NotTop _) = LT
    liftCompare _ SynthTop SynthTop = EQ
instance Show1 WithTop where
    liftShowsPrec _ _ _ SynthTop = showString "Top"
    liftShowsPrec f _ d (NotTop a) = f d a
instance IsList a => IsList (WithTop a) where
    type Item (WithTop a) = Item a
    fromList = NotTop . fromList
    toList SynthTop = []
    toList (NotTop a) = toList a
instance IsString a => IsString (WithTop a) where
    fromString = NotTop . fromString

instance Applicative WithTop where
    pure = NotTop
    NotTop f <*> NotTop a = NotTop $ f a
    _ <*> _ = SynthTop
instance Monad WithTop where
    SynthTop >>= _ = SynthTop
    NotTop a >>= f = f a

instance Meet a => Meet (WithTop a) where
    (/\) = liftA2 (/\)
instance Meet a => BoundedMeet (WithTop a) where
    top = SynthTop
instance Join a => Join (WithTop a) where
    (\/) = liftA2 (\/)
instance BoundedJoin a => BoundedJoin (WithTop a) where
    bot = pure bot
instance (Meet a, Join a) => Lattice (WithTop a)
instance (Meet a, BoundedJoin a) => BoundedLattice (WithTop a)
    

data WithBot a
    = SynthBot
    | NotBot a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
{-# COMPLETE Bot, Value :: WithBot #-}

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
instance Join a => BoundedJoin (WithBot a) where
    bot = SynthBot
instance Meet a => Meet (WithBot a) where
    (/\) = liftA2 (/\)
instance BoundedMeet a => BoundedMeet (WithBot a) where
    top = pure top
instance (Meet a, Join a) => Lattice (WithBot a)
instance (BoundedMeet a, Join a) => BoundedLattice (WithBot a)

class HasValue f where
    fromValue :: f a -> Maybe a
    toValue :: a -> f a
pattern Value :: HasValue f => a -> f a
pattern Value v <- (fromValue -> Just v)
  where Value v = toValue v

instance HasValue Maybe where
    fromValue = id
    toValue = Just
instance HasValue WithTop where
    fromValue (NotTop a) = Just a
    fromValue _ = Nothing
    toValue = NotTop
instance HasValue WithBot where
    fromValue (NotBot a) = Just a
    fromValue _ = Nothing
    toValue = NotBot
instance (HasValue f, HasValue g) => HasValue (Compose f g) where
    fromValue = (fromValue >=> fromValue) . getCompose
    toValue = Compose . toValue . toValue
instance HasValue Set where
    fromValue s | Set.size s == 1 = Set.lookupMax s
    fromValue _ = Nothing
    toValue = Set.singleton

deriving newtype instance Meet (f (g a)) => Meet (Compose f g a)
deriving newtype instance BoundedMeet (f (g a)) => BoundedMeet (Compose f g a)
deriving newtype instance Join (f (g a)) => Join (Compose f g a)
deriving newtype instance BoundedJoin (f (g a)) => BoundedJoin (Compose f g a)
instance (Meet (f (g a)), Join (f (g a))) => Lattice (Compose f g a)
instance (BoundedMeet (f (g a)), BoundedJoin (f (g a))) => BoundedLattice (Compose f g a)

newtype WithBounds a = WithBounds (Compose WithTop WithBot a)
  deriving newtype (Eq, Ord, Functor, Applicative, HasValue, Eq1, Ord1, Foldable)
{-# COMPLETE Bot, Value, Top :: WithBounds #-}
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
    Value a >>= f = f a
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
    sequenceA (Value f) = Value <$> f
    sequenceA f = pure $ fmap undefined f
