module Data.Group where


{-|
@a <> inv a = mempty@
@inv a <> a = mempty@
@a >< b = a <> inv b
-}
class Monoid g => Group g where
    inv :: g -> g
    (><) :: g -> g -> g
    a >< b = a <> inv b
