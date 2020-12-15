{-# LANGUAGE NoImplicitPrelude #-}
module Control.Invertable where

import "base" Data.Function ( ($) )
import "base" Data.Tuple ( swap, uncurry )
import "base" Data.Monoid
import "base" Control.Category
import "base" Control.Arrow

import "this" Data.Group


data Inv cat a b = (:<->)
    { apply :: !(cat a b)
    , ylppa :: !(cat b a)
    }
type (<->) = Inv (->)

(<->) :: (a -> b) -> (b -> a) -> (a <-> b)
(<->) = (:<->)

instance Category cat => Category (Inv cat) where
    id = id :<-> id
    (f :<-> f') . (g :<-> g') = (f . g) :<-> (g' . f')

data InvBinOp cat a b c = InvBinOp
    !(cat (b, c) a)
    !(cat (a, c) b)
    !(cat (a, b) c)

shift :: Arrow cat => InvBinOp cat a b c -> InvBinOp cat c a b
shift (InvBinOp f g h) = InvBinOp h (f . arr swap) (g . arr swap)

invertiblMappend :: (Arrow cat, Group a) => InvBinOp cat a a a
invertiblMappend = InvBinOp
    (arr $ uncurry (<>))
    (arr $ uncurry (><))
    (arr $ uncurry (><))
