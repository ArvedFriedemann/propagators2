{-# LANGUAGE NoImplicitPrelude #-}
module Data.Iso where

import "base" Prelude hiding ( (.), id )
import "base" Control.Category


data Iso cat a b = (:<->)
    { to :: cat a b
    , from :: cat b a
    }

type (<->) = Iso (->)

instance Category cat => Category (Iso cat) where
    id = id :<-> id
    (t :<-> f) . (t' :<-> f') = (t . t') :<-> (f' . f)

co :: Iso cat a b -> Iso cat b a
co (t :<-> f) = f :<-> t
