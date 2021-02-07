{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Util where

import "base" Data.Typeable

import "tasty" Test.Tasty


tn :: forall a. Typeable a => String
tn = show . typeRep $ Proxy @a

tgrp :: forall a. Typeable a => [TestTree] -> TestTree
tgrp = testGroup (tn @a)