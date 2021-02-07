
import "tasty" Test.Tasty

import "this" Spec.Data.Lattice qualified
import "this" Spec.Control.Propagator.Scope qualified

main :: IO ()
main = defaultMain . testGroup "Tests" $
    [ Spec.Data.Lattice.tests
    , Spec.Control.Propagator.Scope.tests
    ]

