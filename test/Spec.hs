
import "tasty" Test.Tasty

import "this" Spec.Data.Lattice qualified


main :: IO ()
main = defaultMain . testGroup "Tests" $
    [ Spec.Data.Lattice.tests
    ]

