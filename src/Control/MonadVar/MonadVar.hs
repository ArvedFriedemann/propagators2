{-
Disclaimer: MonadVar taken from https://github.com/effectfully/monad-var
package was broken.
-}
module Control.MonadVar.MonadVar (module Export) where

import "this" Control.MonadVar.MonadVar.Classes         as Export
import "this" Control.MonadVar.MonadVar.Combinators     as Export
import "this" Control.MonadVar.MonadVar.Default         as Export
import "this" Control.MonadVar.MonadVar.Instances.IORef as Export ()
import "this" Control.MonadVar.MonadVar.Instances.MVar  as Export ()
import "this" Control.MonadVar.MonadVar.Instances.STRef as Export ()
import "this" Control.MonadVar.MonadVar.Instances.TMVar as Export ()
import "this" Control.MonadVar.MonadVar.Instances.TVar  as Export ()
