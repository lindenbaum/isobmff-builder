{-# LANGUAGE UndecidableInstances #-}
-- | A binary version 0/1 field with seperate content for each version.
module Data.ByteString.IsoBaseFileFormat.Boxes.Versioned
       (Versioned(..))
       where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.Default
import Data.Kind

-- | Two alternative representations based on a /version/ index.
--   Use this for box content that can be either 32 or 64 bit.
data Versioned v0 v1 (version :: Nat) where
        V0 :: IsBoxContent v0 => v0 -> Versioned v0 v1 0
        V1 :: IsBoxContent v1 => v1 -> Versioned v0 v1 1

instance (version ~ 0,IsBoxContent v0,Default v0) => Default (Versioned v0 v1 (version :: Nat)) where
  def = V0 def

instance IsBoxContent (Versioned v0 v1 version) where
  boxSize (V0 c) = boxSize c
  boxSize (V1 c) = boxSize c
  boxBuilder (V0 c) = boxBuilder c
  boxBuilder (V1 c) = boxBuilder c

data VersionedBox c where
  BoxV0 :: forall (v :: Nat -> Type). IsBoxContent (v 0) => v 0 -> VersionedBox v
  BoxV1 :: forall (v :: Nat -> Type). IsBoxContent (v 1) => v 1 -> VersionedBox v

instance (IsBoxContent (b 0), Default (b 0)) => Default (VersionedBox b) where
  def = BoxV0 def

instance IsBoxContent (VersionedBox c) where
  boxSize (BoxV0 c) = boxSize c
  boxSize (BoxV1 c) = boxSize c
  boxBuilder (BoxV0 c) = boxBuilder c
  boxBuilder (BoxV1 c) = boxBuilder c
