{-# LANGUAGE UndecidableInstances #-}
-- | A binary version 0/1 field with seperate content for each version.
module Data.ByteString.IsoBaseFileFormat.Util.Versioned
       (Versioned(..))
       where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.ReExports

-- TODO rewrite/cleanup the versioned mess

-- | Two alternative representations based on a /version/ index.
--   Use this for box content that can be either 32 or 64 bit.
data Versioned v0 v1 (version :: Nat) where
        V0 :: IsBoxContent v0 => !v0 -> Versioned v0 v1 0
        V1 :: IsBoxContent v1 => !v1 -> Versioned v0 v1 1

instance (version ~ 0,IsBoxContent v0,Default v0) => Default (Versioned v0 v1 (version :: Nat)) where
  def = V0 def

instance IsBoxContent (Versioned v0 v1 version) where
  boxSize (V0 c) = boxSize c
  boxSize (V1 c) = boxSize c
  boxBuilder (V0 c) = boxBuilder c
  boxBuilder (V1 c) = boxBuilder c
