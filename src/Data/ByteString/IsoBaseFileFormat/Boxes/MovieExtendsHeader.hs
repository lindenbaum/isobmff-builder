-- | Overall duration of fragmented media.
module Data.ByteString.IsoBaseFileFormat.Boxes.MovieExtendsHeader where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Util.Versioned
import Data.ByteString.IsoBaseFileFormat.ReExports

-- * @mehd@ Box

-- | Construct a 'MovieHeader' box.
movieExtendsHeader
  :: (KnownNat version)
  => MovieExtendsHeader version -> Box (FullBox (MovieExtendsHeader version) version)
movieExtendsHeader = fullBox 0

-- | Movie length incorporating all fragments.
newtype MovieExtendsHeader (version :: Nat) where
        MovieExtendsHeader ::
          Versioned (U32 "fragment_duration") (U64 "fragment_duration") version
            -> MovieExtendsHeader version
    deriving (IsBoxContent)

instance IsBox (MovieExtendsHeader version)

type instance BoxTypeSymbol (MovieExtendsHeader v) = "mehd"
