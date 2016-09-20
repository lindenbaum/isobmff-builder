-- | Sum of the total base media time in the timescale from the movie header box.
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackFragBaseMediaDecodeTime where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.Word
import Data.Default

-- * @tfdt@ Box

-- | Construct a 'TrackFragBaseMediaDecodeTime box.
trackFragBaseMediaDecodeTime -- TODO allow 64 bit variant
  :: Word32 -> Box (FullBox TrackFragBaseMediaDecodeTime 0)
trackFragBaseMediaDecodeTime = fullBox 0 . TrackFragBaseMediaDecodeTime . Scalar

-- | Track fragment media base decode time
newtype TrackFragBaseMediaDecodeTime where
  TrackFragBaseMediaDecodeTime
    :: U32 "track-fragment-base-media-decode-time"
    -> TrackFragBaseMediaDecodeTime
  deriving (IsBoxContent)

instance IsBox TrackFragBaseMediaDecodeTime

type instance BoxTypeSymbol TrackFragBaseMediaDecodeTime = "tfdt"

-- | Return the static size of the empty box
trackFragBaseMediaDecodeTimeStaticSize :: Num a => a
trackFragBaseMediaDecodeTimeStaticSize =
  fromBoxSize 0 (boxSize (trackFragBaseMediaDecodeTime def))
