-- | Sum of the total base media time in the timescale from the movie header box.
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackFragBaseMediaDecodeTime where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.Word

-- * @tfdt@ Box

-- | Construct a 'TrackFragBaseMediaDecodeTime box.
trackFragBaseMediaDecodeTime -- TODO allow 64 bit variant
  :: TrackFragBaseMediaDecodeTime -> Box (FullBox TrackFragBaseMediaDecodeTime 0)
trackFragBaseMediaDecodeTime = fullBox 0

-- | Construct a 'TrackFragBaseMediaDecodeTime box, with the duration given in milli seconds
-- assuming a default timescale (of 90000)
trackFragBaseMediaDecodeTimeMillis -- TODO allow 64 bit variant
  :: Word32 -> Box (FullBox TrackFragBaseMediaDecodeTime 0)
trackFragBaseMediaDecodeTimeMillis !m =
  trackFragBaseMediaDecodeTime
    (TrackFragBaseMediaDecodeTime (Scalar (m * 90)))

-- | Track fragment media base decode time
newtype TrackFragBaseMediaDecodeTime where
        TrackFragBaseMediaDecodeTime ::
          U32 "baseMediaDecodeTime" -> TrackFragBaseMediaDecodeTime
    deriving (IsBoxContent)

instance IsBox TrackFragBaseMediaDecodeTime

type instance BoxTypeSymbol TrackFragBaseMediaDecodeTime = "tfdt"
