-- | Sum of the total base media time in the timescale from the movie header box.
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackFragBaseMediaDecodeTime where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Util.Time
import GHC.TypeLits

-- * @tfdt@ Box

-- | Construct a 'TrackFragBaseMediaDecodeTime box.
trackFragBaseMediaDecodeTime -- TODO allow 64 bit variant
  :: KnownNat version
  => TS version "track-fragment-base-media-decode-time"
  -> Box (FullBox (TrackFragBaseMediaDecodeTime version) version)
trackFragBaseMediaDecodeTime = fullBox 0 . TrackFragBaseMediaDecodeTime

-- | Track fragment media base decode time
newtype TrackFragBaseMediaDecodeTime (version :: Nat) where
  TrackFragBaseMediaDecodeTime
    :: TS version "track-fragment-base-media-decode-time"
    -> TrackFragBaseMediaDecodeTime version
  deriving (IsBoxContent)

instance IsBox (TrackFragBaseMediaDecodeTime v)

type instance BoxTypeSymbol (TrackFragBaseMediaDecodeTime v) = "tfdt"

-- | Return the static size of the empty box
trackFragBaseMediaDecodeTimeStaticSize64 :: Num a => a
trackFragBaseMediaDecodeTimeStaticSize64 =
  fromBoxSize 0 (boxSize (trackFragBaseMediaDecodeTime (TSv1 0)))
