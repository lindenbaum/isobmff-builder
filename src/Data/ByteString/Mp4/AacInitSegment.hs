-- | A single-track AAC audio streaming utility wrapping 'AudioFile' for
-- streaming via e.g. DASH.
module Data.ByteString.Mp4.AacInitSegment
  ( AacInitSegment(..)
  , BinaryAacInitSegment(..)
  , buildAacInitSegment
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.IsoBaseFileFormat.Brands.Dash as X
import Data.ByteString.IsoBaseFileFormat.MediaFile as X
import Data.ByteString.IsoBaseFileFormat.ReExports as X
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields as X
import Data.ByteString.IsoBaseFileFormat.Util.Time
import Data.ByteString.IsoBaseFileFormat.Util.Versioned
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Mp4.Boxes.AudioSpecificConfig
import Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry as X
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Initialisation segment parameters of an aac audio stream mp4 file.
data AacInitSegment = AacInitSegment
  { creationTime :: !(TS32 "creation_time")
  , trackName :: !String
  , useHeAac :: !Bool
  , sampleRate :: !SamplingFreqTable
  , channelConfig :: !ChannelConfigTable
  }

instance Show BinaryAacInitSegment where
  show (BinaryAacInitSegment !d) =
    printf "INIT SEGMENT - size: %14d" (BS.length d)

-- | Encoded, binary MP4 AAC init segment (strict).
newtype BinaryAacInitSegment = BinaryAacInitSegment
  { fromBinaryAacInitSegment :: BS.ByteString
  }

-- | Convert an 'AacInitSegment' record to a generic 'BinaryAacInitSegment'.
buildAacInitSegment :: AacInitSegment -> BinaryAacInitSegment
buildAacInitSegment =
  BinaryAacInitSegment . BL.toStrict . toLazyByteString . build

-- | Convert an 'AacInitSegment' record to a generic 'BinaryAacInitSegment'.
build :: AacInitSegment -> Builder
build AacInitSegment {..} =
  let timeScaleForSampleRate = TimeScale (sampleRateToNumber sampleRate)
  in mediaBuilder dash $
      -- TODO must be iso5 for the way we use elementary stream descriptors
     fileTypeBox (FileType "iso5" 0 ["isom", "iso5", "dash", "mp42"]) :.
     skipBox
       (Skip
          (TE.encodeUtf8
             (T.pack "Lindenbaum GmbH isobmff-builder, Sven Heyll 2017"))) :|
     movie
       (movieHeader
          (MovieHeader $
           V0 (creationTime :+ relabelScalar creationTime :+ def :+ TSv0 0) :+
           def :+
           def :+
           def :+
           def :+
           def :+
           def :+
           Custom (Scalar 2)) :.
        track
          (trackHeader
             TrackInMovieAndPreview
             (TrackHeader $
              V0
                (creationTime :+ relabelScalar creationTime :+ 1 :+ Constant :+
                 0) :+
              def) :|
           media
             (mediaHeader
                (MediaHeader $
                 V0
                   (creationTime :+ relabelScalar creationTime :+
                    timeScaleForSampleRate :+
                    TSv0 0) :+
                 def) :.
              handler (namedAudioTrackHandler (T.pack trackName)) :|
              mediaInformation
                (soundMediaHeader (SoundMediaHeader def) :.
                 (dataInformation $: localMediaDataReference) :|
                 sampleTable
                   ((sampleDescription $:
                     audioSampleEntry
                       1
                       (aacAudioSampleEntrySimple
                          useHeAac
                          sampleRate
                          channelConfig
                          (Scalar 16)) :.
                     timeToSample [] :.
                     sampleToChunk [] :.
                     fixedSampleSize 0 0 :|
                     chunkOffset32 []))))) :|
        (movieExtends $: trackExtendsUnknownDuration 1 1))
