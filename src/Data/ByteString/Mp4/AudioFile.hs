module Data.ByteString.Mp4.AudioFile
 (module Data.ByteString.Mp4.AudioFile,
  module X)
where

import Data.ByteString.Mp4.Boxes.AudioSpecificConfig as X
import Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry as X
import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.IsoBaseFileFormat.Brands.Dash as X
import Data.ByteString.IsoBaseFileFormat.MediaFile as X
import Data.ByteString.IsoBaseFileFormat.ReExports as X
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields as X
import Data.ByteString.IsoBaseFileFormat.Util.Time  as X
import Data.ByteString.IsoBaseFileFormat.Util.Versioned
import qualified Data.Text as T

-- | Initialisation segment parameters of an aac audio stream mp4 file.
data AacMp4StreamConfig =
  AacMp4StreamConfig { creationTime  :: !(TS32 "creation_time")
                     , trackName     :: !String
                     , duration      :: !Integer
                     , useHeAac      :: !Bool
                     , sampleRate    :: !SamplingFreqTable
                     , channelConfig :: !ChannelConfigTable}

-- | Convert a 'AacMp4StreamConfig' record to a generic 'Boxes' collection.
buildAacMp4StreamInit
  :: AacMp4StreamConfig -> Builder
buildAacMp4StreamInit AacMp4StreamConfig{..} =
  mediaBuilder dash $
  -- TODO must be iso5 for the way we use elementary stream descriptors
  fileTypeBox (FileType "iso5" 0 ["isom","iso5","dash","mp42"])
  :| movie
      ( movieHeader
        (MovieHeader $
          V0 (creationTime
               :+ 0
               :+ Template
               :+ durationFromSeconds Template duration)
          :+ def)
        :. track
          (trackHeader
            (TrackHeader $
              V0 (0 :+ 0 :+ 1 :+ Constant :+ durationFromSeconds Template duration) :+
              def)
            :| media
            (mediaHeader (MediaHeader def)
              :. handler (namedAudioTrackHandler (T.pack trackName))
              :| mediaInformation
              ( soundMediaHeader (SoundMediaHeader def)
                :. (dataInformation $: localMediaDataReference)
                :| sampleTable
                ((sampleDescription
                   $: audioSampleEntry 0 (aacAudioSampleEntrySimple
                                          useHeAac
                                          sampleRate
                                          channelConfig
                                          (Scalar 16))
                   :. timeToSample []
                   :. sampleToChunk []
                   :. chunkOffset32 []
                   :| fixedSampleSize 0 0)))))
          :| (movieExtends $: trackExtendsUnknownDuration 1 1))

-- | Media fragment segment parameters of an aac audio stream mp4 file.
data AacMp4StreamFragment =
  AacMp4StreamFragment { fragSequence              :: !Word32
                       , baseMediaDecodeTimeMillis :: !Word32
                       }

-- | Convert a 'AacMp4StreamFragment record to a generic 'Boxes' collection.
buildAacMp4StreamFragment
  :: AacMp4StreamFragment -> Builder
buildAacMp4StreamFragment AacMp4StreamFragment{..} =
  mediaBuilder dash $
  -- TODO must be iso5 for the way we use elementary stream descriptors
  segmentTypeBox (SegmentType "iso5" 0 ["isom","iso5","dash","mp42"])
  :| movieFragment
      ( movieFragmentHeader (MovieFragmentHeader (Scalar fragSequence))
      :| trackFragment
        (   trackFragmentHeader def
         :| trackFragBaseMediaDecodeTimeMillis baseMediaDecodeTimeMillis))
