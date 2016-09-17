module Data.ByteString.Mp4.AudioFile where

import Data.ByteString.Mp4.Boxes.AudioSpecificConfig
import Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry
import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.IsoBaseFileFormat.Brands.Dash
import Data.ByteString.IsoBaseFileFormat.MediaFile
import Data.ByteString.IsoBaseFileFormat.ReExports
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.Time
import Data.ByteString.IsoBaseFileFormat.Util.Versioned
import qualified Data.Text as T

-- | Initialisation segment parameters of an aac audio stream mp4 file.
data AacMp4StreamConfig =
  AacMp4StreamConfig { creationTime :: (TS32 "creation_time")
                     , trackName :: String
                     , duration :: Integer
                     , useHeAac :: Bool
                     , sampleRate :: SamplingFreqTable
                     , channelConfig :: ChannelConfigTable}

-- | Convert a 'SingleAudioTrackInit' record to a generic 'Boxes' collection.
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
