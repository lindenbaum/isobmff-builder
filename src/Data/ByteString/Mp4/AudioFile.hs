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
import qualified Data.ByteString as BS

-- | Initialisation segment parameters of an aac audio stream mp4 file.
data AacMp4StreamConfig =
  AacMp4StreamConfig { creationTime  :: !(TS32 "creation_time")
                     , trackName     :: !String
                     , useHeAac      :: !Bool
                     , sampleRate    :: !SamplingFreqTable
                     , channelConfig :: !ChannelConfigTable}

-- | Convert a 'AacMp4StreamConfig' record to a generic 'Boxes' collection.
buildAacMp4StreamInit
  :: AacMp4StreamConfig -> Builder
buildAacMp4StreamInit AacMp4StreamConfig{..} =
  let
    timeScaleForSampleRate = TimeScale (sampleRateToNumber sampleRate)
  in
    mediaBuilder dash $
    -- TODO must be iso5 for the way we use elementary stream descriptors
    fileTypeBox (FileType "iso5" 0 ["isom","iso5","dash","mp42"])
    :| movie
        ( movieHeader
          (MovieHeader $
           V0 (creationTime
                :+ relabelScalar creationTime
                :+ def
                :+ 0)
            :+ def)
          :. track
            (trackHeader
              TrackInMovieAndPreview
              (TrackHeader $
                V0 (creationTime
                    :+ relabelScalar creationTime
                    :+ 1
                    :+ Constant
                    :+ 0) :+
                def)
              :| media
              (mediaHeader (MediaHeader $
                             V0 (creationTime
                                  :+ relabelScalar creationTime
                                  :+ timeScaleForSampleRate
                                  :+ 0)
                             :+ def)
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
data AacMp4TrackFragment =
  AacMp4TrackFragment { fragmentSampleSequence      :: !Word32
                      , fragmentBaseMediaDecodeTime :: !Word32
                      , fragmentSamples             :: ![(Word32, BS.ByteString)]
                      }

-- | Convert a 'AacMp4TrackFragment record to a generic 'Boxes' collection.
buildAacMp4TrackFragment
  :: AacMp4TrackFragment -> Builder
buildAacMp4TrackFragment AacMp4TrackFragment{..} =
    mediaBuilder dash
      (  styp
      :. movieFragment
           (  mfhd
           :| trackFragment
                (  tfhd
                :. tfdt
                :| trun))
      :| mdat)
  where
    !styp = segmentTypeBox               (SegmentType "iso5" 0 ["isom","iso5","dash","mp42"])
    !mfhd = movieFragmentHeader          (MovieFragmentHeader (Scalar fragmentSampleSequence))
    !tfhd = trackFragmentHeader          def
    !tfdt = trackFragBaseMediaDecodeTime fragmentBaseMediaDecodeTime
    !trun = trackRunIso5                 mdatOffset fragmentSamples
      where
        !mdatOffset = movieFragmentStaticSize
                     + movieFragmentHeaderStaticSize
                     + trackFragmentStaticSize
                     + trackFragmentHeaderStaticSize
                     + trackFragBaseMediaDecodeTimeStaticSize
    !mdat = mediaData (MediaData (BS.concat (snd <$> fragmentSamples)))
