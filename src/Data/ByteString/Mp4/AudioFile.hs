module Data.ByteString.Mp4.AudioFile where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry
import Data.ByteString.IsoBaseFileFormat.Brands.Dash
import Data.ByteString.IsoBaseFileFormat.MediaFile
import Data.ByteString.IsoBaseFileFormat.ReExports

-- | Initialisation segment parameters of an aac audio stream mp4 file.
data AacStreamInitSegment =
  AacStreamInitSegment { creationTime :: (TS32 "creation_time")
                       , trackName :: String
                       , duration :: Integer }

  -- AacStreamInitSegment {mvhd :: !(MovieHeader 0)
  --                      ,tkhd :: !(TrackHeader 0)
  --                      ,mdhd :: !(MediaHeader 0)
  --                      ,hdlr :: !(Handler 'AudioTrack)
  --                      ,smhd :: !SoundMediaHeader
  --                      ,ause :: !(AudioSampleEntry (Box AudioEsd))}

-- | Convert a 'SingleAudioTrackInit' record to a generic 'Boxes' collection.
buildAacStreamInitSegment
  :: AacStreamInitSegment -> Builder
buildAacStreamInitSegment AacStreamInitSegment{..} = mediaBuilder dash $
     fileTypeBox (FileType "dash" 0 ["isom","iso5","mp42"]) -- TODO must be iso5 for the way we use elementary stream descriptors
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
              :. handler (namedAudioTrackHandler trackName)
              :| mediaInformation
                   ( soundMediaHeader (SoundMediaHeader def)
                   :. (dataInformation $: localMediaDataReference)
                   :| sampleTable
                       ((  sampleDescription $: sampleEntry 0 aacLcMono16kEsd)
                        :. timeToSample []
                        :. sampleToChunk []
                        :. chunkOffset32 []
                        :| fixedSampleSize 0 0 ))))
      :| (movieExtends $: trackExtendsUnknownDuration 1 1))
