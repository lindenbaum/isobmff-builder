module Data.ByteString.Mp4.AudioFile where

import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.Mp4.Boxes.AudioSampleEntry
import Data.ByteString.IsoBaseFileFormat.Brands.Dash

-- | A record which contains the stuff needed for a single track initialization
-- document according to the 'Dash' brand. TODO incomplete
-- TODO take this out into its own module, make a new package for specific file formats
data SingleAudioTrackInit =
  SingleAudioTrackInit {mvhd :: !(MovieHeader 0)
                       ,tkhd :: !(TrackHeader 0)
                       ,mdhd :: !(MediaHeader 0)
                       ,hdlr :: !(Handler 'AudioTrack)
                       ,smhd :: !SoundMediaHeader}

-- | Convert a 'SingleAudioTrackInit' record to a generic 'Boxes' collection.
mkSingleTrackInit
  :: SingleAudioTrackInit -> Builder
mkSingleTrackInit doc = mediaBuilder dash $
     fileTypeBox (FileType "dash" 0 ["isom","iso5","mp42"])
  :| movie
      ( movieHeader (mvhd doc)
      :| track
          ( trackHeader (tkhd doc)
          :| media
              ( mediaHeader (mdhd doc)
              :. handler (hdlr doc)
              :| mediaInformation
                   ( soundMediaHeader (smhd doc)
                   :. (dataInformation $: localMediaDataReference)
                   :| sampleTable
                       ((  sampleDescription $: audioSampleEntry def 1 def)
                        :. timeToSample []
                        :. sampleToChunk []
                        :. chunkOffset32 []
                        :| fixedSampleSize 0 0 )))))
