{-# LANGUAGE UndecidableInstances #-}
-- | Predefined Box composition matching the @dash@ brand. TODO this is an
-- incomplete,  special-purpose variant of this brand, serving my personal,
-- educational, current need.
-- This is a convenient way of building documents of that kind.
module Data.ByteString.IsoBaseFileFormat.Brands.Dash
       (Dash, SingleAudioTrackInit(..), mkSingleTrackInit, module X)
       where

import Data.ByteString.IsoBaseFileFormat.Brands.Types
import Data.ByteString.IsoBaseFileFormat.Boxes as X hiding (All)
import Data.Kind (Type, Constraint)

-- | A phantom type to indicate this branding. Version can be 0 or 1 it is used
-- in some boxes to switch between 32/64 bits.
data Dash (version :: Nat)

-- | A constant to indicate the 'Dash' brand with version 0
dash :: Proxy (Dash 0)
dash = Proxy

-- | A 'BoxLayout' which contains the stuff needed for the 'dash' brand.
-- TODO incomplete
instance IsMediaFileFormat (Dash v) where
  type BoxLayout (Dash v) =
    Boxes
     '[ OM_ FileType
      , OM  Movie
           '[ OM_ (MovieHeader v)
            , SomeMandatoryX
               (OneOf '[ TrackLayout v 'VideoTrack
                       , TrackLayout v 'AudioTrack
                       , TrackLayout v 'HintTrack
                       , TrackLayout v 'TimedMetaDataTrack
                       , TrackLayout v 'AuxilliaryVideoTrack])
            ]
     , SO_ Skip
     ]

type TrackLayout version handlerType =
  (ContainerBox Track
   '[ OM_ (TrackHeader version)
    , OM  Media
         '[ OM_ (MediaHeader version)
          , OM_ (Handler handlerType)
          , OM  MediaInformation
               '[ OneOf '[ OM_ (MediaHeaderFor handlerType)
                         , OM_ NullMediaHeader]
                , OM  DataInformation
                     '[ OM  DataReference
                           '[ SomeMandatoryX
                               (OneOf '[ OM_ DataEntryUrl
                                       , OM_ DataEntryUrn])]]
                , OM  SampleTable
                      '[ OM  (SampleDescription handlerType)
                            '[ SomeMandatoryX (MatchSampleEntry handlerType) ]
                       , OM_ TimeToSample
                       , OM_ SampleToChunk
                --       , OO_ (SampleSizes version)
                --       , OM_ (SampleChunkOffset version)
                       ]
                ]
         ]
    ])


-- Missing Boxes
--  stts
--  stsc
--  stsz
--  stco
--  esds
--  mvex
--  trex
-- For media
-- styp
-- moof
-- mfhd
-- traf
-- tfhd
-- trun
-- | A record which contains the stuff needed for a single track initialization
-- document according to the 'Dash' brand. TODO incomplete
-- TODO take this out into its own module, make a new package for specific file formats
data SingleAudioTrackInit =
  SingleAudioTrackInit {mvhd :: MovieHeader 0
                       ,tkhd :: TrackHeader 0
                       ,mdhd :: MediaHeader 0
                       ,hdlr :: Handler 'AudioTrack
                       ,smhd :: SoundMediaHeader}

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
                       ((sampleDescription $: audioSampleEntry Mpeg4Aac 1 def)
                        :. timeToSample []
                        :| sampleToChunk [] )))))
