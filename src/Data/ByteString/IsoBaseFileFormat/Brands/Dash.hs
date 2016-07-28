{-# LANGUAGE UndecidableInstances #-}
-- | Predefined Box composition matching the @dash@ brand. TODO this is an
-- incomplete,  special-purpose variant of this brand, serving my personal,
-- educational, current need.
-- This is a convenient way of building documents of that kind.
module Data.ByteString.IsoBaseFileFormat.Brands.Dash
       (Dash, SingleAudioTrackInit(..), mkSingleTrackInit, module X) where

import Data.ByteString.IsoBaseFileFormat.Brands.Types
import Data.ByteString.IsoBaseFileFormat.Boxes as X hiding (All)
import Data.Kind (Type, Constraint)
import Control.Lens

-- | A phantom type to indicate this branding. Version can be 0 or 1 it is used
-- in some boxes to switch between 32/64 bits.
data Dash (version :: Nat)

-- | A constant to indicate the 'Dash' brand with version 0
dash :: Proxy (Dash 0)
dash = Proxy

-- | A 'BoxLayout' which contains the stuff needed for the 'dash' brand.
-- TODO incomplete
instance IsBrand (Dash v) where
  type BoxLayout (Dash v) =
    Boxes
    '[ OM_ FileType
      , OM  Movie
           '[ OM_ (MovieHeader v)
            , SM  Track
                 '[ OM_ (TrackHeader v)
                  , OM  Media
                       '[ OM_ (MediaHeader v)
                        , OM_ Handler
                        , OM  MediaInformation
                             '[ OneOf '[ OM_ VideoMediaHeader
                                       , OM_ SoundMediaHeader
                                       , OM_ HintMediaHeader
                                       , OM_ NullMediaHeader]
                              , OM  DataInformation
                                   '[ OM  DataReference
                                         '[ OneOf '[ OM_ DataEntryUrl
                                                   , OM_ DataEntryUrn]
                                          , SomeOptionalX
                                             (OneOf '[ OM_ DataEntryUrl
                                                     , OM_ DataEntryUrn])]]
                              -- , OM  (SampleTable v)               -- TODO
                              --      '[ OM_ (SampleDescriptions v)
                              --       , OM_ (TimeToSample v)
                              --       , OM_ (SampleToChunk v)
                              --       , OO_ (SampleSizes v)
                              --       , OM_ (SampleChunkOffset v)
                              --       ]
                              ]
                       ]
                  ]
            ]
     , SO_ Skip
     ]

-- Missing Boxes
-- START 17:47:
--  mdia
--  mdhd
--  hdlr
--  minf
--  smhd
--  dinf
--  dref
--  ??url
--  stbl
--  stsd
--  stts
--  stsc
--  stsz
--  stco
--  soun
--  mp4a
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
data SingleAudioTrackInit =
  SingleAudioTrackInit {_mvhd :: MovieHeader 0
                       ,_tkhd :: TrackHeader 0
                       ,_mdhd :: MediaHeader 0
                       ,_hdlr :: Handler
                       ,_smhd :: SoundMediaHeader}

makeLenses ''SingleAudioTrackInit

-- | Convert a 'SingleAudioTrackInit' record to a generic 'Boxes' collection.
mkSingleTrackInit
  :: SingleAudioTrackInit -> Builder
mkSingleTrackInit doc = mediaBuilder dash $
  fileTypeBox (FileType "dash" 0 ["isom","iso5","mp42"])
  :|
   movie (movieHeader (doc ^. mvhd) :|
          track (trackHeader (doc ^. tkhd) :|
                 media (mediaHeader (doc ^. mdhd) :. handler (doc ^. hdlr) :|
                        mediaInformation
                            (soundMediaHeader (doc ^. smhd) :| dataInformation $: localMediaDataReference ))))
