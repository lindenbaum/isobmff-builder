{-# LANGUAGE UndecidableInstances #-}
-- | Predefined Box composition matching the @dash@ brand. TODO this is an
-- incomplete,  special-purpose variant of this brand, serving my personal,
-- educational, current need.
-- This is a convenient way of building documents of that kind.
module Data.ByteString.IsoBaseFileFormat.Brands.Dash
       (Dash, dash)
       where
import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.MediaFile
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.IsoBaseFileFormat.ReExports


-- TODO rename dash to iso5
-- | A phantom type to indicate this branding. Version can be 0 or 1 it is used
-- in some boxes to switch between 32/64 bits.
data Dash (version :: Nat)

-- | A constant to indicate the 'Dash' brand with version 0
dash :: Proxy (Dash 0)
dash = Proxy

-- | A 'BoxLayout' which contains the stuff needed for the 'dash' brand.
-- TODO add iso1 iso2 iso3 iso5 isom formats
instance IsMediaFileFormat (Dash v) where
  type BoxLayout (Dash v) =
    OneOf '[ MovieLayout v
           , SegmentLayout]

type MovieLayout v =
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
            , OO  MovieExtends
                 '[ OO_ (MovieExtendsHeader v)
                  , OM_ TrackExtends ]
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
                      '[ OM  SampleDescription
                            '[ SomeMandatoryX (MatchSampleEntry handlerType) ]
                       , OM_ TimeToSample
                       , OM_ SampleToChunk
                       , OneOf '[ OM_ ChunkOffset32
                                , OM_ ChunkOffset64 ]
                       , OM_ SampleSize
                       ]
                ]
         ]
    ])

type SegmentLayout =
  Boxes '[ OM_ SegmentType
         , OM  MovieFragment
              '[ OM_ MovieFragmentHeader
               , OM  TrackFragment
                    '[ OM_ TrackFragmentHeader
                     , OO_ (TrackFragBaseMediaDecodeTime 1)
                     , SO_ TrackRun
                     ]
               ]
         , SM_ MediaData
         ]
