-- | A list of sample offsets and sizes of a track. This tells the decoder where
-- stuff inside an 'mdat' box is, and also where the 'mdat' box /starts/, which
-- is the reason why currently there are several TODOs concercning the offset
-- calculation.
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackRun (trackRunIso5, TrackRun) where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.ReExports
import qualified Data.ByteString as BS

-- | Create a track run box. First parameter is the accumulated offset to the
-- /moof/ box. This is needed for data offset calculation. Then follows a list
-- of __sample-duration__ and __sample-sizes__. All samples are flagged
-- /independent/.
trackRunIso5 :: Int32 -> [(Word32, BS.ByteString)] -> Box TrackRun
trackRunIso5 !mdatOffset !samples = Box c
  where
    !c = TrackRun (header <> body)
      where
        !header = h (fromIntegral sampleCount) dataOffset
          where
            !dataOffset       = 8 -- box size field + fourcc code of this very box
                                  + mdatOffset + 8  -- +8 because we want to skip the box header od the mdat box
                                  + fromIntegral
                                  ((headerStaticSize `div` 8)
                                   + sampleCount * (sampleStaticSize `div` 8))
            !h                = bitBuilderBox (Proxy @(Header TrackRunFlagsIso5))
            !headerStaticSize = (natVal (Proxy @(BitRecordSize (Header TrackRunFlagsIso5))))
            !sampleStaticSize = (natVal (Proxy @(BitRecordSize (Sample TrackRunFlagsIso5))))
            !sampleCount      = fromIntegral (length samples)

        !body = mconcat $ s <$> samples
          where
            s (!duration, !bs) = bitBuilderBox (Proxy @(Sample TrackRunFlagsIso5)) duration size
              where
                !size = fromIntegral (BS.length bs)

newtype TrackRun where
  TrackRun :: BuilderBox -> TrackRun
  deriving (IsBoxContent)

instance IsBox TrackRun
type instance BoxTypeSymbol TrackRun = "trun"

-- class MkTrackRunArgs (t :: IsA (TrackRunFlags sop fsp sdp ssp sfp sop)) where
--   data TrRunArgs (t :: IsA (TrackRunFlags sop fsp sdp ssp sfp sop))

data TrackRunFlags
  (dataOffsetPresent :: Bool)
  (firstSampleFlagsPresent :: Bool)
  (sampleDurationPresent :: Bool)
  (sampleSizePresent :: Bool)
  (sampleFlagsPresent :: Bool)
  (sampleCompositionTimeOffsetPresent :: Bool)

data TrackRunFlagsIso5
  :: IsA (TrackRunFlags 'True 'False 'True 'True 'True 'False)

type Header (t :: IsA (TrackRunFlags dop fsp sdp ssp sfp sctop)) =
      "version"                   @: FieldU8  := 0 -- TODO allow version 1
  .+:                                Field 12 := 0
  .+: "sample-scto-present"       @: Flag     := sctop
  .+: "sample-flags-present"      @: Flag     := sfp
  .+: "sample-size-present"       @: Flag     := ssp
  .+: "sample-duration-present"   @: Flag     := sdp
  .+:                                Field  5 := 0
  .+: "first-sample-flag-present" @: Flag     := fsp
  .+:                                Flag     := 'False
  .+: "data-offset-preset"        @: Flag     := dop
  .+: "sample-count"              @: FieldU32
  .+: WhenR dop
        ('BitRecordMember ("data-offset"        @: FieldI32))
  :+: WhenR fsp
        ('BitRecordMember ("first-sample-flags" @: FieldU32))


type Sample (t :: IsA (TrackRunFlags dop fsp sdp ssp sfp sctop)) =
      WhenR sdp ('BitRecordMember ("sample-duration" @: FieldU32))
  :+: WhenR ssp ('BitRecordMember ("sample-size"     @: FieldU32))
  :+: WhenR sfp ('BitRecordMember ("sample-flags"    @: FieldU32 := 0x02000000)) -- TODO allow flags as in TrackExtends 
