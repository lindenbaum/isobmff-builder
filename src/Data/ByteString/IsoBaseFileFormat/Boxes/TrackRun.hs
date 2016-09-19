-- | A list of sample offsets and sizes of a track. This tells the decoder where
-- stuff inside an 'mdat' box is, and also where the 'mdat' box /starts/, which
-- is the reason why currently there are several TODOs concercning the offset
-- calculation.
module Data.ByteString.IsoBaseFileFormat.Boxes.TrackRun where


import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import Data.ByteString.IsoBaseFileFormat.Util.FullBox
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import Data.ByteString.IsoBaseFileFormat.Boxes.SpecificMediaHeader
import Data.ByteString.IsoBaseFileFormat.ReExports


-- | Create a sound media header data box.
trackRunIso5 :: I32 "trun_offset_from_moof" -> [ -> Box (FullBox (TrackRun 0)
TrackRunIso5 = fullBox 0

trackRunSampleIso5
  :: Word32
  -> Word32
  -> TrackRunSampleFlagsIso5
  -> TrackRunSample TrackRunFlagsIso5
trackRunSampleIso5 sampleDur sampleSize sampleFlags

-- |  TODO reuse  TrackExtendsDefaultSampleFlags
data TrackRunSampleFlagsIso5 = IFrameLikeSample


-- | Track run box.
newtype TrackRun (t :: IsA (TrackRunFlags dop fsp sdp sfp sctop))  where
  TrackRun ::
    Template (I32 "data_offset") 0 :+  (U16 "reserved") 0 -> SoundMediaHeader
  deriving (Default, IsBoxContent)

data TrackRunFlagsIso5
  :: IsA (TrackRunFlags 'True 'False 'True 'True 'True 'False)

data TrackRunFlags
  (dataOffsetPresent :: Bool)
  (firstSampleFlagsPresent :: Bool)
  (sampleDurationPresent :: Bool)
  (sampleSizePresent :: Bool)
  (sampleFlagsPresent :: Bool)
  (sampleCompositionTimeOffsetPresent :: Bool)

newtype TrackRunSample (tflags ::  IsA (TrackRunFlags dop fsp sdp sfp sctop)) where
  TrackRunSample :: BuilderBox -> TrackRunSample tflags
  deriving (IsBoxContent)




--

-- newtype instance TrackRunSample TrackRunFlagsIso5 where
--   TrunSampleIso5 ::
--     U32 "sample_duration"
--     U32 "sample_size"
--     Template (U32 "sample_flags")



instance IsBox TrackRun
type instance BoxTypeSymbol TrackRun = "trun"
