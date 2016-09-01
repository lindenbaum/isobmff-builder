{-# LANGUAGE UndecidableInstances #-}
-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.Mp4AudioSampleEntry where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Boxes
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor

import           Data.Type.BitRecords
import           Data.Type.Pretty


-- | A /body/ for 'AudioSampleEntry'. This 'IsBoxContent' with an
-- 'ElementaryStreamDescriptor' for ISO-14496-3 audio, with audio decoder
-- specific info.

-- | Create an 'AudioSampleEntry' with an 'AudioEsd'
audioSampleEntry
  :: AudioSampleEntry ()
  -> AudioEsd
  -> AudioSampleEntry (Box AudioEsd)
audioSampleEntry ase eds = const (Box eds) <$> ase

-- | Create an mp4 audio elementary stream descriptor full box
audioEsd
  :: Tagged "esId" Word16 -> Tagged "streamPriority" Word64 -> AudioEsd
audioEsd = runHoley $ hoistR AudioEsd $ bitBoxHoley $ Proxy @ESDescriptorSimple

-- | Consists of 'ElementaryStreamDescriptor's
newtype AudioEsd =
  AudioEsd (BitBox ESDescriptorSimple)
  deriving IsBoxContent

instance Default AudioEsd where
    def = audioEsd def def

instance IsBox AudioEsd
type instance BoxTypeSymbol AudioEsd = "mp4a"


-- * Interface from ISO 14496-3 (Audio)


-- ** Audio Object Type

data AudioObjecTypeId =
    AacMain                        -- ^ ISO 14496-4 subpart 4
  | AacLc                          -- ^ ISO 14496-4 subpart 4
  | AacSsr                         -- ^ ISO 14496-4 subpart 4
  | AacLtp                         -- ^ ISO 14496-4 subpart 4
  | Sbr                            -- ^ ISO 14496-4 subpart 4
  | AacScalable                    -- ^ ISO 14496-4 subpart 4
  | TwinVq                         -- ^ ISO 14496-4 subpart 4
  | Celp                           -- ^ ISO 14496-4 subpart 3
  | Hvxc                           -- ^ ISO 14496-4 subpart 2
  | AoReserved1
  | AoReserved2
  | Ttsi                           -- ^ ISO 14496-4 subpart 6
  | MainSunthetic                  -- ^ ISO 14496-4 subpart 5
  | WavetableSynthesis             -- ^ ISO 14496-4 subpart 5
  | GeneralMidi                    -- ^ ISO 14496-4 subpart 5
  | AlgorithmicSynthesisAndAudioFx -- ^ ISO 14496-4 subpart 5
  | ErAacLc                        -- ^ ISO 14496-4 subpart 4
  | AoReserved3
  | ErAacLtp                       -- ^ ISO 14496-4 subpart 4
  | ErAacScalable                  -- ^ ISO 14496-4 subpart 4
  | ErTwinVq                       -- ^ ISO 14496-4 subpart 4
  | ErBsac                         -- ^ ISO 14496-4 subpart 4
  | ErAacLd                        -- ^ ISO 14496-4 subpart 4
  | ErCelp                         -- ^ ISO 14496-4 subpart 3
  | ErHvxc                         -- ^ ISO 14496-4 subpart 2
  | ErHiln                         -- ^ ISO 14496-4 subpart 7
  | ErParametric                   -- ^ ISO 14496-4 subpart 2 or 7
  | Ssc                            -- ^ ISO 14496-4 subpart 8
  | AoReserved4
  | AoReserved5
  | AoCustom
  | AoLayer1                       -- ^ ISO 14496-4 subpart 9
  | AoLayer2                       -- ^ ISO 14496-4 subpart 9
  | AoLayer3                       -- ^ ISO 14496-4 subpart 9
  | AoDst                          -- ^ ISO 14496-4 subpart 10

type instance TableIndex AudioObjecTypeId 'AacMain                        = 1
type instance TableIndex AudioObjecTypeId 'AacLc                          = 2
type instance TableIndex AudioObjecTypeId 'AacSsr                         = 3
type instance TableIndex AudioObjecTypeId 'AacLtp                         = 4
type instance TableIndex AudioObjecTypeId 'Sbr                            = 5
type instance TableIndex AudioObjecTypeId 'AacScalable                    = 6
type instance TableIndex AudioObjecTypeId 'TwinVq                         = 7
type instance TableIndex AudioObjecTypeId 'Celp                           = 8
type instance TableIndex AudioObjecTypeId 'Hvxc                           = 9
type instance TableIndex AudioObjecTypeId 'AoReserved1                    = 10
type instance TableIndex AudioObjecTypeId 'AoReserved2                    = 11
type instance TableIndex AudioObjecTypeId 'Ttsi                           = 12
type instance TableIndex AudioObjecTypeId 'MainSunthetic                  = 13
type instance TableIndex AudioObjecTypeId 'WavetableSynthesis             = 14
type instance TableIndex AudioObjecTypeId 'GeneralMidi                    = 15
type instance TableIndex AudioObjecTypeId 'AlgorithmicSynthesisAndAudioFx = 16
type instance TableIndex AudioObjecTypeId 'ErAacLc                        = 17
type instance TableIndex AudioObjecTypeId 'AoReserved3                    = 18
type instance TableIndex AudioObjecTypeId 'ErAacLtp                       = 19
type instance TableIndex AudioObjecTypeId 'ErAacScalable                  = 20
type instance TableIndex AudioObjecTypeId 'ErTwinVq                       = 21
type instance TableIndex AudioObjecTypeId 'ErBsac                         = 22
type instance TableIndex AudioObjecTypeId 'ErAacLd                        = 23
type instance TableIndex AudioObjecTypeId 'ErCelp                         = 24
type instance TableIndex AudioObjecTypeId 'ErHvxc                         = 25
type instance TableIndex AudioObjecTypeId 'ErHiln                         = 26
type instance TableIndex AudioObjecTypeId 'ErParametric                   = 27
type instance TableIndex AudioObjecTypeId 'Ssc                            = 28
type instance TableIndex AudioObjecTypeId 'AoReserved4                    = 29
type instance TableIndex AudioObjecTypeId 'AoReserved5                    = 30
type instance TableIndex AudioObjecTypeId 'AoCustom                       = 31
type instance TableIndex AudioObjecTypeId 'AoLayer1                       = 32
type instance TableIndex AudioObjecTypeId 'AoLayer2                       = 33
type instance TableIndex AudioObjecTypeId 'AoLayer3                       = 34
type instance TableIndex AudioObjecTypeId 'AoDst                          = 35

type AudioObjecTypeRec n =
  'ReplacePretty
    (If (n <=? 30) "AudioObjectType" "ExtAudioObjectType" <:> PutHex8 n)
    (AudioObjectTypeField1 n :>: AudioObjectTypeField2 n)

type family AudioObjectTypeField1 (n :: Nat) :: BitRecordField where
  AudioObjectTypeField1 n = If (n <=? 30) (Field 5 := n) (Field 5 := 31)

type family AudioObjectTypeField2 (n :: Nat) :: BitRecord where
  AudioObjectTypeField2 n = If (n <=? 30) 'EmptyBitRecord (ToBitRecord (Field 6 := (n - 31)))

-- *** Sampling Frequency

data SamplingFreqTable =
      SF96000
    | SF88200
    | SF64000
    | SF48000
    | SF44100
    | SF32000
    | SF24000
    | SF22050
    | SF16000
    | SF12000
    | SF11025
    | SF8000
    | SF7350
    | SFReserved1
    | SFReserved2
    | SFCustom

type instance TableIndex SamplingFreqTable 'SF96000     = 0
type instance TableIndex SamplingFreqTable 'SF88200     = 1
type instance TableIndex SamplingFreqTable 'SF64000     = 2
type instance TableIndex SamplingFreqTable 'SF48000     = 3
type instance TableIndex SamplingFreqTable 'SF44100     = 4
type instance TableIndex SamplingFreqTable 'SF32000     = 5
type instance TableIndex SamplingFreqTable 'SF24000     = 6
type instance TableIndex SamplingFreqTable 'SF22050     = 7
type instance TableIndex SamplingFreqTable 'SF16000     = 8
type instance TableIndex SamplingFreqTable 'SF12000     = 9
type instance TableIndex SamplingFreqTable 'SF11025     = 0xa
type instance TableIndex SamplingFreqTable 'SF8000      = 0xb
type instance TableIndex SamplingFreqTable 'SF7350      = 0xc
type instance TableIndex SamplingFreqTable 'SFReserved1 = 0xd
type instance TableIndex SamplingFreqTable 'SFReserved2 = 0xe
type instance TableIndex SamplingFreqTable 'SFCustom    = 0xf

type instance ToBitRecordField (x :: SamplingFreqTable) =
  Field 4 := (TableIndex SamplingFreqTable x)

type instance ToBitRecord (x :: SamplingFreqTable) = ToFreqTableRecord x

type family ToFreqTableRecord (x :: SamplingFreqTable) :: BitRecord where
  ToFreqTableRecord 'SFCustom = ToBitRecordField 'SFCustom :>: "samplingFrequency" :=> Field 24
  ToFreqTableRecord x = ToBitRecord (ToBitRecordField x)
