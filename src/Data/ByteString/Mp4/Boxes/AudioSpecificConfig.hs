{-# LANGUAGE UndecidableInstances #-}
-- | @mp4a@ Audio sample entry according to ISO 14496-14
module Data.ByteString.Mp4.Boxes.AudioSpecificConfig where

import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.DecoderSpecificInfo

-- * Interface from ISO 14496-3 (Audio)

-- | A minimalistic audio config without SBR
-- TODO add error protection specific config
-- TODO add sbr support
data NonSbrAudioConfig
  :: AudioObjectTypeId
  -> IsAn AudioSubConfig
  -> IsAn (EnumOf SamplingFreqTable)
  -> IsAn (EnumOf ChannelConfigTable)
  -> IsA (DecoderSpecificInfo 'AudioIso14496_3 'AudioStream)

type instance
  Eval (NonSbrAudioConfig
        aoId
        subCfg
        freq
        channels) =
   ('MkDecoderSpecificInfo
    (("audio-specific-config" <:> PutHex8 (FromEnum AudioObjectTypeId aoId))
     #$ (AudioConfigBeginning
         aoId
         (BitRecordOfEnum freq)
         (BitRecordOfEnum channels)
         :>: (BitRecordOfAudioSubConfig subCfg))))

type AudioConfigBeginning audioObjId freq channels =
      AudioObjectTypeRec audioObjId
  :>: freq
  :>: channels

-- ** Audio Object Type

data AudioObjectTypeId =
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

type instance FromEnum AudioObjectTypeId 'AacMain                        = 1
type instance FromEnum AudioObjectTypeId 'AacLc                          = 2
type instance FromEnum AudioObjectTypeId 'AacSsr                         = 3
type instance FromEnum AudioObjectTypeId 'AacLtp                         = 4
type instance FromEnum AudioObjectTypeId 'Sbr                            = 5
type instance FromEnum AudioObjectTypeId 'AacScalable                    = 6
type instance FromEnum AudioObjectTypeId 'TwinVq                         = 7
type instance FromEnum AudioObjectTypeId 'Celp                           = 8
type instance FromEnum AudioObjectTypeId 'Hvxc                           = 9
type instance FromEnum AudioObjectTypeId 'AoReserved1                    = 10
type instance FromEnum AudioObjectTypeId 'AoReserved2                    = 11
type instance FromEnum AudioObjectTypeId 'Ttsi                           = 12
type instance FromEnum AudioObjectTypeId 'MainSunthetic                  = 13
type instance FromEnum AudioObjectTypeId 'WavetableSynthesis             = 14
type instance FromEnum AudioObjectTypeId 'GeneralMidi                    = 15
type instance FromEnum AudioObjectTypeId 'AlgorithmicSynthesisAndAudioFx = 16
type instance FromEnum AudioObjectTypeId 'ErAacLc                        = 17
type instance FromEnum AudioObjectTypeId 'AoReserved3                    = 18
type instance FromEnum AudioObjectTypeId 'ErAacLtp                       = 19
type instance FromEnum AudioObjectTypeId 'ErAacScalable                  = 20
type instance FromEnum AudioObjectTypeId 'ErTwinVq                       = 21
type instance FromEnum AudioObjectTypeId 'ErBsac                         = 22
type instance FromEnum AudioObjectTypeId 'ErAacLd                        = 23
type instance FromEnum AudioObjectTypeId 'ErCelp                         = 24
type instance FromEnum AudioObjectTypeId 'ErHvxc                         = 25
type instance FromEnum AudioObjectTypeId 'ErHiln                         = 26
type instance FromEnum AudioObjectTypeId 'ErParametric                   = 27
type instance FromEnum AudioObjectTypeId 'Ssc                            = 28
type instance FromEnum AudioObjectTypeId 'AoReserved4                    = 29
type instance FromEnum AudioObjectTypeId 'AoReserved5                    = 30
type instance FromEnum AudioObjectTypeId 'AoCustom                       = 31
type instance FromEnum AudioObjectTypeId 'AoLayer1                       = 32
type instance FromEnum AudioObjectTypeId 'AoLayer2                       = 33
type instance FromEnum AudioObjectTypeId 'AoLayer3                       = 34
type instance FromEnum AudioObjectTypeId 'AoDst                          = 35

type AudioObjectTypeRec n =
    (If ((FromEnum AudioObjectTypeId n) <=? 30)
            "AudioObjectType"
            "ExtAudioObjectType") <:> PutHex8 (FromEnum AudioObjectTypeId n)
    #$ AudioObjectTypeField1 (FromEnum AudioObjectTypeId n)
    .>: AudioObjectTypeField2 (FromEnum AudioObjectTypeId n)

type family AudioObjectTypeField1 (n :: Nat)
  :: IsA (BitRecordField ('MkFieldBits :: BitField Word64 Nat 5)) where
  AudioObjectTypeField1 n =
    If (n <=? 30) (Field 5 := n) (Field 5 := 31)

type family AudioObjectTypeField2 (n :: Nat) :: IsA BitRecord where
  AudioObjectTypeField2 n =
    If (n <=? 30) (Return 'EmptyBitRecord) (RecordField (Field 6 := (n - 31)))

-- *** Sampling Frequency

type SamplingFreq = ExtEnum SamplingFreqTable 4 'SFCustom (Field 24)

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

type instance FromEnum SamplingFreqTable 'SF96000     = 0
type instance FromEnum SamplingFreqTable 'SF88200     = 1
type instance FromEnum SamplingFreqTable 'SF64000     = 2
type instance FromEnum SamplingFreqTable 'SF48000     = 3
type instance FromEnum SamplingFreqTable 'SF44100     = 4
type instance FromEnum SamplingFreqTable 'SF32000     = 5
type instance FromEnum SamplingFreqTable 'SF24000     = 6
type instance FromEnum SamplingFreqTable 'SF22050     = 7
type instance FromEnum SamplingFreqTable 'SF16000     = 8
type instance FromEnum SamplingFreqTable 'SF12000     = 9
type instance FromEnum SamplingFreqTable 'SF11025     = 0xa
type instance FromEnum SamplingFreqTable 'SF8000      = 0xb
type instance FromEnum SamplingFreqTable 'SF7350      = 0xc

-- *** Channel Config (Mono, Stereo, 7-1 Surround, ...)

type ChannelConfig = FixedEnum ChannelConfigTable 4

data ChannelConfigTable =
    GasChannelConfig
  | SingleChannel
  | ChannelPair
  | SinglePair
  | SinglePairSingle
  | SinglePairPair
  | SinglePairPairLfe
  | SinglePairPairPairLfe

type instance FromEnum ChannelConfigTable 'GasChannelConfig = 1
type instance FromEnum ChannelConfigTable 'SingleChannel = 2
type instance FromEnum ChannelConfigTable 'ChannelPair = 3
type instance FromEnum ChannelConfigTable 'SinglePair = 4
type instance FromEnum ChannelConfigTable 'SinglePairSingle = 5
type instance FromEnum ChannelConfigTable 'SinglePairPair = 6
type instance FromEnum ChannelConfigTable 'SinglePairPairLfe = 7
type instance FromEnum ChannelConfigTable 'SinglePairPairPairLfe = 8


-- ** More Specific audio decoder config

data AudioSubConfig :: Type

type family BitRecordOfAudioSubConfig (x :: IsA AudioSubConfig) :: IsA BitRecord

data GASpecificConfig
  (frameLenFlag   :: IsA (FieldValue "frameLenFlag" Bool))
  (coreCoderDelay :: Maybe (IsA (FieldValue "coreCoderDelay" Nat)))
  (extension      :: IsA GASExtension)
  :: IsA AudioSubConfig

type DefaultGASpecificConfig = GASpecificConfig (StaticFieldValue "frameLenFlag" 'False) 'Nothing MkGASExtension

type instance Eval (GASpecificConfig fl cd ext)
  = TypeError ('Text "AudioSubConfig is abstract!")


type instance
  BitRecordOfAudioSubConfig (GASpecificConfig fl cd ext) =
     (    Flag :~ fl
      .>: Field 14 :~? cd
      :>: BitRecordOfGASExtension ext
     )

-- | TODO implment that GAS extensions
data GASExtension
data MkGASExtension :: IsA GASExtension

type BitRecordOfGASExtension (x :: IsA GASExtension) =
  RecordField ("has-gas-extension" @: Flag := 'False)
