{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.Type.BitRecords
import           Data.ByteString.Mp4.Boxes.BaseDescriptor



type ESDescriptor dependsOnEsId url ocrEsId =
                     "esId" :=> FieldU16
  :>: (FlagJust dependsOnEsId)
  :>: (FlagJust url)
  :>: (FlagJust ocrEsId)
  :>: "streamPriority"       :=> Field 5
  :>: dependsOnEsId
  :>: url
  :>: ocrEsId


-- * Interface from ISO 14496-3 (Audio)

staticESDescriptorAudio :: StaticBaseDescriptorWithArgs ESDescriptorAudio
staticESDescriptorAudio =
  staticBaseDescriptorWithArgs (ES_DescrP :: ESDescriptorAudio)

type ESDescriptorAudio = ES_DescrP (ESDescriptor NoBitRecord NoBitRecord NoBitRecord)

data AudioObjectType :: Nat -> Type

type instance ToBitRecord (AudioObjectType n) =
  AudioObjectTypeXField1 n :>: AudioObjectTypeXField2 n

type family AudioObjectTypeXField1 (n :: Nat) :: BitRecordField Nat where
  AudioObjectTypeXField1 n = If (30 <=? n) (Field 5 := n) (Field 5 := 31)

type family AudioObjectTypeXField2 (n :: Nat) :: BitRecord where
  AudioObjectTypeXField2 n = If (30 <=? n) 'EmptyBitRecord (ToBitRecord (Field 6 := (n - 32)))
