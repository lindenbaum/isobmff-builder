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
  AudioObjectTypeField1 n :>: AudioObjectTypeField2 n

type family AudioObjectTypeField1 (n :: Nat) :: BitRecordField where
  AudioObjectTypeField1 n = If (n <=? 30) (Field 5 := n) (Field 5 := 31)

type family AudioObjectTypeField2 (n :: Nat) :: BitRecord where
  AudioObjectTypeField2 n = If (n <=? 30) 'EmptyBitRecord (ToBitRecord (Field 6 := (n - 31)))
