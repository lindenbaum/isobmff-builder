{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.Type.BitRecords
import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.Type.Pretty


type ESDescriptor dependsOnEsId url ocrEsId =
  BaseDescriptor
     'ES_Descr
     (   "esId" :=> FieldU16
      :>: dependsOnEsId
      :>: url
      :>: ocrEsId
      :>: "streamPriority" :=> Field 5
      :>: (WhenR dependsOnEsId ("depEsId" :=> FieldU16))
      :>: (WhenR url SizedString)
      :>: (WhenR ocrEsId ("ocrEsId" :=> FieldU16)))

type ESDescriptorSimple = ESDescriptor 'False 'False 'False

-- * Interface from ISO 14496-3 (Audio)

staticESDescriptorAudio :: Tagged "esId" Word16 -> Tagged "streamPriority" Word64 -> BitBox ESDescriptorSimple
staticESDescriptorAudio = bitBoxWithArgs (Proxy @ESDescriptorSimple)

data AudioObjectType where
  AudioObjectType :: Nat -> AudioObjectType

type instance ToBitRecord ('AudioObjectType n) =
  'ReplacePretty
    (If (n <=? 30) ("AudioObjectType") ("ExtAudioObjectType") <:> PutHex8 n)
    (AudioObjectTypeField1 n :>: AudioObjectTypeField2 n)

type family AudioObjectTypeField1 (n :: Nat) :: BitRecordField where
  AudioObjectTypeField1 n = If (n <=? 30) (Field 5 := n) (Field 5 := 31)

type family AudioObjectTypeField2 (n :: Nat) :: BitRecord where
  AudioObjectTypeField2 n = If (n <=? 30) 'EmptyBitRecord (ToBitRecord (Field 6 := (n - 31)))
