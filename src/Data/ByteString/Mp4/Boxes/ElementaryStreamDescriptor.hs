{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.Type.BitRecords
import           Data.ByteString.Mp4.Boxes.BaseDescriptor


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

staticESDescriptorSimple
  :: Tagged "esId" Word16
  -> Tagged "streamPriority" Word64
  -> BitBox ESDescriptorSimple
staticESDescriptorSimple = bitBoxWithArgs (Proxy @ESDescriptorSimple)
