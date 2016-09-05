{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.SyncLayerConfigDescriptor
import           Data.ByteString.Mp4.Boxes.DecoderSpecificInfo

data ESDescriptor dependsOnEsId url ocrEsId
  :: IsA DecoderSpecificInfo
  -> IsA (Descriptor 'SLConfigDescr)
  -> IsA (Descriptor 'ES_Descr)

type instance Eval (ESDescriptor dependsOnEsId url ocrEsId decConfig slConfig)  =
  'MkDescriptor
     (PutStr "elementary-stream-descriptor" #$
       "esId" :=> FieldU16
      :>: "dependsOnEsId" :=> Flag := dependsOnEsId
      :>: "urlFlag" :=> Flag := url
      :>: "ocrEsIdFlag" :=> Flag := ocrEsId
      :>: "streamPriority" :=> Field 5
      :>: (WhenR dependsOnEsId ("depEsId" :=> FieldU16))
      :>: (WhenR url SizedString)
      :>: (WhenR ocrEsId ("ocrEsId" :=> FieldU16))
      :>: decConfig
      :>: slConfig
      -- TODO add the rest of the ESDescriptor
     )

-- | ISO-14496-14 section 3.1.2 defines restrictions of the elementary stream
-- descriptor.
-- TODO seperate this and other modules so theres the same seperation as in between
-- the parts of the standard.
type ESDescriptorMp4File decInfo = ESDescriptor 'False 'False 'False decInfo Mp4SyncLayerDescriptor
type ESDescriptorMp4Url decInfo = ESDescriptor 'False 'True 'False decInfo Mp4SyncLayerDescriptor
