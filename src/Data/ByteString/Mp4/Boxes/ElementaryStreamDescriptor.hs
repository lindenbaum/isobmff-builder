{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.SyncLayerConfigDescriptor

-- * Esd Box

newtype EsdBox (d :: IsA (Descriptor 'ES_Descr)) where
  EsdBox :: forall (d :: IsA (Descriptor 'ES_Descr)) . BitBox (d ~~> BitRecord) -> EsdBox d

deriving instance KnownNat (BitRecordSize (Eval (d ~~> BitRecord)))
  => IsBoxContent (EsdBox d)

-- * Esd Record

type DefaultEsId = 'StaticFieldValue 1

data ESDescriptor
  :: FieldValue (Tagged "esId" Word16)
  -> Maybe (FieldValue (Tagged "depEsId" Word16))
  -> Maybe (IsA BitRecordField)
  -> Maybe (FieldValue (Tagged "ocrEsId" Word16))
  -> FieldValue (Tagged "streamPrio" Word64)
  -> IsA (Descriptor 'DecoderConfigDescr)
  -> IsA (Descriptor 'SLConfigDescr)
  -> IsA (Descriptor 'ES_Descr)

-- | ISO-14496-14 section 3.1.2 defines restrictions of the elementary stream
-- descriptor.
-- TODO seperate this and other modules so theres the same seperation as in between
-- the parts of the standard.
type ESDescriptorMp4File esId decInfo =
  ESDescriptor esId 'Nothing 'Nothing 'Nothing  ('StaticFieldValue 1) decInfo Mp4SyncLayerDescriptor

type instance Eval (ESDescriptor esId depEsId url ocrEsId streamPrio decConfig slConfig)  =
  'MkDescriptor
     (PutStr "elementary-stream-descriptor" #$
          FieldU16 :~ esId
      .>: "depEsIdFlag" @: FlagJust depEsId
      .>: "urlFlag" @: FlagJust url
      .>: "ocrEsIdFlag" @: FlagJust ocrEsId
      .>: "streamPriority" @: Field 5 :~ streamPrio
      .>: "depEsId" @: FieldU16 :~? depEsId
      :>: PutStr "url" #: OptionalRecordOf url
      :>: "ocrEsId" @: FieldU16 :~? ocrEsId
      :>: FromA decConfig
      :>: FromA slConfig
      -- TODO add the rest of the ESDescriptor
     )
