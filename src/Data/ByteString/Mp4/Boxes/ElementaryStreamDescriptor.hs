{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.SyncLayerConfigDescriptor

-- * Esd Box

type ABitRecordOfAnESDescriptor (d :: IsA (Descriptor 'ES_Descr))=
  ((d :>>=: BitRecordOfDescriptor) :: IsA BitRecord)

newtype EsdBox (d :: IsA (Descriptor 'ES_Descr)) where
  EsdBox ::
    forall (d :: IsA (Descriptor 'ES_Descr)) .
    BitBox (ABitRecordOfAnESDescriptor d) -> EsdBox d

deriving instance KnownNat (BitRecordSize (Eval (ABitRecordOfAnESDescriptor d)))
  => IsBoxContent (EsdBox d)

-- * Esd Record

data ESDescriptor -- TODO reduce all the IsA
  :: IsA (FieldValue "esId" Nat)
  -> Maybe (IsA (FieldValue "depEsId" Nat))
  -> Maybe (IsA (BitRecordField t))
  -> Maybe (IsA (FieldValue "ocrEsId" Nat))
  -> IsA (FieldValue "streamPrio" Nat)
  -> IsA (Descriptor 'DecoderConfigDescr)
  -> IsA (Descriptor 'SLConfigDescr)
  -> IsA (Descriptor 'ES_Descr)

-- | ISO-14496-14 section 3.1.2 defines restrictions of the elementary stream
-- descriptor.
-- TODO seperate this and other modules so theres the same seperation as in between
-- the parts of the standard.
type ESDescriptorMp4File esId decInfo =
  ESDescriptor esId 'Nothing 'Nothing 'Nothing DefaultStreamPrio decInfo Mp4SyncLayerDescriptor

type DefaultEsId = StaticFieldValue "esId" 1
type DefaultStreamPrio = StaticFieldValue "streamPrio" 1

type instance
  Eval (ESDescriptor esId depEsId url ocrEsId streamPrio decConfig slConfig) =
  'MkDescriptor
     (PutStr "elementary-stream-descriptor" #$
          FieldU16 :~ esId
      .>: "depEsIdFlag" @: FlagJust depEsId
      .>: "urlFlag" @: FlagJust url
      .>: "ocrEsIdFlag" @: FlagJust ocrEsId
      .>: "streamPriority" @: Field 5 :~ streamPrio
      .>: "depEsId" @: FieldU16 :~? depEsId
      :>: PutStr "url" #: OptionalRecordOf (Fun1 RecordField) url
      :>: "ocrEsId" @: FieldU16 :~? ocrEsId
      :>: (decConfig :>>=: BitRecordOfDescriptor)
      :>: (slConfig :>>=: BitRecordOfDescriptor)
      -- TODO add the rest of the ESDescriptor
     )
