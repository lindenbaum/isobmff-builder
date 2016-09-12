{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.ByteString.IsoBaseFileFormat.Util.FullBox
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.SyncLayerConfigDescriptor

-- * Esd Box

esdBox :: forall (d :: IsA (Descriptor 'ES_Descr)) .
  ( KnownNat (BitRecordSize (ABitRecordOfAnESDescriptor d))
  , BitStringBuilderHoley (Proxy (ABitRecordOfAnESDescriptor d)) (Box (EsdBox d)))
  => Proxy d
  -> ToBitStringBuilder (Proxy (ABitRecordOfAnESDescriptor d)) (Box (EsdBox d))
esdBox = runHoley . esdBoxHoley

esdBoxHoley :: forall (d :: IsA (Descriptor 'ES_Descr)) r .
  ( KnownNat (BitRecordSize (ABitRecordOfAnESDescriptor d))
  , BitStringBuilderHoley (Proxy (ABitRecordOfAnESDescriptor d)) r)
  => Proxy d
  -> Holey
      (Box (EsdBox d)) r
      (ToBitStringBuilder (Proxy (ABitRecordOfAnESDescriptor d)) r)
esdBoxHoley _proxyD =
  hoistM
  ((fullBox 0 :: (EsdBoxContent d) -> Box (EsdBox d))
   . (EsdBoxContent :: BitBox (ABitRecordOfAnESDescriptor d) -> EsdBoxContent d))
  (bitBoxHoley (Proxy @(ABitRecordOfAnESDescriptor d)))

type EsdBox (d :: IsA (Descriptor 'ES_Descr)) =
  FullBox (EsdBoxContent d) 0

data EsdBoxContent (d :: IsA (Descriptor 'ES_Descr)) where
  EsdBoxContent ::
    forall (d :: IsA (Descriptor 'ES_Descr))
    . KnownNat (BitRecordSize (ABitRecordOfAnESDescriptor d))
    => BitBox (ABitRecordOfAnESDescriptor d)
    -> EsdBoxContent d

type instance BoxTypeSymbol (EsdBoxContent d) = "esds"

instance forall (d :: IsA (Descriptor 'ES_Descr)) .
         IsBoxContent (EsdBoxContent d)
         => IsBox (EsdBoxContent d)

instance forall (d :: IsA (Descriptor 'ES_Descr)) .
    IsBoxContent (BitBox (ABitRecordOfAnESDescriptor d))
    => IsBoxContent (EsdBoxContent d)
  where
    boxSize (EsdBoxContent bb) = boxSize bb
    boxBuilder (EsdBoxContent bb) = boxBuilder bb

type ABitRecordOfAnESDescriptor (d :: IsA (Descriptor 'ES_Descr))=
  (Eval (d :>>=: BitRecordOfDescriptor) :: BitRecord)

-- * Esd Record

data ESDescriptor
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
