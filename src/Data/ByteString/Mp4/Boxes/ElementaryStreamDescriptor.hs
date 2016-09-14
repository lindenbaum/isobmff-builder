{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints  #-}
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Util.FullBox
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.SyncLayerConfigDescriptor

-- * Esd Box

type EsdBox = Box (FullBox (BuilderBox ESD) 0)
data ESD
type instance BoxTypeSymbol ESD = "esds"

esdBox :: forall (record :: IsA (Descriptor 'ES_Descr)) (rendered :: BitRecord) .
         ( BitStringBuilderHoley (Proxy rendered) EsdBox
         , rendered ~ (RenderEsDescr record))
       => Proxy record -> ToBitStringBuilder (Proxy rendered) EsdBox
esdBox =
  runHoley
  . esdBoxHoley

esdBoxHoley :: forall (record :: IsA (Descriptor 'ES_Descr)) r (rendered :: BitRecord) .
               ( BitStringBuilderHoley (Proxy rendered) r
               , rendered ~ (RenderEsDescr record)
               )
             => Proxy record -> Holey EsdBox r (ToBitStringBuilder (Proxy rendered) r)
esdBoxHoley _p =
  hoistM (fullBox 0 :: BuilderBox ESD -> EsdBox) $
  bitBuilderBox (Proxy @ESD) (Proxy @rendered)

type RenderEsDescr (d :: IsA (Descriptor 'ES_Descr)) =
  (Eval (BitRecordOfDescriptor $~ (Eval d)) :: BitRecord)

-- * Esd Record

data ESDescriptor
  :: IsA (FieldValue "esId" Nat)
  -> Maybe (IsA (FieldValue "depEsId" Nat))
    -- TODO Improve the custom field and also the sizedstring API
  -> Maybe (IsA (BitRecordField ('MkFieldCustom :: BitField ASizedString ASizedString (urlSize :: Nat))))
  -> Maybe (IsA (FieldValue "ocrEsId" Nat))
  -> IsA (FieldValue "streamPrio" Nat)
  -> IsA (Descriptor 'DecoderConfigDescr)
  -> IsA (Descriptor 'SLConfigDescr)
  -> IsA (Descriptor 'ES_Descr)

-- | ISO-14496-14 section 3.1.2 defines restrictions of the elementary stream
-- descriptor.
-- TODO seperate this and other modules so theres the same seperation as in between
-- the parts of the standard.
type ESDescriptorMp4File esId decConfigDescr =
  ESDescriptor esId 'Nothing 'Nothing
               'Nothing DefaultStreamPrio
               decConfigDescr Mp4SyncLayerDescriptor

type DefaultEsId = StaticFieldValue "esId" 1
type DefaultStreamPrio = StaticFieldValue "streamPrio" 0

type instance
  Eval (ESDescriptor esId depEsId url ocrEsId streamPrio decConfig slConfig) =
  'MkDescriptor
     (PutStr "elementary-stream-descriptor"
      #$  "esId" @: FieldU16 :~ esId
      .>: "depEsIdFlag" @: FlagJust depEsId
      .>: "urlFlag" @: FlagJust url
      .>: "ocrEsIdFlag" @: FlagJust ocrEsId
      .>: "streamPriority" @: Field 5 :~ streamPrio
      .>: "depEsId" @: FieldU16 :~? depEsId
      :>: (PutStr "url" #: OptionalRecordOf (Fun1 RecordField) url)
      :>: "ocrEsId" @: FieldU16 :~? ocrEsId
      :>: (decConfig :>>=: BitRecordOfDescriptor)
      :>: (slConfig :>>=: BitRecordOfDescriptor)

      -- TODO add the rest of the ESDescriptor
     )
