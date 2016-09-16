{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.DecoderConfigDescriptor where

import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.DecoderSpecificInfo
import           Data.Type.BitRecords
import           Data.Type.Pretty
import           Data.Kind.Extra
import           GHC.TypeLits

-- | Information about what decoder is required for the an elementary stream.
-- The stream type indicates the general category of the stream and.
data DecoderConfigDescriptor
       (ot :: ObjectTypeIndication)
       (st :: StreamType)
          :: [IsA (DecoderSpecificInfo ot st)]
          -> [IsA (Descriptor 'ProfileLevelIndicationIndexDescr)]
          -> IsA (Descriptor 'DecoderConfigDescr)

type instance Eval (DecoderConfigDescriptor ot st di ps) =
  'MkDescriptor (DecoderConfigDescriptorBody ot st di ps)

type family
    DecoderConfigDescriptorBody
      ot st
      (di :: [IsA (DecoderSpecificInfo ot st)])
      (ps :: [IsA (Descriptor 'ProfileLevelIndicationIndexDescr)])
        :: BitRecord
  where
    DecoderConfigDescriptorBody ot st di ps =
      PutStr "decoder-config-descriptor"
        #+$ (BitRecordOfEnum (SetEnum "objectTypeIndication" ObjectTypeIndicationEnum ot)
              :+: BitRecordOfEnum (SetEnum "objectTypeIndication" StreamTypeEnum st)
              :+: "upstream" @: Flag
              .+: "reserved" @: Field 1        :=  1
              .+: "bufferSizeDB" @: Field 24
              .+: "maxBitrate"   @: FieldU32
              .+: "avgBitrate"   @: FieldU32
              .+: Eval (BitRecordOfList
                        (DescriptorOfDecoderSpecificInfo
                         :^>>>: BitRecordOfDescriptor)
                        (di ?:: LengthIn 0 1))
              :+: Eval (BitRecordOfList
                        (Extract :>>>: BitRecordOfDescriptor)
                        (ps ?:: LengthIn 0 255)))

-- ** 'ProfileLevelIndicationIndexDescriptor'

data ProfileLevelIndicationIndexDescriptor
  :: IsA (FieldValue "profileLevelIndicationIndex" Nat)
  -> IsA (Descriptor 'ProfileLevelIndicationIndexDescr)

type instance Eval (ProfileLevelIndicationIndexDescriptor val) =
  'MkDescriptor
  ('BitRecordMember (FieldU8 :~ val))
