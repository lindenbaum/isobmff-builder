{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.DecoderConfigDescriptor where

import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.DecoderSpecificInfo
import           Data.Type.BitRecords
import           Data.Type.Pretty
import           Data.Word
import           Data.Kind.Extra

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
        :: IsA BitRecord
  where
    DecoderConfigDescriptorBody ot st di ps =
      (PutStr "decoder-config-descriptor" <+>
        ("objectTypeIndication" <:> PutHex8 (FromEnum ObjectTypeIndication ot)) <+>
        ("streamType"           <:> PutHex8 (FromEnum StreamType           st)))
      #$ ((SetEnum ObjectTypeIndicationEnum ot ~~> BitRecord)
           :>: (SetEnum StreamTypeEnum st ~~> BitRecord)
           :>: "upstream"@: Flag
           .>: "reserved"@: Field 1        :=  1
           .>: "bufferSizeDB" @: Field 24
           .>: "maxBitrate"   @: FieldU32
           .>: "avgBitrate"   @: FieldU32
           .>: (Pure (CoerceListTo BitRecord (di ?:: LengthIn 0 1)) ~~> BitRecord)
           :>: (Pure (CoerceListTo BitRecord (ps ?:: LengthIn 0 255)) ~~> BitRecord)
         )

-- ** 'ProfileLevelIndicationIndexDescriptor'

data ProfileLevelIndicationIndexDescriptor
  :: FieldValue Word8
  -> IsA (Descriptor 'ProfileLevelIndicationIndexDescr)
type instance Eval (ProfileLevelIndicationIndexDescriptor val) =
  'MkDescriptor
  (("profileLevelIndicationIndex" @: FieldU8 :~ val) ~~> BitRecord)
