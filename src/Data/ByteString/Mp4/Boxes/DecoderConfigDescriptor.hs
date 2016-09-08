{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.DecoderConfigDescriptor where

import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.DecoderSpecificInfo
import           Data.Type.BitRecords
import           Data.Type.Pretty

import           Data.Kind.Extra

-- | Information about what decoder is required for the an elementary stream.
-- The stream type indicates the general category of the stream and.
data DecoderConfigDescriptor
       (ot :: ObjectTypeIndication)
       (st :: StreamType)
          :: [DecoderSpecificInfo ot st]
          -> [ProfileLevelIndicationIndexDescriptor]
          -> IsA (Descriptor 'DecoderConfigDescr)

type instance Eval (DecoderConfigDescriptor ot st di ps) =
  'MkDescriptor (DecoderConfigDescriptorBody ot st di ps)

type family
    DecoderConfigDescriptorBody
      ot st
      (di :: [DecoderSpecificInfo ot st])
      (ps :: [ProfileLevelIndicationIndexDescriptor])
        :: BitRecord
  where
    DecoderConfigDescriptorBody ot st di ps =
      (PutStr "decoder-config-descriptor" <+>
        ("objectTypeIndication" <:> PutHex8 (FromEnum ObjectTypeIndication ot)) <+>
        ("streamType"           <:> PutHex8 (FromEnum StreamType           st)))
      #$ (StaticEnumRecord ObjectTypeIndicationEnum ot
           :>: StaticEnumRecord StreamTypeEnum st
           :>: "upstream"@: Flag
           :>: "reserved"@: Field 1        :=  1
           :>: "bufferSizeDB" @: Field 24
           :>: "maxBitrate"   @: FieldU32
           :>: "avgBitrate"   @: FieldU32
           :>: (di ?:: LengthIn 0 1)
           :>: (ps ?:: LengthIn 0 255)
         )

-- ** 'ProfileLevelIndicationIndexDescriptor'

data ProfileLevelIndicationIndexDescriptor =
  MkProfileLevelIndicationIndexDescriptor BitRecordField

type instance Eval (SetWith
                    (p :: IsA ProfileLevelIndicationIndexDescriptor)
                    (OverwriteWith n)) =
  'MkProfileLevelIndicationIndexDescriptor (FieldU8 := (Assert (NatIn 0 255) n))

type instance Eval (SetWith
                    (p :: IsA ProfileLevelIndicationIndexDescriptor)
                    (NamedRuntimeParameter label)) =
  'MkProfileLevelIndicationIndexDescriptor (label @: FieldU8)

type instance
  Eval ('MkProfileLevelIndicationIndexDescriptor field
        ~~> Descriptor 'ProfileLevelIndicationIndexDescr) =
  'MkDescriptor (PutStr "profile-level-indication-index-descriptor" #$ 'BitRecordMember field)
