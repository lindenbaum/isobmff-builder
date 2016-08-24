{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.BaseDescriptor where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.Expandable
import           Data.Type.BitRecords
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields

-- * The runtime base constructor

-- | The base descriptor for runtime parameters
newtype BaseDescriptor t where
        BaseDescriptor :: Expandable t -> BaseDescriptor t

instance (KnownNat (GetClassTag t), IsBoxContent t) =>
         IsBoxContent (BaseDescriptor t) where
    boxSize (BaseDescriptor et) =
        1 + boxSize et
    boxBuilder (BaseDescriptor et) =
        word8 (fromIntegral (natVal (Proxy :: Proxy (GetClassTag t))))
            <> boxBuilder et

-- * Static base constructor

type StaticBaseDescrHoley t =
    (KnownDescriptor t, ToHoley BitStringBuilder (Proxy (StaticBaseDescriptorContent t)) (StaticBaseDescriptor t))
type StaticBaseDescriptorWithArgs t =
    ToM BitStringBuilder (Proxy (StaticBaseDescriptorContent t)) (StaticBaseDescriptor t)

staticBaseDescriptorWithArgs :: (StaticBaseDescrHoley t)
                         => t
                         -> StaticBaseDescriptorWithArgs t
staticBaseDescriptorWithArgs prec = runHoley $ staticBaseDescriptorHoley prec

staticBaseDescriptor :: ( KnownDescriptor t
                       , ToHoley BitStringBuilder (Proxy (StaticBaseDescriptorContent t)) (StaticBaseDescriptor t)
                       , ToM BitStringBuilder (Proxy (StaticBaseDescriptorContent t)) (StaticBaseDescriptor t) ~ (StaticBaseDescriptor t))
                     => t
                     -> StaticBaseDescriptor t
staticBaseDescriptor prec = runHoley $ staticBaseDescriptorHoley prec

staticBaseDescriptorHoley :: forall t r .
                          ( KnownDescriptor t
                          , ToHoley BitStringBuilder (Proxy (StaticBaseDescriptorContent t)) r)
                          => t
                          -> Holey (StaticBaseDescriptor t) r (ToM BitStringBuilder (Proxy (StaticBaseDescriptorContent t)) r)
staticBaseDescriptorHoley _ =
  hoistM StaticBaseDescriptor (bitBoxHoley (Proxy :: Proxy (StaticBaseDescriptorContent t)))

-- | base descriptors that are known at compile time

type KnownDescriptor t = ( KnownNat (GetClassTag t)
                         , KnownNat (GetRecordSize (GetClassBody t))
                         , KnownNat (GetRecordSize (StaticBaseDescriptorContent t)))

type StaticBaseDescriptorContent t =
    Word8 := GetClassTag t :>: StaticExpandableContent (GetClassBody t)

newtype StaticBaseDescriptor t where
        StaticBaseDescriptor ::
          BitBox (StaticBaseDescriptorContent t) -> StaticBaseDescriptor t
    deriving Monoid

deriving instance (KnownDescriptor t) => IsBoxContent (StaticBaseDescriptor t)

-- * Base Descriptor Class Tags

-- | Family of class tags indexed by a content type
type family GetClassTag t :: Nat
type family GetClassBody t :: Type

data ObjectDescr -- TODO
type instance GetClassTag ObjectDescr = 1

data InitialObjectDescr -- TODO
type instance GetClassTag InitialObjectDescr = 0x02

data ES_Descr -- TODO
type instance GetClassTag ES_Descr = 0x03

data ES_DescrP a = ES_DescrP -- TODO
type instance GetClassTag (ES_DescrP a) = 0x03
type instance GetClassBody (ES_DescrP a) = a

data DecoderConfigDescr -- TODO
type instance GetClassTag DecoderConfigDescr = 0x04

data DecSpecificInfo -- TODO
type instance GetClassTag DecSpecificInfo = 0x05

data SLConfigDescr -- TODO
type instance GetClassTag SLConfigDescr = 0x06

data ContentIdentDescr -- TODO
type instance GetClassTag ContentIdentDescr = 0x07

data SupplContentIdentDescr -- TODO
type instance GetClassTag SupplContentIdentDescr = 0x08

data IPI_DescrPointer -- TODO
type instance GetClassTag IPI_DescrPointer = 0x09

data IPMP_DescrPointer -- TODO
type instance GetClassTag IPMP_DescrPointer = 0x0A

data IPMP_Descr -- TODO
type instance GetClassTag IPMP_Descr = 0x0B

data QoS_Descr -- TODO
type instance GetClassTag QoS_Descr = 0x0C

data RegistrationDescr -- TODO
type instance GetClassTag RegistrationDescr = 0x0D

data ES_ID_Inc -- TODO
type instance GetClassTag ES_ID_Inc = 0x0E

data ES_ID_Ref -- TODO
type instance GetClassTag ES_ID_Ref = 0x0F

data MP4_IOD_ -- TODO
type instance GetClassTag MP4_IOD_ = 0x10

data MP4_OD_ -- TODO
type instance GetClassTag MP4_OD_ = 0x11

data IPL_DescrPointerRef -- TODO
type instance GetClassTag IPL_DescrPointerRef = 0x12

data ExtensionProfileLevelDescr -- TODO
type instance GetClassTag ExtensionProfileLevelDescr = 0x13

data ProfileLevelIndicationIndexDescr -- TODO
type instance GetClassTag ProfileLevelIndicationIndexDescr = 0x14

data ContentClassificationDescr -- TODO
type instance GetClassTag ContentClassificationDescr = 0x40

data KeyWordDescr -- TODO
type instance GetClassTag KeyWordDescr = 0x41

data RatingDescr -- TODO
type instance GetClassTag RatingDescr = 0x42

data LanguageDescr -- TODO
type instance GetClassTag LanguageDescr = 0x43

data ShortTextualDescr -- TODO
type instance GetClassTag ShortTextualDescr = 0x44

data ExpandedTextualDescr -- TODO
type instance GetClassTag ExpandedTextualDescr = 0x45

data ContentCreatorNameDescr -- TODO
type instance GetClassTag ContentCreatorNameDescr = 0x46

data ContentCreationDateDescr -- TODO
type instance GetClassTag ContentCreationDateDescr = 0x47

data OCICreatorNameDescr -- TODO
type instance GetClassTag OCICreatorNameDescr = 0x48

data OCICreationDateDescr -- TODO
type instance GetClassTag OCICreationDateDescr = 0x49

data SmpteCameraPositionDescr -- TODO
type instance GetClassTag SmpteCameraPositionDescr = 0x4A

data SegmentDescr -- TODO
type instance GetClassTag SegmentDescr = 0x4B

data MediaTimeDescr -- TODO
type instance GetClassTag MediaTimeDescr = 0x4C

data IPMP_ToolsListDescr -- TODO
type instance GetClassTag IPMP_ToolsListDescr = 0x60

data IPMP_Tool -- TODO
type instance GetClassTag IPMP_Tool = 0x61

data M4MuxTimingDescr -- TODO
type instance GetClassTag M4MuxTimingDescr = 0x62

data M4MuxCodeTableDescr -- TODO
type instance GetClassTag M4MuxCodeTableDescr = 0x63

data ExtSLConfigDescr -- TODO
type instance GetClassTag ExtSLConfigDescr = 0x64

data M4MuxBufferSizeDescr -- TODO
type instance GetClassTag M4MuxBufferSizeDescr = 0x65

data M4MuxIdentDescr -- TODO
type instance GetClassTag M4MuxIdentDescr = 0x66

data DependencyPointer -- TODO
type instance GetClassTag DependencyPointer = 0x67

data DependencyMarker -- TODO
type instance GetClassTag DependencyMarker = 0x68

data M4MuxChannelDescr -- TODO
type instance GetClassTag M4MuxChannelDescr = 0x69


type ExtDescrTagStartRange = 0x6A
type ExtDescrTagEndRange = 0xFE
type OCIDescrTagStartRange = 0x40
type OCIDescrTagEndRange = 0x5F

-- * ObjectDescriptorBase
