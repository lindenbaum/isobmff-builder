{-# LANGUAGE UndecidableInstances #-}
-- | Detailed visual sample description.
module Data.ByteString.IsoBaseFileFormat.Boxes.VisualSampleEntry where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.Handler
import Data.ByteString.IsoBaseFileFormat.Boxes.SampleEntry
import qualified Data.Text as T

-- | Construct a visual sample entry box
visualSampleEntry ::
     VideoCoding codec
  -> U16 "data_reference_index"
  -> SampleEntry 'VideoTrack (VideoCoding codec)
  -> Box (SampleEntry 'VideoTrack (VideoCoding codec))
visualSampleEntry _ = sampleEntry

-- | Fields if visual sample entries.
--  A @depth@ of 0x0018 means colour image with no alpha.
--  The @horizresolution@ and @vertresolution@ of 0x00480000 means 72 dpi.
--  The @frame_count@ indicates the number of video frames per sample.
newtype instance SampleEntry 'VideoTrack (VideoCoding c) where
    VideoSampleEntry
      :: U16 "pre_defined"
      :+ Constant (U16 "reserved") 0
      :+ U16 "width"
      :+ U16 "height"
      :+ Template (U32 "horizresolution") 0x00480000 -- TODO fix point
      :+ Template (U32 "vertresolution") 0x00480000 -- TODO fix point
      :+ Constant (U32 "reserved") 0
      :+ Template (U16 "frame_count") 1
      :+ FixSizeText 32 "compressorname"
      :+ Template (U16 "depth") 0x0018
      :+ Template (I16 "pre_defined") 65535
      :+ Maybe (Box CleanAperture)
      :+ Maybe (Box PixelAspectRatio)
      :+ [Box SomeColourInformation]
      -> SampleEntry 'VideoTrack (VideoCoding c)
    deriving (IsBoxContent, Default)

instance IsBoxContent [Box SomeColourInformation] where
  boxSize = sum . fmap boxSize
  boxBuilder = fold . fmap boxBuilder

-- | A coproduct of video codec types
data family VideoCoding (c :: Symbol)

-- | Simple default  MPEG-4 video
data instance VideoCoding "mp4v" = Mpeg4Avc

type instance BoxTypeSymbol (VideoCoding c) = c

-- * Clean Aperture sub box

-- | Construct a 'CleanAperture' (sub-) 'Box'
cleanAperture :: CleanAperture -> Box CleanAperture
cleanAperture = Box

-- | The clean aperture settings
newtype CleanAperture where
  CleanAperture
    :: U32 "cleanApertureWidthN"
    :+ U32 "cleanApertureWidthD"
    :+ U32 "cleanApertureHeightN"
    :+ U32 "cleanApertureHeightD"
    :+ U32 "horizOffN"
    :+ U32 "horizOffD"
    :+ U32 "vertOffN"
    :+ U32 "vertOffD"
    -> CleanAperture
    deriving (Default, IsBoxContent)

type instance BoxTypeSymbol CleanAperture = "clap"
instance IsBox CleanAperture

-- * Pixel aspect ratio sub box

-- | Construct a 'PixelAspectRatio' (sub-) 'Box'
pixelAspectRatio :: PixelAspectRatio -> Box PixelAspectRatio
pixelAspectRatio = Box

-- | The pixel aspect ratio.
newtype PixelAspectRatio where
  PixelAspectRatio
    :: U32 "hSpacing"
    :+ U32 "vSpacing"
    -> PixelAspectRatio
    deriving (Default, IsBoxContent)

type instance BoxTypeSymbol PixelAspectRatio = "pasp"
instance IsBox PixelAspectRatio

-- * Colour information sub box

-- | Construct a 'ColourInformation' (sub-) 'Box'
colourInformation
  :: ColourType p -> ColourInformation p
colourInformation = ColourInformation . (Constant :+)

-- | Construct a 'ColourInformation' (sub-) 'Box' from 'OnScreenColours'
onScreenColourInformation
  :: ColourType 'OnScreenColours -> ColourInformation 'OnScreenColours
onScreenColourInformation = colourInformation

-- | Construct a 'ColourInformation' (sub-) 'Box' from 'RestrictedICCProfile'
restrictedICCProfileColourInformation
  :: ColourType 'RestrictedICCProfile -> ColourInformation 'RestrictedICCProfile
restrictedICCProfileColourInformation = colourInformation

-- | Construct a 'ColourInformation' (sub-) 'Box' from 'UnrestrictedICCProfile'
unrestrictedICCProfileColourInformation
  :: ColourType 'UnrestrictedICCProfile -> ColourInformation 'UnrestrictedICCProfile
unrestrictedICCProfileColourInformation = colourInformation

type instance BoxTypeSymbol SomeColourInformation = "pasp"
instance IsBox SomeColourInformation

-- | A wrapper that hides the concrete 'ColourTypeProfile' of a
-- 'ColourInformation'.
data SomeColourInformation where
  SomeColourInformation
    :: forall (profile :: ColourTypeProfile)
    . IsBoxContent (ColourInformation profile)
    => !(ColourInformation profile)
    -> SomeColourInformation
instance IsBoxContent SomeColourInformation where
  boxSize (SomeColourInformation c) = boxSize c
  boxBuilder (SomeColourInformation c) = boxBuilder c

-- | Profile dependent colour information
newtype ColourInformation (profile :: ColourTypeProfile) where
  ColourInformation
    :: Constant (U32Text "colour_type") (ColourTypeCode profile)
    :+ ColourType profile
    -> ColourInformation profile

deriving instance
  (Default (ColourType profile))
    => Default (ColourInformation profile)
deriving instance
  (KnownSymbol (ColourTypeCode profile),IsBoxContent (ColourType profile))
    => IsBoxContent (ColourInformation profile)

-- | Colour type profiles
data ColourTypeProfile =
  -- | PTM_COLOR_INFO from A.7.2 of ISO/IEC 29199-2, mind the full range flag.
  OnScreenColours |
  -- | A restricted ICC.1 (2010) profile
  RestrictedICCProfile |
  -- | An unrestricted IEC ISO-15076 part 1, ICC.1 (2010) profile
  UnrestrictedICCProfile

-- | Profile dependent colour information family
type family
  ColourType (p :: ColourTypeProfile) where
  ColourType 'OnScreenColours =
       U16 "colour_primaries"
    :+ U16 "transfer_characteristics"
    :+ U16 "matrix_coefficients"
    :+ FullRangeFlag
  ColourType 'RestrictedICCProfile =
    T.Text -- TODO
  ColourType 'UnrestrictedICCProfile =
    T.Text -- TODO

-- | The full range flag, note the different bit layout compared to
-- PTM_COLOR_INFO in ISO 29199-2.
data FullRangeFlag = IsFullRange | IsNotFullRange
instance IsBoxContent FullRangeFlag where
  boxSize _ = 1
  boxBuilder IsFullRange = word8 128
  boxBuilder IsNotFullRange = word8 0
instance Default FullRangeFlag where
  def = IsFullRange

-- | Return the color type four letter code for a 'ColourTypeProfile'.
type family
  ColourTypeCode (p :: ColourTypeProfile) :: Symbol where
  ColourTypeCode 'OnScreenColours         = "nclx"
  ColourTypeCode 'RestrictedICCProfile    = "rICC"
  ColourTypeCode 'UnrestrictedICCProfile  = "prof"
