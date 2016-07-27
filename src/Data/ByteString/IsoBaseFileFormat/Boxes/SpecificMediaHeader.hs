-- | Media-independent properties of a tracks media content.
module Data.ByteString.IsoBaseFileFormat.Boxes.SpecificMediaHeader where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox
import Data.Default

-- * Videos

-- | Video header data box.
newtype VideoMediaHeader where
  VideoMediaHeader
   :: Template (U16 "graphicsmode") 0
   :+ Template (U16 "reserved") 0
   -> VideoMediaHeader
   deriving (Default, IsBoxContent)

-- | Create a video media header data box.
videoMediaHeader :: VideoMediaHeader -> Box (FullBox VideoMediaHeader 0)
videoMediaHeader = fullBox 1

instance IsBox VideoMediaHeader
type instance BoxTypeSymbol VideoMediaHeader = "vmhd"

-- * Sounds

-- | Sound header data box.
newtype SoundMediaHeader where
  SoundMediaHeader
   :: Template (I16 "balance") 0
   :+ Constant (U16Arr "opcolor" 3) '[0,0,0]
   -> SoundMediaHeader
   deriving (Default, IsBoxContent)

-- | Create a sound media header data box.
soundMediaHeader :: SoundMediaHeader -> Box (FullBox SoundMediaHeader 0)
soundMediaHeader = fullBox 0

instance IsBox SoundMediaHeader
type instance BoxTypeSymbol SoundMediaHeader = "smhd"

-- * Hints

-- | Hint data box.
newtype HintMediaHeader where
  HintMediaHeader
   :: U16 "maxPDUsize"
   :+ U16 "avgPDUsize"
   :+ U16 "maxbitrate"
   :+ U16 "avgbitrate"
   :+ U32 "reserved"
   -> HintMediaHeader
   deriving (Default, IsBoxContent)

-- | Create a hint media header data box.
hintMediaHeader :: HintMediaHeader -> Box (FullBox HintMediaHeader 0)
hintMediaHeader = fullBox 0

instance IsBox HintMediaHeader
type instance BoxTypeSymbol HintMediaHeader = "hmhd"

-- * Dummy/Null media

-- | Null header data box.
data NullMediaHeader = NullMediaHeader

-- | Create a null media header data box.
nullMediaHeader :: Box (FullBox NullMediaHeader 0)
nullMediaHeader = fullBox 0 NullMediaHeader

instance IsBox NullMediaHeader
type instance BoxTypeSymbol NullMediaHeader = "nmhd"

instance Default NullMediaHeader where
  def = NullMediaHeader
instance IsBoxContent NullMediaHeader where
  boxSize _ = 0
  boxBuilder _ = mempty
