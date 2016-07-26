-- | Media-independent properties of a tracks media content.
module Data.ByteString.IsoBaseFileFormat.Boxes.SpecificMediaHeader where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Boxes.BoxFields
import Data.ByteString.IsoBaseFileFormat.Boxes.FullBox

-- | Media header data box. The actual box type is determined by the claused
-- used to construct the record.
data SpecificMediaHeader where
  VideoMediaHeader
   :: Template (U16 "graphicsmode") 0
   :+ Template (U16 "reserved") 0
   -> SpecificMediaHeader
  SoundMediaHeader
   :: Template (I16 "balance") 0
   :+ Constant (U16Arr "opcolor" 3) '[0,0,0]
   -> SpecificMediaHeader
  HintMediaHeader
   :: U16 "maxPDUsize"
   :+ U16 "avgPDUsize"
   :+ U16 "maxbitrate"
   :+ U16 "avgbitrate"
   :+ U32 "reserved"
   -> SpecificMediaHeader
  NullMediaHeader
   :: SpecificMediaHeader

-- | Create a 'SpecificMediaHeader' box.
specificMediaHeader
  :: SpecificMediaHeader -> Box (FullBox 0 SpecificMediaHeader)
specificMediaHeader h@(VideoMediaHeader _) = fullBox 1 h
specificMediaHeader h                      = fullBox 0 h

instance IsBoxType SpecificMediaHeader where
  type BoxContent SpecificMediaHeader = SpecificMediaHeader
  toBoxType _ (VideoMediaHeader _) = StdType "vmhd"
  toBoxType _ (SoundMediaHeader _) = StdType "smhd"
  toBoxType _ (HintMediaHeader  _) = StdType "hmhd"
  toBoxType _ NullMediaHeader      = StdType "nmhd"

instance IsBoxContent SpecificMediaHeader where
  boxSize (VideoMediaHeader c) = boxSize c
  boxSize (SoundMediaHeader c) = boxSize c
  boxSize (HintMediaHeader  c) = boxSize c
  boxSize NullMediaHeader      = boxSize ()
  boxBuilder (VideoMediaHeader c) = boxBuilder c
  boxBuilder (SoundMediaHeader c) = boxBuilder c
  boxBuilder (HintMediaHeader  c) = boxBuilder c
  boxBuilder NullMediaHeader      = boxBuilder ()
