-- | Definition of the most basic elements in an ISOBMFF file /boxes/.
-- See Chapter 4 in the standard document. Since the standard
module Media.IsoBaseFileFormat.Blaze.Types.Box where

import Data.Word
import Data.Proxy

-- * Basic Types

-- | The standard (32 bit) box size
newtype CompactSize = CompactSize Word32 deriving (Num,Eq)

-- | The big 64 bit box size, mostly for media data
newtype LargeSize = LargeSize Word64 deriving (Num,Eq)

-- | This size indicates that a box has the size of the rest of the file.
data ExtendToEof = ExtendToEof deriving (Eq,Ord,Read,Show)

-- | 'ExtendToEof' must be a 'Num' too, so implement an instance that always
-- returns 'ExtendToEof'.
instance Num ExtendToEof where
  (+) _ _ = ExtendToEof
  (-) _ _ = ExtendToEof
  (*) _ _ = ExtendToEof
  abs _ = ExtendToEof
  signum _ = ExtendToEof
  fromInteger _ = ExtendToEof

-- | `FourCc` can be used as @boxType@ in `Box`, standard four letter character
-- code, e.g. @ftyp@
data FourCc =
  FourCc (Char,Char,Char,Char)

-- | CustomBoxType defines custom @boxType@s in `Box`es.
data CustomBoxType =
  CustomBoxType UUID

newtype UUID =
  UUID String -- TODO

-- | The box version (in a 'FullBox') is a single byte
newtype BoxVersion =
  BoxVersion Word8

-- | In addition to a 'BoxVersion' there can be 24 bits for custom flags etc in
-- a 'FullBox'.
newtype BoxFlags =
  BoxFlags (Word8,Word8,Word8)

-- * Basic /Boxes/

-- | Base /object/ of all boxes.

data BoxSizeTypes = CompactSizeX | LargeSizeX | RestOfFileX

data BoxSize (t :: BoxSizeTypes) where
  CompactSizeXX :: Word32 -> BoxSize CompactSizeX
  LargeSizeXX :: Word64 -> BoxSize LargeSizeX
  RestOfFileXX :: BoxSize RestOfFileX

data WhatBoxSizeType 

data BoxTypeX = FourCcX String | CustomUuidX String

data Box t

instance IsBox t => IsBox (Box t) where
  data BoxContent (Box t) = Box (BoxTypeType t) (BoxSize (BoxSizeType t)) (BoxContent t)
  type BoxTypeType (Box t) = BoxTypeType t
  type BoxSizeType (Box t) = BoxSizeType t
  boxType _ = boxType (Proxy :: Proxy t)
  boxSize (Box _ _ t) = error "TODO"

-- | A `Box` with /version/ and /branding/ information
data FullBox child

instance (IsBox child) => IsBox (FullBox child) where
  data BoxContent (FullBox child) = FullBoxContent BoxVersion BoxFlags (BoxContent child)
  type BoxTypeType (FullBox child) = BoxTypeType child
  type BoxSizeType (FullBox child) = BoxSizeType child
  boxType _ = boxType (Proxy :: Proxy child)
  boxSize (FullBoxContent _ _ child) = 4 + boxSize child

-- | The File Type Box /ftyp/. Mandatory box, must occure only once.
data FileTypeBox



class (Num (BoxSize (BoxSizeType a))) => IsBox a where
  data BoxContent a
  type BoxTypeType a
  type BoxSizeType a :: BoxSizeTypes
  type BoxSizeType a = 'CompactSizeX
  boxType :: p a -> BoxTypeType a
  boxSize :: BoxContent a -> BoxSize (BoxSizeType a)
