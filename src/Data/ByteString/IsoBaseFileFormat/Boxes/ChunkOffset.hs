-- | A table mapping chunks to *absolute* file offsets.
-- Two variants exist: 32 or 64 bits.

module Data.ByteString.IsoBaseFileFormat.Boxes.ChunkOffset where

import Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import           Data.ByteString.IsoBaseFileFormat.Util.FullBox

-- | Create a hunk offset box for 32 bit entries (@stco@). If possible use
-- this over 'ChunkOffset64'
chunkOffset32 :: [StcoEntry32] -> Box ChunkOffset32
chunkOffset32 = fullBox 0 . ChunkOffsetTable . ListContent

-- | Create a hunk offset box for 64 bit entries (@stco@). If possible use
-- 'ChunkOffset32'
chunkOffset64 :: [StcoEntry64] -> Box ChunkOffset64
chunkOffset64 = fullBox 0 . ChunkOffsetTable . ListContent

-- | Chunk offset box for 32 bit entries (@stco@). If possible use
-- this over 'ChunkOffset64'
type ChunkOffset32 = FullBox ChunkOffsetTable32 0

-- | Chunk offset box for 64 bit entries (@stco@). If possible use
-- 'ChunkOffset32'
type ChunkOffset64 = FullBox ChunkOffsetTable64 0

-- | Alias for 'ChunkOffsetTable' for 32 bit entries (@stco@)
type ChunkOffsetTable32 = ChunkOffsetTable StcoEntry32

-- | Alias for 'ChunkOffsetTable' for 64 bit entries (@co64@)
type ChunkOffsetTable64 = ChunkOffsetTable StcoEntry64

-- | A list of 'StcoEntry32' or 'StcoEntry64' entries.
newtype ChunkOffsetTable stcoEntry =
  ChunkOffsetTable (ListContent (U32 "entry_count") stcoEntry)
  deriving (Default, IsBoxContent)

-- | An entry of the 'ChunkOffsetTable' contains just the absolute file offset
-- to the chunk.
type StcoEntry32 = U32 "chunk_offset"

-- | An entry of the 'ChunkOffsetTable' contains just the absolute file offset
-- to the chunk.
type StcoEntry64 = U64 "chunk_offset"

type instance BoxTypeSymbol ChunkOffsetTable32 = "stco"
type instance BoxTypeSymbol ChunkOffsetTable64 = "co64"

instance IsBox ChunkOffsetTable32
instance IsBox ChunkOffsetTable64
