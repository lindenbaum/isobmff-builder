{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Builder.LazyByteStringBuilder where

import Data.Type.BitRecords.Builder.BitBuffer
import Data.Type.BitRecords.Builder.Holey
import Data.Monoid
import Control.Category
import Prelude hiding ((.), id)
import Data.ByteString.Builder

----------------
----------------
----------------

newtype BittrWriter = BittrWriter { unBittrWriter :: Endo BittrWriterState }
  deriving Monoid

runBittrWriter :: BittrWriter -> Builder
runBittrWriter !w =
  evalBittrWriterState $ appBittrWriter w initialBittrWriterState

appBittrWriter :: BittrWriter -> BittrWriterState -> BittrWriterState
appBittrWriter !w = appEndo (unBittrWriter w)

data BittrWriterState where
    BittrWriterState :: !Builder -> !BitOutBuffer -> BittrWriterState

initialBittrWriterState :: BittrWriterState
initialBittrWriterState = BittrWriterState mempty emptyBitOutBuffer

evalBittrWriterState :: BittrWriterState -> Builder
evalBittrWriterState (BittrWriterState !builder !buff) =
    flushedBuilder
  where
    !flushedBuilder = if isBitOutBufferEmpty buff
                      then builder
                      else error "TODO implement flush"

---

-- | Write all the bits, in chunks, filling and writing the 'BittrBuffer'
-- in the 'BittrWriterState' as often as necessary.
appendUnlimited :: BittrBufferUnlimited -> BittrWriter
appendUnlimited x' = BittrWriter $
    Endo $
        \(BittrWriterState !builder !buff) -> go x' builder buff
  where
    go !x !builder !buff
        | isBittrBufferUnlimitedEmpty x =
              BittrWriterState builder buff
        | otherwise = let (!rest, !buff') = bufferBitsInteger x buff
                      in
                          if bitOutBufferSpaceLeft buff' > 0
                          then BittrWriterState builder buff'
                          else let !nextBuilder = builder <>
                                       word64BE (unBitBuffer (bitOutBufferContent buff'))
                               in
                                   go rest nextBuilder emptyBitOutBuffer


-- | Write all the bits, in chunks, filling and writing the 'BittrBuffer'
-- in the 'BittrWriterState' as often as necessary.
appendBittrBuffer :: BittrBuffer -> BittrWriter
appendBittrBuffer x' = BittrWriter $
    Endo $
        \(BittrWriterState !builder !buff) -> go x' builder buff
  where
    go !x !builder !buff
        | bittrBufferLength x == 0 =
              BittrWriterState builder buff
        | otherwise = let (!rest, !buff') = bufferBits x buff
                      in
                          if bitOutBufferSpaceLeft buff' > 0
                          then BittrWriterState builder buff'
                          else let !nextBuilder = builder <>
                                       word64BE (unBitBuffer (bitOutBufferContent buff'))
                               in
                                   go rest nextBuilder emptyBitOutBuffer

runBittrWriterHoley :: Holey BittrWriter Builder a -> a
runBittrWriterHoley (HM !x) = x runBittrWriter

instance ToHoley BittrWriter BittrBufferUnlimited r where
  toHoley = immediate . appendUnlimited

instance ToHoley BittrWriter BittrBuffer r where
  toHoley = immediate . appendBittrBuffer
