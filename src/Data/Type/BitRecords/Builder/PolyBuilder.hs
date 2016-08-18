{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Builder.PolyBuilder where

import           Data.Type.BitRecords.Builder.BitBuffer
import           Data.Type.BitRecords.Builder.Holey
import           Data.Type.BitRecords.Builder.Poly
import           Data.Bits
import           Data.Monoid
import           Control.Category
import           Prelude hiding ((.), id)

----------------
----------------
----------------

newtype BittrWriter b = BittrWriter { unBittrWriter :: Endo (BittrWriterState b)
                                    }
    deriving Monoid

runBittrWriter :: Monoid b => BittrWriter b -> b
runBittrWriter !w = evalBittrWriterState $
    appBittrWriter w initialBittrWriterState

appBittrWriter :: BittrWriter b -> BittrWriterState b -> BittrWriterState b
appBittrWriter !w = appEndo (unBittrWriter w)

data BittrWriterState b where
        BittrWriterState :: !b -> !BittrBuffer -> BittrWriterState b

initialBittrWriterState :: Monoid b => BittrWriterState b
initialBittrWriterState =
    BittrWriterState mempty emptyBittrBuffer

evalBittrWriterState :: BittrWriterState b -> b
evalBittrWriterState (BittrWriterState !builder !buff) =
    flushedBuilder
  where
    !flushedBuilder = if isBittrBufferEmpty buff
                      then builder
                      else error "TODO implement flush"

-- | Write all the bits, in chunks, filling and writing the 'BittrBuffer'
-- in the 'BittrWriterState' as often as necessary.

appendUnlimited :: (Monoid wi, ToBitBufferBuilder wi)
                => BittrBufferUnlimited
                -> BittrWriter wi
appendUnlimited x' = BittrWriter $
    Endo $
        \(BittrWriterState !builder !buff) -> go x' builder buff
  where
    go !x !builder !buff
        | isBittrBufferUnlimitedEmpty x =
              BittrWriterState builder buff
        | otherwise = let (!rest, !buff') = bufferBitsInteger x buff
                      in
                          if bittrBufferSpaceLeft buff' > 0
                          then BittrWriterState builder buff'
                          else let !nextBuilder = builder <>
                                       toBitBufferBuilder (bittrBufferContent buff')
                               in
                                   go rest nextBuilder emptyBittrBuffer

-- | Write all the bits, in chunks, filling and writing the 'BittrBuffer'
-- in the 'BittrWriterState' as often as necessary.
appendBittrBuffer :: (Monoid wi, ToBitBufferBuilder wi)
                  => BittrBuffer
                  -> BittrWriter wi
appendBittrBuffer x' = BittrWriter $
    Endo $
        \(BittrWriterState !builder !buff) -> go x' builder buff
  where
    go !x !builder !buff
        | bittrBufferLength x == 0 =
              BittrWriterState builder buff
        | otherwise = let (!rest, !buff') = bufferBits x buff
                      in
                          if bittrBufferSpaceLeft buff' > 0
                          then BittrWriterState builder buff'
                          else let !nextBuilder = builder <>
                                       toBitBufferBuilder (bittrBufferContent buff')
                               in
                                   go rest nextBuilder emptyBittrBuffer

runBittrWriterHoley :: (Monoid wi) => Holey (BittrWriter wi) wi a -> a
runBittrWriterHoley (HM !x) =
    x runBittrWriter


instance (ToBitBufferBuilder wi, Monoid wi) =>
         ToHoley (BittrWriter wi) BittrBufferUnlimited r where
    toHoley = immediate . appendUnlimited

instance (ToBitBufferBuilder wi, Monoid wi) =>
         ToHoley (BittrWriter wi) BittrBuffer r where
    toHoley = immediate . appendBittrBuffer
