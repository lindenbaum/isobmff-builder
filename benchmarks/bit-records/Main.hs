{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
module Main where

import Prelude hiding ((.), id)
import Control.Category
import           Criterion.Main
import qualified Data.ByteString.Builder as L
import qualified Data.ByteString.Lazy as L
import           Data.Proxy
import           Data.Type.BitRecords
import qualified Data.Type.BitRecords.Builder.StaticLazyByteStringBuilder as SB
import qualified Data.Type.BitRecords.Builder.LazyByteStringBuilder as B
import           Data.Word
import Data.Tagged
import           GHC.TypeLits ()
import           Test.TypeSpecCrazy

#ifdef FULLBENCHMARKS

type Static64 =
      Field 3 := 2
  :>: Field 5 := 4
  :>: Field 9 := 333
  :>: Field 7 := 35
  :>: Field 30 := 458329
  :>: Field 2 := 1
  :>: Field 2 := 0
  :>: Field 2 := 1
  :>: Field 4 := 9


type Static64WithParams =
      Field 3 := 0
  :>: Field 5 := 0
  :>: Field 9 := 0
  :>: "x" :=> Field 7
  :>: Field 30 := 0
  :>: "y" :=> Field 2
  :>: Field 2 := 0
  :>: Field 2 := 0
  :>: Field 4 := 0

type Static128 = Field 128 := 0xdeadbeef

type Static256 =
  Static64 :>: Static128 :>: Static64

type Static517 =
   Static256 :>: Static256 :>: Field 5 := 0

#else

type Static64 = Field 64


type Static64WithParams =
      "x" :=> Field 32
  :>: "y" :=> Field 32

#endif

aboutStatic64 ::

  "Test Types Sizes"
  ########################

        It's "64 bit long: Static64" (ShouldBe 64 (GetRecordSize Static64))
     -* It's "64 bit long: Static64WithParams" (ShouldBe 64 (GetRecordSize Static64WithParams))
#ifdef FULLBENCHMARKS
     -* It's "128 bit long" (ShouldBeTrue ((GetRecordSize Static128) == 128))
     -* It's "256 bit long" (ShouldBeTrue ((GetRecordSize Static256) == 256))
     -* It's "517 bit long" (ShouldBeTrue ((GetRecordSize Static517) == 517))
#endif

aboutStatic64 =
  Valid

lumpUp :: Word64 -> L.Builder -> [Word8]
lumpUp m = L.unpack . L.toLazyByteString . mconcat . replicate (fromIntegral m)

static64SB m = lumpUp m $
    SB.runBittrWriterHoley $ toHoley (Proxy :: Proxy Static64)

static64WithParamSB m = lumpUp m $
    SB.runBittrWriterHoley (toHoley (Proxy :: Proxy Static64WithParams))
                           (Tagged m)
                           (Tagged m)

static64B m = lumpUp m $
    B.runBittrWriterHoley $ toHoley (Proxy :: Proxy Static64)

static64WithParamB m = lumpUp m $
    B.runBittrWriterHoley (toHoley (Proxy :: Proxy Static64WithParams))
                          (Tagged m)
                          (Tagged m)

#ifdef FULLBENCHMARKS

static128SB m =
  lumpUp m $ SB.runBittrWriterHoley $ toHoley (Proxy :: Proxy Static128)

static256SB m =
  lumpUp m $ SB.runBittrWriterHoley $ toHoley (Proxy :: Proxy Static256)

static517SB m =
  lumpUp m $ SB.runBittrWriterHoley $ toHoley (Proxy :: Proxy Static517)

staticPlain512bitBaselineSB m =
  lumpUp m $ SB.runBittrWriterHoley $ toHoley
    (Proxy :: Proxy (
      Field 64 :>: Field 64 :>: Field 64 :>: Field 64 :>:
      Field 64 :>: Field 64 :>: Field 64 :>: Field 64
    ))

static128B m =
  lumpUp m $ B.runBittrWriterHoley $ toHoley (Proxy :: Proxy Static128)

static256B m =
  lumpUp m $ B.runBittrWriterHoley $ toHoley (Proxy :: Proxy Static256)

static517B m =
  lumpUp m $ B.runBittrWriterHoley $ toHoley (Proxy :: Proxy Static517)

staticPlain512bitBaselineB m =
  lumpUp m $ B.runBittrWriterHoley $ toHoley
    (Proxy :: Proxy (
      Field 64 :>: Field 64 :>: Field 64 :>: Field 64 :>:
      Field 64 :>: Field 64 :>: Field 64 :>: Field 64
    ))
#endif

main = do
    print aboutStatic64
    defaultMain [ bgroup "Builder"
                         [ bgroup "Static"
                                  [ bgroup "ByteStringBuilder"
                                           [ bgroup "64-bit"
                                                    [ bench "1" $ nf static64SB 1
                                                    , bench "5" $ nf static64SB 5
                                                    , bench "1000" $
                                                        nf static64SB 1000
                                                    ]
                                           , bgroup "64-bit parameterized"
                                                    [ bench "1" $
                                                        nf static64WithParamSB 1
                                                    , bench "5" $
                                                        nf static64WithParamSB 5
                                                    , bench "1000" $
                                                        nf static64WithParamSB 1000
                                                    ]
#ifdef FULLBENCHMARKS
                                           , bgroup "128-bit"
                                                    [ bench "1" $
                                                        nf static128SB 1
                                                    , bench "5" $
                                                        nf static128SB 5
                                                    , bench "100" $
                                                        nf static128SB 100
                                                    ]
                                           , bgroup "256-bit"
                                                    [ bench "1" $
                                                        nf static256SB 1
                                                    , bench "5" $
                                                        nf static256SB 5
                                                    , bench "100" $
                                                        nf static256SB 100
                                                    ]
                                           , bgroup "517-bit"
                                                    [ bench "1" $
                                                        nf static517SB 1
                                                    , bench "5" $
                                                        nf static517SB 5
                                                    , bench "100" $
                                                        nf static517SB 100
                                                    ]
                                           , bgroup "512-bit baseline"
                                                    [ bench "1" $
                                                        nf staticPlain512bitBaselineSB
                                                           1
                                                    , bench "5" $
                                                        nf staticPlain512bitBaselineSB
                                                           5
                                                    , bench "100" $
                                                        nf staticPlain512bitBaselineSB
                                                           100
                                                    ]
#endif
                                           ]
                                  ]
                         ]
                , bgroup "Runtime"
                         [ bgroup "ByteStringBuilder"
                                           [ bgroup "64-bit"
                                                    [ bench "1" $ nf static64B 1
                                                    , bench "5" $ nf static64B 5
                                                    , bench "1000" $ nf static64B 1000
                                                    ]
                                           , bgroup "64-bit parameterized"
                                                    [ bench "1" $
                                                        nf static64WithParamB 1
                                                    , bench "5" $
                                                        nf static64WithParamB 5
                                                    , bench "1000" $
                                                        nf static64WithParamB 1000
                                                    ]
#ifdef FULLBENCHMARKS
                                           , bgroup "128-bit"
                                                    [ bench "1" $
                                                        nf static128B 1
                                                    , bench "5" $
                                                        nf static128B 5
                                                    , bench "100" $
                                                        nf static128B 100
                                                    ]
                                           , bgroup "256-bit"
                                                    [ bench "1" $
                                                        nf static256B 1
                                                    , bench "5" $
                                                        nf static256B 5
                                                    , bench "100" $
                                                        nf static256B 100
                                                    ]
                                           , bgroup "517-bit"
                                                    [ bench "1" $
                                                        nf static517B 1
                                                    , bench "5" $
                                                        nf static517B 5
                                                    , bench "100" $
                                                        nf static517B 100
                                                    ]
                                           , bgroup "512-bit baseline"
                                                    [ bench "1" $
                                                        nf staticPlain512bitBaselineB
                                                           1
                                                    , bench "5" $
                                                        nf staticPlain512bitBaselineB
                                                           5
                                                    , bench "100" $
                                                        nf staticPlain512bitBaselineB
                                                           100
                                                    ]
#endif
                                  , bgroup "BittrBufferUnlimited-direct"
                                           [ bench "1" $
                                               nf bittrBufferUnlimitedDirectB 1
                                           , bench "5" $
                                               nf bittrBufferUnlimitedDirectB 5
                                           , bench "1000" $
                                               nf bittrBufferUnlimitedDirectB
                                                  1000
                                           ]
                                  , bgroup "BittrBufferWord64-direct"
                                           [ bench "1" $
                                               nf bittrBufferWord64DirectB 1
                                           , bench "5" $
                                               nf bittrBufferWord64DirectB 5
                                           , bench "1000" $
                                               nf bittrBufferWord64DirectB 1000
                                           ]
                                  , bgroup "BittrBufferWord64-holey"
                                           [ bench "1" $
                                               nf bittrBufferWord64HoleyB 1
                                           , bench "5" $
                                               nf bittrBufferWord64HoleyB 5
                                           , bench "1000" $
                                               nf bittrBufferWord64HoleyB 1000
                                           ]
                                  ]
                         ]
                ]

bittrBufferUnlimitedDirectB m =
  lumpUp 1
    $ B.runBittrWriter
    $ B.appendUnlimited
    $ bittrBufferUnlimited 0x01020304050607 (64 * m)

bittrBufferWord64DirectB m =
  lumpUp 1
    $ B.runBittrWriter
    $ mconcat
    $ replicate m
    $ B.appendBittrBuffer
    $ bittrBuffer 0x01020304050607 64

bittrBufferWord64HoleyB m =
  lumpUp 1
    $ B.runBittrWriterHoley
    $ mconcat
    $ replicate m
    $ toHoley
    $ bittrBuffer 0x01020304050607 64
