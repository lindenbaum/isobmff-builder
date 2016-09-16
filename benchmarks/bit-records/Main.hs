{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
module Main where

import           Prelude hiding ( (.), id )
import           Control.Category
import           Criterion.Main
import qualified Data.ByteString.Builder as L
import qualified Data.ByteString.Lazy as L
import           Data.Proxy
import           Data.Type.BitRecords
import           Data.Word
import           Data.Tagged
import           GHC.TypeLits ()
#ifdef FULLBENCHMARKS
import           Data.Type.Equality
#endif
import           Test.TypeSpecCrazy

#ifdef FULLBENCHMARKS

type Static64 =
      Field 3 := 2
  .+. Field 5 := 4
  .+. Field 9 := 333
  .+. Field 7 := 35
  .+. Field 30 := 458329
  .+. Field 2 := 1
  .+. Field 2 := 0
  .+. Field 2 := 1
  .+. Field 4 := 9


type Static64WithParams =
      Field 3 := 0
  .+. Field 5 := 0
  .+. Field 9 := 0
  .+. "x" @: Field 7
  .+. Field 30 := 0
  .+. "y" @: Field 2
  .+. Field 2 := 0
  .+. Field 2 := 0
  .+. Field 4 := 0

type Static128 = Field 64 := 3735928559 .+. Field 64 := 3405688830

type Static256 = Static64 :+: Static128 :+: Static64

type Static517 = Static256 :+: Static256 :+. Field 5 := 0

#else

type Static64 = 'BitRecordMember (Field 64 := 0)


type Static64WithParams =
      "x" @: Field 32
  .+. "y" @: Field 32

#endif

aboutStatic64 ::

  "Test Types Sizes"
  ########################

        It's "64 bit long: Static64" (ShouldBe 64 (BitRecordSize Static64))
     -* It's "64 bit long: Static64WithParams" (ShouldBe 64 (BitRecordSize Static64WithParams))
#ifdef FULLBENCHMARKS
     -* It's "128 bit long" (ShouldBeTrue ((BitRecordSize Static128) == 128))
     -* It's "256 bit long" (ShouldBeTrue ((BitRecordSize Static256) == 256))
     -* It's "517 bit long" (ShouldBeTrue ((BitRecordSize Static517) == 517))
#endif

aboutStatic64 =
  Valid

lumpUp :: Word64 -> L.Builder -> [Word8]
lumpUp m = L.unpack . L.toLazyByteString . mconcat . replicate (fromIntegral m)

static64 m = lumpUp m $
    runBitStringBuilderHoley $ bitStringBuilderHoley (Proxy :: Proxy Static64)

static64WithParam m = lumpUp m $
    runBitStringBuilderHoley (bitStringBuilderHoley (Proxy :: Proxy Static64WithParams))
                        (B m)
                        (B m)

#ifdef FULLBENCHMARKS

static128 m =
  lumpUp m $ runBitStringBuilderHoley $ bitStringBuilderHoley (Proxy :: Proxy Static128)

static256 m =
  lumpUp m $ runBitStringBuilderHoley $ bitStringBuilderHoley (Proxy :: Proxy Static256)

static517 m =
  lumpUp m $ runBitStringBuilderHoley $ bitStringBuilderHoley (Proxy :: Proxy Static517)

staticPlain512bitBaseline m =
  lumpUp m $ runBitStringBuilderHoley $ bitStringBuilderHoley
    (Proxy :: Proxy (
      Field 64 .+. Field 64 .+. Field 64 .+. Field 64 .+.
      Field 64 .+. Field 64 .+. Field 64 .+. Field 64
    ))


#endif

main = do
    print aboutStatic64
    defaultMain [ bgroup "ByteStringBuilder"
                         [ bgroup "64-bit"
                                  [ bench "1" $ nf static64 1
                                  , bench "100" $ nf static64 5
                                  , bench "1000" $ nf static64 1000
                                  ]
                         , bgroup "64-bit parameterized"
                                  [ bench "1" $
                                      nf static64WithParam 1
                                  , bench "100" $
                                      nf static64WithParam 5
                                  , bench "1000" $
                                      nf static64WithParam 1000
                                  ]
#ifdef FULLBENCHMARKS
                         , bgroup "128-bit"
                                  [ bench "1" $
                                      nf static128 1
                                  , bench "100" $
                                      nf static128 5
                                  , bench "1000" $
                                      nf static128 1000
                                  ]
                         , bgroup "256-bit"
                                  [ bench "1" $
                                      nf static256 1
                                  , bench "100" $
                                      nf static256 5
                                  , bench "1000" $
                                      nf static256 1000
                                  ]
                         , bgroup "517-bit"
                                  [ bench "1" $
                                      nf static517 1
                                  , bench "100" $
                                      nf static517 5
                                  , bench "1000" $
                                      nf static517 1000
                                  ]
                         , bgroup "512-bit baseline"
                                  [ bench "1" $
                                      nf staticPlain512bitBaseline 1
                                  , bench "100" $
                                      nf staticPlain512bitBaseline 100
                                  , bench "1000" $
                                      nf staticPlain512bitBaseline 1000
                                  ]
#endif
                         , bgroup "BitString Word64 direct"
                                  [ bench "1" $
                                      nf bitStringWord64Direct 1
                                  , bench "100" $
                                      nf bitStringWord64Direct 5
                                  , bench "1000" $
                                      nf bitStringWord64Direct 1000
                                  ]
                         , bgroup "BitString Word64 holey"
                                  [ bench "1" $
                                      nf bitStringWord64Holey 1
                                  , bench "100" $
                                      nf bitStringWord64Holey 5
                                  , bench "1000" $
                                      nf bitStringWord64Holey 1000
                                  ]
                         ]
                ]

bitStringWord64Direct m =
  lumpUp 1
    $ runBitStringBuilder
    $ mconcat
    $ replicate m
    $ appendBitString
    $ bitString 64 0x01020304050607

bitStringWord64Holey m =
  lumpUp 1
    $ runBitStringBuilderHoley
    $ mconcat
    $ replicate m
    $ bitStringBuilderHoley
    $ bitString 64 0x01020304050607
