module MediaFileSpec
  (spec)
  where

import Test.Hspec
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.IsoBaseFileFormat.MediaFile
import Data.ByteString.Lazy (unpack)

spec :: Spec
spec =
  describe "mediaBuilder" $
  do describe "Empty BoxLayout" $
       it "accepts valid box content types" $
       let test =
             mediaBuilder (Proxy :: Proxy TestBrandEmpty)
                          NoBoxes
       in printBuilder test
     describe "Single Box BoxLayout" $
       it "accepts valid box content types" $
       let test =
             mediaBuilder (Proxy :: Proxy TestBrandSingle)
                          (singletonBox testBox1)
       in printBuilder test
     describe "Multiple nested Boxes BoxLayout" $
       it "accepts valid box content types" $
       let test =
             mediaBuilder (Proxy :: Proxy TestBrandNested)
                          (singletonBox (testParentBox1 $: testBox1))
       in printBuilder test

printBuilder :: Builder -> IO ()
printBuilder b =
  putStrLn $ unlines $ ("                     "++) <$> lines (show (unpack (toLazyByteString b)))

data TestBox1

instance IsBox TestBox1 where
  type BoxContent TestBox1 = ()

type instance BoxTypeSymbol TestBox1 = "tst1"

testBox1 :: Box TestBox1
testBox1 = Box ()

data TestParentBox1

instance IsBox TestParentBox1 where
  type BoxContent TestParentBox1 = ()

type instance BoxTypeSymbol TestParentBox1 = "par1"

testParentBox1
  :: Boxes ts -> Box (ContainerBox TestParentBox1 ts)
testParentBox1 = containerBox ()

data TestBrandEmpty

instance IsMediaFileFormat TestBrandEmpty where
  type BoxLayout TestBrandEmpty = Boxes '[]

data TestBrandSingle

instance IsMediaFileFormat TestBrandSingle where
  type BoxLayout TestBrandSingle = Boxes '[OM_ TestBox1]

data TestBrandNested

instance IsMediaFileFormat TestBrandNested where
  type BoxLayout TestBrandNested = Boxes '[OM TestParentBox1 '[OM_ TestBox1]]
