module DataReferenceSpec (spec) where

import Test.Hspec
import Data.ByteString.IsoBaseFileFormat.Boxes
import Data.ByteString.IsoBaseFileFormat.Brands.Dash

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as Binary
import qualified Data.Text as T

spec :: Spec
spec =
  describe "IsBoxContent" $ do
    describe "LocalMediaEntry" $ do
      describe "boxSize" $ do
        it "returns 0" $
          let actual = boxSize (localMediaDataReference :: DRef)
              --         dref: size+fourcc+fullbox+entries -> 4 * 4bytes
              expected = 4 + 4 + 4 + 4 + (4 + 4 + boxSize ("xxxx" :+ FullBox Default 0 () :: DEntryLocal))
          in actual `shouldBe` expected

type DRef = Box (Dash 0) DataReference
type DEntryLocal = FourCc :+ FullBox 0 ()
type DEntryUrl = FourCc :+ FullBox 0 (T.Text)
type DEntryUrn = FourCc :+ FullBox 0 (T.Text :+ T.Text)
