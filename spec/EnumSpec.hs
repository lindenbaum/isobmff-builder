{-# LANGUAGE UndecidableInstances #-}
module EnumSpec (spec) where

import Data.Proxy
import Data.Type.BitRecords
import Data.Type.Equality ()
import Data.Kind.Extra
import Prelude hiding ((.), id)
import Test.Hspec

spec = do
  describe "ToPretty" $ do
    it "renders as a record using showRecord" $
      showRecord (Proxy @(Eval (BitRecordOfEnum (EnumParam "test" TestEnumExt))))
      `shouldBe` "test: <<enum>>(2)"
  describe "BitStringBuilder" $ do
    it "produces binary output" $
     bitStringPrinter (Proxy  @(Eval
                                (BitRecordOfEnum (EnumParam "test" TestEnumExt))))
      (MkEnumValue (Proxy @'A))
     `shouldBe` "<< 40 >>"

type TestEnumExt = ExtEnum TestEnum 2 'Be FieldU16


data TestEnum =
  A | Be | C

type instance FromEnum TestEnum 'A = 1
type instance FromEnum TestEnum 'Be = 2
type instance FromEnum TestEnum 'C = 4
