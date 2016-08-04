{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module TypeLayoutSpec (spec) where

import Test.Hspec
import GHC.TypeLits ()
import Data.ByteString.IsoBaseFileFormat.Util.TypeLayout
import Data.ByteString.IsoBaseFileFormat.Box

spec :: Spec
spec =
  describe "IsRuleConform" $ do
    describe "TopLevel" $ do
      it "validates TopLevel Boxes" $ test1 `shouldBe` ()
    describe "ContainerBoxes" $ do
      describe "OnceOptional" $ do
        it "validates empty containters" $ test2a `shouldBe` ()
        it "validates a singleton container" $ test2b `shouldBe` ()
      describe "SomeOptional" $ do
        it "validates empty containters" $ test3a `shouldBe` ()
        it "validates a singleton container" $ test3b `shouldBe` ()
        it "validates a multi-element container" $ test3c `shouldBe` ()
      describe "(implied) mandatory" $ do
        it "validates a singleton container" $ test4 `shouldBe` ()
      describe "SomeMandatory" $ do
        it "validates a singleton container" $ test5a `shouldBe` ()
        it "validates a multi-element container" $ test5b `shouldBe` ()
      it "validates a Mix of SomeMandatory, OnceOptional, SomeOptional" $ do
        test6a `shouldBe` ()
        test6b `shouldBe` ()
        test6c `shouldBe` ()
        test6d `shouldBe` ()
      it "validates deeply nested container" $ do
        test7a `shouldBe` ()
        test7b `shouldBe` ()
        test7c `shouldBe` ()

----
data Foo
type instance ToSymbol Foo = "foo "
data Fov
type instance ToSymbol Fov = "fov "
data Bar
type instance ToSymbol Bar = "bar "
data Baz
type instance ToSymbol Baz = "baz "
----
type TestRule1 = TopLevel (MatchSymbol "foo ")
type TestType1 = Foo
test1 :: (IsRuleConform TestType1 TestRule1 ~ 'True) => ()
test1 = ()
----
type TestRule2 = TopLevel (ContainerBox Foo '[OnceOptionalX (MatchSymbol "bar ")])
type TestType2a = Box (ContainerBox Foo '[])
test2a :: (IsRuleConform TestType2a TestRule2 ~ 'True) => ()
test2a = ()
--
type TestType2b = Box (ContainerBox Foo '[Bar])
test2b :: (IsRuleConform TestType2b TestRule2 ~ 'True) => ()
test2b = ()
----
type TestRule3 = TopLevel (ContainerBox Foo '[SomeOptionalX (MatchSymbol "bar ")])
type TestType3a = Box (ContainerBox Foo '[])
test3a :: (IsRuleConform TestType3a TestRule3 ~ 'True) => ()
test3a = ()
--
type TestType3b = Box (ContainerBox Foo '[Bar])
test3b :: (IsRuleConform TestType3b TestRule3 ~ 'True) => ()
test3b = ()
--
type TestType3c = Box (ContainerBox Foo '[Bar,Bar])
test3c :: (IsRuleConform TestType3c TestRule3 ~ 'True) => ()
test3c = ()
----
type TestRule4 = TopLevel (ContainerBox Foo '[MatchSymbol "bar "])
type TestType4 = Box (ContainerBox Foo '[Bar])
test4 :: (IsRuleConform TestType4 TestRule4 ~ 'True) => ()
test4 = ()
----
type TestRule5 = TopLevel (ContainerBox Foo '[SomeMandatoryX (MatchSymbol "bar ")])
type TestType5a = Box (ContainerBox Foo '[Bar])
test5a :: (IsRuleConform TestType5a TestRule5 ~ 'True) => ()
test5a = ()
type TestType5b = Box (ContainerBox Foo '[Bar, Bar])
test5b :: (IsRuleConform TestType5b TestRule5 ~ 'True) => ()
test5b = ()
----
type TestRule6 =
           (ContainerBox Foo
           '[ OnceOptionalX (MatchSymbol "baz ")
            , SomeMandatoryX (MatchSymbol "bar ")
            , SomeOptionalX (MatchSymbol "fov ")
            , MatchSymbol "foo "])
type TestType6a =
  Box (ContainerBox Foo '[Baz,Bar,Bar,Bar,Fov,Fov,Foo])
test6a :: (IsRuleConform TestType6a TestRule6 ~ 'True) => ()
test6a = ()
type TestType6b =
  Box (ContainerBox Foo '[Bar,Bar,Bar,Fov,Fov,Foo])
test6b :: (IsRuleConform TestType6b TestRule6 ~ 'True) => ()
test6b = ()
type TestType6c =
  Box (ContainerBox Foo '[Bar,Fov,Fov,Foo])
test6c :: (IsRuleConform TestType6c TestRule6 ~ 'True) => ()
test6c = ()
type TestType6d = Box (ContainerBox Foo '[Bar,Foo])
test6d :: (IsRuleConform TestType6d TestRule6 ~ 'True) => ()
test6d = ()
----
type TestRule7 =
  TopLevel (ContainerBox Foo
           '[ SomeOptionalX (ContainerBox Fov
              '[ OnceOptionalX (ContainerBox Baz
                                '[MatchSymbol "foo "])
               , SomeMandatoryX (MatchSymbol "bar ")
               ])])
type TestType7a = Box (ContainerBox Foo '[ ])
test7a :: (IsRuleConform TestType7a TestRule7 ~ 'True) => ()
test7a = ()
type TestType7b = Box (ContainerBox Foo
                      '[ Box (ContainerBox Fov
                              '[Box (ContainerBox Baz
                               '[Foo])
                             , Bar])])
test7b :: (IsRuleConform TestType7b TestRule7 ~ 'True) => ()
test7b = ()
type TestType7c = Box (ContainerBox Foo
                      '[ Box (ContainerBox Fov
                           '[ Box (ContainerBox Baz
                               '[Foo])
                            , Bar
                            , Bar
                            , Bar
                            ])
                       , Box (ContainerBox Fov
                            '[ Box (ContainerBox Baz
                                '[Foo])
                             , Bar
                             , Bar
                             , Bar
                             ])
                       ])
test7c :: (IsRuleConform TestType7c TestRule7 ~ 'True) => ()
test7c = ()
