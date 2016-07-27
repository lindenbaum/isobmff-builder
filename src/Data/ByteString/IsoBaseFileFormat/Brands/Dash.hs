{-# LANGUAGE UndecidableInstances #-}
-- | Predefined Box composition matching the @dash@ brand. TODO this is an
-- incomplete,  special-purpose variant of this brand, serving my personal,
-- educational, current need.
-- This is a convenient way of building documents of that kind.
module Data.ByteString.IsoBaseFileFormat.Brands.Dash
       (Dash, SingleTrackInit(..), mkSingleTrackInit, module X) where

import Data.ByteString.IsoBaseFileFormat.Boxes as X hiding (All)
import Data.Kind (Type, Constraint)
import Control.Lens
import Data.Promotion.Prelude.List
import Data.Singletons

-- | A phantom type to indicate this branding. Version can be 0 or 1 it is used
-- in some boxes to switch between 32/64 bits.
data Dash (version :: Nat)

-- | A 'BoxLayout' which contains the stuff needed for the 'dash' brand.
-- TODO incomplete
instance KnownNat v => IsBrand (Dash v) where
  type BoxLayout (Dash v) =
    '[ OM_ FileType
     , OM  Movie
          '[ OM_ (MovieHeader v)
           , SM  Track
                '[ OM_ (TrackHeader v)
                 , OM  Media
                      '[ OM_ (MediaHeader v)
                       , OM_ Handler
                       , OM  MediaInformation
                            '[ OO_ SpecificMediaHeader
                             , OM  DataInformation
                                  '[ OM_ DataReference ]
                      --        , OM  (SampleTable v)               -- TODO
                      --             '[ OM_ (SampleDescriptions v)
                      --              , OM_ (TimeToSample v)
                      --              , OM_ (SampleToChunk v)
                      --              , OO_ (SampleSizes v)
                      --              , OM_ (SampleChunkOffset v)
                      --              ]
                              ]
                      ]
                 ]
           ]
     , SO_ Skip
     ]

-- Missing Boxes
-- START 17:47:
--  mdia
--  mdhd
--  hdlr
--  minf
--  smhd
--  dinf
--  dref
--  ??url
--  stbl
--  stsd
--  stts
--  stsc
--  stsz
--  stco
--  soun
--  mp4a
--  esds
--  mvex
--  trex
-- For media
-- styp
-- moof
-- mfhd
-- traf
-- tfhd
-- trun
-- | A record which contains the stuff needed for a single track initialization
-- document according to the 'Dash' brand. TODO incomplete
data SingleTrackInit =
  SingleTrackInit {_mvhd :: MovieHeader 0
                  ,_tkhd :: TrackHeader 0
                  ,_mdhd :: MediaHeader 0
                  ,_hdlr :: Handler
                  ,_xmhd :: SpecificMediaHeader}

makeLenses ''SingleTrackInit

-- | Convert a 'SingleTrackInit' record to a generic 'Boxes' collection.
mkSingleTrackInit
  :: SingleTrackInit -> MediaFile (Dash 0)
mkSingleTrackInit doc = undefined
  -- MediaFile $
  -- fileTypeBox (FileType "iso5" 0 ["isom","iso5","dash","mp42"]) :|
  -- movie (movieHeader (doc ^. mvhd) :|
  --        track (trackHeader (doc ^. tkhd) :|
  --               media (mediaHeader (doc ^. mdhd) :. handler (doc ^. hdlr) :|
  --                      mediaInformation (
  --                        specificMediaHeader (doc ^. xmhd)
  --                      :| dataInformation
  --                           $: dataReference ))))


----
type family
  ReportIt (es :: Maybe ErrorMessage) :: Constraint where
    ReportIt 'Nothing = ()
    ReportIt ('Just e) = TypeError e
----
data (payload :: payloadK) <@ (info :: infoK)
type Dress payload info = payload <@ info
infixr 4 <@
type family
  Naked w
  where
    Naked (payload <@ info) = payload
    Naked payload           = payload
----
type family IsRuleConform (b :: k) (r :: l) :: Bool
data IsRuleConform0 :: k ~> l ~> Bool
type instance Apply IsRuleConform0 ts = IsRuleConform1 ts
data IsRuleConform1 :: k -> l ~> Bool
type instance Apply (IsRuleConform1 ts) rule = IsRuleConform ts rule
----
data TopLevel  :: Type -> Type
type instance IsRuleConform t (TopLevel rule) = IsRuleConform t rule
----
type instance IsRuleConform b (Box fourcc) = IsBox b && ToFourCc b == fourcc
type family IsBox t :: Bool where
  IsBox (Box a) = 'True
  IsBox b = 'False
type family ToFourCc t :: Symbol
type instance ToFourCc (Box t) = ToFourCc t
type instance ToFourCc (ContainerBox t ts) = ToFourCc t
data ToFourCc0 :: Type ~> Symbol
type instance Apply ToFourCc0 t = ToFourCc t
----
type family IsContainerBox t :: Bool where
  IsContainerBox (ContainerBox a as) = 'True
  IsContainerBox b = 'False
type family ContainerBoxChildren c :: [Type] where
  ContainerBoxChildren (ContainerBox a as) = as
type instance IsRuleConform b (ContainerBox fourcc ts)
  = IsContainerBox b
    && ToFourCc b == fourcc
    && IsContainerBoxConform (ContainerBoxChildren b) ts
----
data OnceOptionalX t
data SomeOptionalX t
data SomeMandatoryX t
----
type family
  IsContainerBoxConform (bs :: [k]) (rs :: [j]) :: Bool
  where
   IsContainerBoxConform '[]       '[]                      = 'True
   IsContainerBoxConform (b ': bs) '[]                      = 'False
   --
   IsContainerBoxConform '[]       (OnceOptionalX r ': rs)  = IsContainerBoxConform '[] rs
   IsContainerBoxConform (b ': bs) (OnceOptionalX r ': rs)  =
     If (IsRuleConform b r)
        (IsContainerBoxConform bs        rs)
        (IsContainerBoxConform (b ': bs) rs)
   --
   IsContainerBoxConform '[]       (SomeOptionalX r ': rs)  = IsContainerBoxConform '[] rs
   IsContainerBoxConform (b ': bs) (SomeOptionalX r ': rs)  =
     If (IsRuleConform b r)
        (IsContainerBoxConform bs        (SomeOptionalX r ': rs))
        (IsContainerBoxConform (b ': bs) rs                     )
   --
   IsContainerBoxConform '[]       (SomeMandatoryX r ': rs)  = 'False
   IsContainerBoxConform (b ': bs) (SomeMandatoryX r ': rs)  =
     IsRuleConform b r && IsContainerBoxConform  bs (SomeOptionalX r ': rs)
   --
   IsContainerBoxConform '[]       (r ': rs)  = 'False
   IsContainerBoxConform (b ': bs) (r ': rs)  =
     IsRuleConform b r && IsContainerBoxConform bs rs

----
----
data Foo
type instance ToFourCc Foo = "foo "
data Fov
type instance ToFourCc Fov = "fov "
data Bar
type instance ToFourCc Bar = "bar "
data Baz
type instance ToFourCc Baz = "baz "
----
type TestRule1 = TopLevel (Box "foo ")
type TestType1 = Box Foo
test1 :: (IsRuleConform TestType1 TestRule1 ~ 'True) => ()
test1 = ()
----
type TestRule2 = TopLevel (ContainerBox "foo " '[OnceOptionalX (Box "bar ")])
type TestType2a = ContainerBox Foo '[]
test2a :: (IsRuleConform TestType2a TestRule2 ~ 'True) => ()
test2a = ()
--
type TestType2b = ContainerBox Foo '[Box Bar]
test2b :: (IsRuleConform TestType2b TestRule2 ~ 'True) => ()
test2b = ()
----
type TestRule3 = TopLevel (ContainerBox "foo " '[SomeOptionalX (Box "bar ")])
type TestType3a = ContainerBox Foo '[]
test3a :: (IsRuleConform TestType3a TestRule3 ~ 'True) => ()
test3a = ()
--
type TestType3b = ContainerBox Foo '[Box Bar]
test3b :: (IsRuleConform TestType3b TestRule3 ~ 'True) => ()
test3b = ()
--
type TestType3c = ContainerBox Foo '[Box Bar, Box Bar]
test3c :: (IsRuleConform TestType3c TestRule3 ~ 'True) => ()
test3c = ()
----
type TestRule4 = TopLevel (ContainerBox "foo " '[Box "bar "])
type TestType4 = ContainerBox Foo '[Box Bar]
test4 :: (IsRuleConform TestType4 TestRule4 ~ 'True) => ()
test4 = ()
----
type TestRule5 = TopLevel (ContainerBox "foo " '[SomeMandatoryX (Box "bar ")])
type TestType5 = ContainerBox Foo '[Box Bar,Box Bar]
test5 :: (IsRuleConform TestType5 TestRule5 ~ 'True) => ()
test5 = ()
type TestType5b = ContainerBox Foo '[Box Bar]
test5b :: (IsRuleConform TestType5b TestRule5 ~ 'True) => ()
test5b = ()
----
type TestRule6 =
  TopLevel (ContainerBox "foo "
           '[ OnceOptionalX (Box "baz ")
            , SomeMandatoryX (Box "bar ")
            , SomeOptionalX (Box "fov ")
            , Box "foo "])
type TestType6a =
  ContainerBox Foo '[Box Baz,Box Bar,Box Bar,Box Bar,Box Fov,Box Fov,Box Foo]
test6a :: (IsRuleConform TestType6a TestRule6 ~ 'True) => ()
test6a = ()
type TestType6b =
  ContainerBox Foo '[Box Bar,Box Bar,Box Bar,Box Fov,Box Fov,Box Foo]
test6b :: (IsRuleConform TestType6b TestRule6 ~ 'True) => ()
test6b = ()
type TestType6c =
  ContainerBox Foo '[Box Bar,Box Fov,Box Fov,Box Foo]
test6c :: (IsRuleConform TestType6c TestRule6 ~ 'True) => ()
test6c = ()
type TestType6d = ContainerBox Foo '[Box Bar,Box Foo]
test6d :: (IsRuleConform TestType6d TestRule6 ~ 'True) => ()
test6d = ()
----
type TestRule7 =
  TopLevel (ContainerBox "foo "
           '[ SomeOptionalX (ContainerBox "fov "
              '[ OnceOptionalX (ContainerBox "baz "
                               '[Box "foo "])
               , SomeMandatoryX (Box "bar ")
               ])])
type TestType7a = ContainerBox Foo
                    '[ ]
test7a :: (IsRuleConform TestType7a TestRule7 ~ 'True) => ()
test7a = ()
type TestType7b = ContainerBox Foo
                    '[ ContainerBox Fov
                         '[ContainerBox Baz
                             '[Box Foo]
                          , Box Bar]]
test7b :: (IsRuleConform TestType7b TestRule7 ~ 'True) => ()
test7b = ()
type TestType7c = ContainerBox Foo
                    '[ ContainerBox Fov
                         '[ ContainerBox Baz
                             '[Box Foo]
                          , Box Bar
                          , Box Bar
                          , Box Bar
                          ]
                     , ContainerBox Fov
                          '[ ContainerBox Baz
                              '[Box Foo]
                           , Box Bar
                           , Box Bar
                           , Box Bar
                           ]
                     ]
test7c :: (IsRuleConform TestType7c TestRule7 ~ 'True) => ()
test7c = ()
