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
type family IsRuleConform (b :: k) (r :: l) :: Bool
data IsRuleConform0 :: k ~> l ~> Bool
type instance Apply IsRuleConform0 ts = IsRuleConform1 ts
data IsRuleConform1 :: k -> l ~> Bool
type instance Apply (IsRuleConform1 ts) rule = IsRuleConform ts rule
----
data TopLevel  :: Type -> Type
type instance IsRuleConform t (TopLevel rule)
  = IsRuleConform  t rule
----
data IsBox (fourcc :: Symbol)
type instance IsRuleConform t (IsBox fourcc) = ToFourCc t == fourcc
type family ToFourCc t :: Symbol
data ToFourCc0 :: Type ~> Symbol
type instance Apply ToFourCc0 t = ToFourCc t
----
data IsContainerBox (fourcc :: Symbol) (ts :: [ContainmentRule])
type instance IsRuleConform (ContainerBox b bs) (IsContainerBox fourcc ts)
  = ToFourCc b == fourcc && IsContainerBoxConform (Map ToFourCc0 bs) ts
----
data ContainmentRule
   = OnceOptionalX Symbol
   | OnceMandatoryX Symbol
   | SomeOptionalX Symbol
   | SomeMandatoryX Symbol
type family
  IsContainerBoxConform (bs :: [Symbol]) (rs :: [ContainmentRule]) :: Bool where
   IsContainerBoxConform '[]       '[]                      = 'True
   IsContainerBoxConform (b ': bs) '[]                      = 'False
   --
   IsContainerBoxConform '[]       (OnceOptionalX r ': rs)  = IsContainerBoxConform '[] rs
   IsContainerBoxConform (r ': bs) (OnceOptionalX r ': rs)  = IsContainerBoxConform  bs rs
   IsContainerBoxConform bs        (OnceOptionalX r ': rs)  = IsContainerBoxConform  bs rs
   --
   IsContainerBoxConform '[]       (SomeOptionalX r ': rs)  = IsContainerBoxConform '[] rs
   IsContainerBoxConform (r ': bs) (SomeOptionalX r ': rs)  = IsContainerBoxConform  bs (SomeOptionalX r ': rs)
   IsContainerBoxConform bs        (SomeOptionalX r ': rs)  = IsContainerBoxConform  bs rs
   --
   IsContainerBoxConform (r ': bs) (OnceMandatoryX r ': rs)  = IsContainerBoxConform  bs rs
   IsContainerBoxConform bs        (OnceMandatoryX r ': rs)  = 'False
   --
   IsContainerBoxConform (r ': bs) (SomeMandatoryX r ': rs)  = IsContainerBoxConform  bs (SomeOptionalX r ': rs)
   IsContainerBoxConform bs        (SomeMandatoryX r ': rs)  = 'False
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
type TestRule1 = TopLevel (IsBox "foo ")
type TestType1 = Foo
test1 :: (IsRuleConform TestType1 TestRule1 ~ 'True) => ()
test1 = ()
----
type TestRule2 = TopLevel (IsContainerBox "foo " '[OnceOptionalX "bar "])
type TestType2a = ContainerBox Foo '[]
test2a :: (IsRuleConform TestType2a TestRule2 ~ 'True) => ()
test2a = ()
--
type TestType2b = ContainerBox Foo '[Bar]
test2b :: (IsRuleConform TestType2b TestRule2 ~ 'True) => ()
test2b = ()
----
type TestRule3 = TopLevel (IsContainerBox "foo " '[SomeOptionalX "bar "])
type TestType3a = ContainerBox Foo '[]
test3a :: (IsRuleConform TestType3a TestRule3 ~ 'True) => ()
test3a = ()
--
type TestType3b = ContainerBox Foo '[Bar]
test3b :: (IsRuleConform TestType3b TestRule3 ~ 'True) => ()
test3b = ()
--
type TestType3c = ContainerBox Foo '[Bar,Bar]
test3c :: (IsRuleConform TestType3c TestRule3 ~ 'True) => ()
test3c = ()
----
type TestRule4 = TopLevel (IsContainerBox "foo " '[OnceMandatoryX "bar "])
type TestType4 = ContainerBox Foo '[Bar]
test4 :: (IsRuleConform TestType4 TestRule4 ~ 'True) => ()
test4 = ()
----
type TestRule5 = TopLevel (IsContainerBox "foo " '[SomeMandatoryX "bar "])
type TestType5 = ContainerBox Foo '[Bar,Bar]
test5 :: (IsRuleConform TestType5 TestRule5 ~ 'True) => ()
test5 = ()
type TestType5b = ContainerBox Foo '[Bar]
test5b :: (IsRuleConform TestType5b TestRule5 ~ 'True) => ()
test5b = ()
----
type TestRule6 =
  TopLevel (IsContainerBox "foo "
           '[ OnceOptionalX "baz "
            , SomeMandatoryX "bar "
            , SomeOptionalX "fov "
            , OnceMandatoryX "foo "])
type TestType6a = ContainerBox Foo '[Baz,Bar,Bar,Bar,Fov,Fov,Foo]
test6a :: (IsRuleConform TestType6a TestRule6 ~ 'True) => ()
test6a = ()
