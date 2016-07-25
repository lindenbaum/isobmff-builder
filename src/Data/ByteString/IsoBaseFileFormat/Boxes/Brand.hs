-- | Brand/Box-validation
{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.IsoBaseFileFormat.Boxes.Brand
        (IsBrand(..), ValidBox, ValidContainerBox, ValidTopLevel,
        IsBrandConform, BoxTree(..), BoxForrest) where

import Data.Kind
import Data.Type.Bool
import GHC.TypeLits
-- import GHC.Exts
import Data.Singletons (Apply, type (~>))
import Data.Singletons.Prelude.List as S (Map, (:++))
import Data.Type.List (Find)

-- | A class that describes (on the type level) how a box can be nested into
-- other boxes (see 'Boxes).
class KnownNat (GetVersion brand) => IsBrand brand  where
  type BoxLayout brand :: BoxForrest
  type BoxLayout brand = '[]
  type GetVersion brand :: Nat
  type GetVersion brand = 0

-- | Boxes that valid according to the box structure defined in a 'IsBrand'
-- instance, i.e. where 'IsBrandConform' holds.
type family
  IsBrandConform (b :: Type) (parent :: Maybe Type) (ts :: [Type]) :: Constraint where
  IsBrandConform b 'Nothing ts =
    IsBrandConformImpl b (TopLevel b) ts (BoxLayout b)
  IsBrandConform b ('Just parent) ts =
    IsBrandConformImpl b parent ts (LookUpSubtrees parent (BoxLayout b))

-- | Convenience wrapper around @(IsBrandConform b ('Just t) '[])@
type ValidBox b t = IsBrandConform b ('Just t) '[]

-- | Convenience wrapper around @(IsBrandConform b ('Just t) ts)@
type ValidContainerBox b t ts = IsBrandConform b ('Just t) ts

-- | Convenience wrapper around @(IsBrandConform b 'Nothing ts)@
type ValidTopLevel b ts = IsBrandConform b 'Nothing ts


-- | Define the cardinality and valid child boxes of a box.
data BoxTree
  = OnceOptional Type
        BoxForrest
  | OnceMandatory Type
        BoxForrest
  | SomeOptional Type
        BoxForrest
  | SomeMandatory Type
        BoxForrest

-- | A list of 'BoxTree's
type BoxForrest = [BoxTree]

-- * Implementation

-- | A type for marking top-level boxes in TypeError messages, in the
-- 'IsBrandConformImpl' constraint.
data TopLevel :: t -> Type


type family
  LookUpSubtrees t (trees :: BoxForrest) :: BoxForrest where
    LookUpSubtrees t '[] = '[]
    LookUpSubtrees t ('OnceOptional t sub ': rest) = sub
    LookUpSubtrees t ('OnceOptional u sub ': rest) = LookUpSubtrees t (sub :++ rest)
    LookUpSubtrees t ('OnceMandatory t sub ': rest) = sub
    LookUpSubtrees t ('OnceMandatory u sub ': rest) = LookUpSubtrees t (sub :++ rest)
    LookUpSubtrees t ('SomeOptional t sub ': rest) = sub
    LookUpSubtrees t ('SomeOptional u sub ': rest) = LookUpSubtrees t (sub :++ rest)
    LookUpSubtrees t ('SomeMandatory t sub ': rest) = sub
    LookUpSubtrees t ('SomeMandatory u sub ': rest) = LookUpSubtrees t (sub :++ rest)

-- | A constraint that is solved if all 'Box'es layed out in accordance with the
-- 'BoxLayout' if an 'IsBrand' instance.
type IsBrandConformImpl b (info :: Type) (ts :: [Type]) (boxForrest :: BoxForrest) =
  ( IsBrand b
  , ReportIt '[AllRequiredBoxes
                 boxForrest
                 ts]
  , ReportIt '[OnlyValidBoxes info
                 boxForrest
                 ts]
  , ReportIt (Map (RuleAppliesFun info ts)
                  boxForrest))

type family
  ReportIt (es :: [Maybe ErrorMessage]) :: Constraint where
    ReportIt '[] = ()
    ReportIt ('Nothing ': rest) = ReportIt rest
    ReportIt ('Just e ': rest) = TypeError e

type family
  AllRequiredBoxes (f :: BoxForrest) (ts :: [t]) :: Maybe ErrorMessage where
    AllRequiredBoxes '[] children = 'Nothing

    AllRequiredBoxes ('OnceMandatory c sub ': rules) '[] =
      'Just ('Text "OnceMandatory box '"
             ':<>: 'ShowType c ':<>: 'Text "' missing, or out of order.")
    AllRequiredBoxes ('SomeMandatory c sub ': rules) '[] =
      'Just ('Text "SomeMandatory box '"
             ':<>: 'ShowType c ':<>: 'Text "' missing, or out of order.")

    AllRequiredBoxes ('OnceMandatory c sub ': rules) (c ': children) =
      AllRequiredBoxes rules children
    AllRequiredBoxes ('SomeMandatory c sub ': rules) (c ': children) =
      AllRequiredBoxes rules (RemoveNext c children)

    AllRequiredBoxes ('OnceMandatory c sub ': rules) children =
      AllRequiredBoxes rules children
    AllRequiredBoxes ('SomeMandatory c sub ': rules) children =
      AllRequiredBoxes rules children

    AllRequiredBoxes ('OnceOptional c sub ': rules) (c ': children) =
      AllRequiredBoxes rules children
    AllRequiredBoxes ('OnceOptional c sub ': rules) children =
      AllRequiredBoxes rules children
    AllRequiredBoxes ('SomeOptional c sub ': rules) children =
      AllRequiredBoxes rules (RemoveNext c children)

type family
  OnlyValidBoxes (r :: t) (f :: BoxForrest) (ts :: [t]) :: Maybe ErrorMessage where
    OnlyValidBoxes r rules (c ': children) =
      If (Find c (ExtractBoxes rules))
         (OnlyValidBoxes r rules children)
         ('Just ('Text "Invalid box '" ':<>: 'ShowType c
                 ':<>: 'Text "' in box '" ':<>: 'ShowType r ':<>: 'Text "'"))
    OnlyValidBoxes r rules '[] = 'Nothing

type family
  ExtractBoxes (f :: BoxForrest) :: [t] where
    ExtractBoxes '[] = '[]
    ExtractBoxes ('OnceMandatory c sub ': rules) =
      c ': ExtractBoxes rules
    ExtractBoxes ('SomeMandatory c sub ': rules) =
      c ': ExtractBoxes rules
    ExtractBoxes ('OnceOptional c sub ': rules) =
      c ': ExtractBoxes rules
    ExtractBoxes ('SomeOptional c sub ': rules) =
      c ': ExtractBoxes rules

type family
  RemoveNext (t :: k) (ts :: [k]) :: [k] where
    RemoveNext t (t ': ts) = RemoveNext t ts
    RemoveNext t ts = ts

data RuleAppliesFun :: t -> [t] -> BoxTree ~> Maybe ErrorMessage

type instance Apply (RuleAppliesFun parent childBoxes) rules =
  RuleApplies parent childBoxes rules

type family
  RuleApplies (parent :: t) (childBoxes :: [t]) (rules :: BoxTree) :: Maybe ErrorMessage where
  -- Zero or more
  RuleApplies parent children ('SomeOptional other sub) =
    'Nothing
  -- zero or one
  RuleApplies parent (child ': others) ('OnceOptional child sub) =
    If (Find child others)
       ('Just ('Text "It is not allowed to have more than one '"
               ':<>: 'ShowType child
               ':<>: 'Text "' in '"
               ':<>: 'ShowType parent
               ':<>: 'Text "'."))
      'Nothing
  RuleApplies parent (notChild ': others) ('OnceOptional child sub) =
    RuleApplies parent others ('OnceOptional child sub)
  RuleApplies parent '[] ('OnceOptional child sub) = 'Nothing
  -- exactly one
  RuleApplies parent (child ': others) ('OnceMandatory child sub) =
    If (Find child others)
       ('Just ('Text "It is not allowed to have more than one '"
               ':<>: 'ShowType child
               ':<>: 'Text "' in '"
               ':<>: 'ShowType parent
               ':<>: 'Text "'."))
      'Nothing
  RuleApplies parent (notChild ': others) ('OnceMandatory child sub) =
    RuleApplies parent others ('OnceMandatory child sub)
  RuleApplies parent '[] ('OnceMandatory child sub) =
    'Just ('Text "Need exactly one '"
           ':<>: 'ShowType child
           ':<>: 'Text "' in '"
           ':<>: 'ShowType parent
           ':<>: 'Text "'.")
  -- OnceMandatory or more
  RuleApplies parent (child ': others) ('SomeMandatory child sub) =
    'Nothing
  RuleApplies parent (notChild ': others) ('SomeMandatory child sub) =
    RuleApplies parent others ('SomeMandatory child sub)
  RuleApplies parent '[] ('SomeMandatory child sub) =
    'Just ('Text "Need at least one '"
           ':<>: 'ShowType child
           ':<>: 'Text "' in '"
           ':<>: 'ShowType parent
           ':<>: 'Text "'.")
