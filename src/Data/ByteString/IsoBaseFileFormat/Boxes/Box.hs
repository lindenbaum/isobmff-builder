{-# LANGUAGE UndecidableInstances #-}

-- | Definition of the most basic element in an ISOBMFF file: a /box/.  See
-- Chapter 4 in the standard document.  A box is a container with a type, a
-- size, some data and some nested boxes. The standard defines - among other
-- characteristics - available box types and their semantics, the fields they
-- contain and how they are nested into each other.  This library tries to
-- capture some of these characteristics using modern Haskell type system
-- features, in order to provide compile time checks for (partial) standard
-- compliance.
module Data.ByteString.IsoBaseFileFormat.Boxes.Box
       (module Data.ByteString.IsoBaseFileFormat.Boxes.Box, module X)
       where

import Data.Bits as X
import Data.ByteString.Builder as X
import Data.Monoid as X
import Data.Proxy as X
import Data.Word as X
import GHC.TypeLits as X
import Data.String
import Data.Type.Equality
import Data.Type.List
import Data.Type.Bool
import qualified Data.ByteString as B

-- * Basic Types and classes

-- | Base class for all (abstract/phantom/normal-) types that represent boxes
class (IsBoxContent (BoxContent t), BoxRules t) => IsBoxType' t where
  type BoxContent t
  type BoxContent t = ()
  toBoxType' :: proxy t -> BoxType

-- | A class that describes (on the type level) how a box can be nested into
-- other boxes (see 'Boxes).
class BoxRules (t :: k) where
  -- | List of boxes that this box can be nested into.
  type RestrictedTo t :: Maybe [k]
  type RestrictedTo t = 'Just '[]
  -- | If the box is also allowed 'top-level' i.e. in the file directly, not
  -- nested in an other box.
  type IsTopLevelBox t :: Bool
  type IsTopLevelBox t = 'True
  -- | Describes which nested boxes MUST be present in a box using 'boxes'.
  type RequiredNestedBoxes t :: [k]
  type RequiredNestedBoxes t = '[]
  -- | Describes how many times a box should be present in a container (-box).
  type GetCardinality t (c :: k) :: Cardinality
  type GetCardinality t any = 'ExactlyOnce

-- | Describes how many times a box should be present in a container.
data Cardinality = AtMostOnce | ExactlyOnce | OnceOrMore

-- | Types that go into a box. A box content is a piece of data that can be
-- reused in different instances of 'IsBox'. It has no 'BoxType' and hence
-- defines no box.
class IsBoxContent a  where
  boxSize :: a -> BoxSize
  boxBuilder :: a -> Builder

-- * Data types

-- | A type that wraps the contents of a box and the box type.
data Box' b where
  Box' :: (IsBoxType' b, ValidBoxes b ts) => BoxContent b -> Boxes ts -> Box' b

instance IsBoxContent (Box' cnt) where
  boxBuilder b@(Box' cnt nested) = sB <> tB <> sExtB <> tExtB <> cntB <> nestedB
    where s       = boxSize    b
          t       = toBoxType' b
          sB      = boxBuilder s
          sExtB   = boxBuilder (BoxSizeExtension s)
          tB      = boxBuilder t
          tExtB   = boxBuilder (BoxTypeExtension t)
          cntB    = boxBuilder cnt
          nestedB = boxBuilder nested

  boxSize b@(Box' cnt nested) = sPayload + boxSize (BoxSizeExtension sPayload)
    where sPayload =   boxSize (BoxSize undefined)
                     + boxSize t
                     + boxSize cnt
                     + boxSize (BoxTypeExtension t)
                     + boxSize nested
          t        = toBoxType' b

-- | A heterogenous collection of boxes.
data Boxes (boxTypes :: [k]) where
        Nested :: Boxes '[]
        (:.) :: Boxes ts -> Box' t -> Boxes (t ': ts)

infixl 2 :.

instance IsBoxContent (Boxes bs) where
  boxSize Nested = 0
  boxSize (bs :. b) = boxSize bs + boxSize b
  boxBuilder Nested = mempty
  boxBuilder (bs :. b) = boxBuilder bs <> boxBuilder b

-- | A box that contains no nested boxes.
closedBox :: (IsBoxType' t, ValidBoxes t '[]) => BoxContent t -> Box' t
closedBox c = Box' c Nested

-- | A box that contains no fields, but nested boxes.
containerBox :: (IsBoxType' t, ValidBoxes t ts, BoxContent t ~ ()) => Boxes ts -> Box' t
containerBox = Box' ()

-- | A complete media file, consisting of top-level boxes.
mediaFile :: Boxes ts -> Builder
mediaFile = boxBuilder

-- * Box Size and Type

-- | The size of the box. If the size is limited to a (fixed) value, it can be
-- provided as a 'Word64' which will be represented as either a 32bit compact
-- size or as 64 bit /largesize/. If 'UnlimitedSize' is used, the box extends to
-- the end of the file.
data BoxSize
  = UnlimitedSize
  | BoxSize Word64
  deriving (Show,Eq)

instance IsBoxContent BoxSize where
  boxSize _ = BoxSize 4
  boxBuilder UnlimitedSize = word32BE 0
  boxBuilder (BoxSize n) =
    word32BE $
    if n < (4294967296 :: Word64)
       then fromIntegral n
       else 1

instance Num BoxSize where
  (+) UnlimitedSize _ = UnlimitedSize
  (+) _ UnlimitedSize = UnlimitedSize
  (+) (BoxSize l) (BoxSize r) = BoxSize (l + r)
  (-) UnlimitedSize _ = UnlimitedSize
  (-) _ UnlimitedSize = UnlimitedSize
  (-) (BoxSize l) (BoxSize r) = BoxSize (l - r)
  (*) UnlimitedSize _ = UnlimitedSize
  (*) _ UnlimitedSize = UnlimitedSize
  (*) (BoxSize l) (BoxSize r) = BoxSize (l * r)
  abs UnlimitedSize = UnlimitedSize
  abs (BoxSize n) = BoxSize (abs n)
  signum UnlimitedSize = UnlimitedSize
  signum (BoxSize n) = BoxSize (signum n)
  fromInteger n = BoxSize $ fromInteger n

-- | The 'BoxSize' can be > 2^32 in which case an 'BoxSizeExtension' must be
-- added after the type field.
data BoxSizeExtension =
  BoxSizeExtension BoxSize

instance IsBoxContent BoxSizeExtension where
  boxBuilder (BoxSizeExtension UnlimitedSize) = mempty
  boxBuilder (BoxSizeExtension (BoxSize n)) =
    if n < 4294967296
       then mempty
       else word64BE n
  boxSize (BoxSizeExtension UnlimitedSize) = 0
  boxSize (BoxSizeExtension (BoxSize n)) =
    BoxSize $
    if n < 4294967296
       then 0
       else 8

-- | A box has a /type/, this is the value level representation for the box type.
data BoxType
  =
    -- | `FourCc` can be used as @boxType@ in `Box`, standard four letter character
    -- code, e.g. @ftyp@
    StdType FourCc
  |
    -- | CustomBoxType defines custom @boxType@s in `Box`es.
    CustomBoxType String
  deriving (Show,Eq)

-- | A type containin a printable four letter character code.
newtype FourCc =
  FourCc (Char,Char,Char,Char)
  deriving (Show,Eq)

instance IsString FourCc where
  fromString str
    | length str == 4 =
      let [a,b,c,d] = str
      in FourCc (a,b,c,d)
    | otherwise =
      error ("cannot make a 'FourCc' of a String which isn't exactly 4 bytes long: " ++
             show str ++ " has a length of " ++ show (length str))

instance IsBoxContent FourCc where
  boxSize _ = 4
  boxBuilder (FourCc (a,b,c,d)) = putW a <> putW b <> putW c <> putW d
    where putW = word8 . fromIntegral . fromEnum

instance IsBoxContent BoxType where
  boxSize _ = boxSize (FourCc undefined)
  boxBuilder t =
    case t of
      StdType x -> boxBuilder x
      CustomBoxType _ -> boxBuilder (FourCc ('u','u','i','d'))

-- | When using custom types extra data must be written after the extra size
-- information. Since the box type and the optional custom box type are not
-- guaranteed to be consequtive, this type handles the /second/ part seperately.
data BoxTypeExtension =
  BoxTypeExtension BoxType

instance IsBoxContent BoxTypeExtension where
  boxSize (BoxTypeExtension (StdType _)) = 0
  boxSize (BoxTypeExtension (CustomBoxType _)) = 16 * 4
  boxBuilder (BoxTypeExtension (StdType _)) = mempty
  boxBuilder (BoxTypeExtension (CustomBoxType str)) =
    mconcat (map (word8 . fromIntegral . fromEnum)
                 (take (16 * 4) str) ++
             repeat (word8 0))

-- * Type-safe box composition


-- * Type level consistency checks

-- | A type-level check that uses 'BoxRules' to check that the contained boxes
-- are standard conform.
type ValidBoxes t ts =
  ( AllAllowedIn t ts ~ 'True
  , HasAllRequiredBoxes t (RequiredNestedBoxes t) ts ~ 'True )

-- | A type function to check that all nested boxes are allowed in the
-- container.
type family AllAllowedIn (container :: k) (boxes :: [k]) :: Bool
     where
        AllAllowedIn c '[] = 'True
        AllAllowedIn c (t ': ts) =
          If (CheckAllowedIn c t (RestrictedTo t))
             (AllAllowedIn c ts)
             (TypeError (NotAllowedMsg c t))

type family CheckAllowedIn (c :: k) (t :: k) (a :: Maybe [k]) :: Bool where
  CheckAllowedIn c t 'Nothing   = 'True
  CheckAllowedIn c t ('Just rs) = Find c rs


-- | The custom (type-) error message for 'AllAllowedIn'.
type NotAllowedMsg c t =
  'Text "Boxes of type: "
  ':<>: 'ShowType c
  ':<>: 'Text " may not contain boxes of type "
  ':<>: 'ShowType t
  ':$$: 'Text "Valid containers for "
  ':<>: 'ShowType t
  ':<>: 'Text " boxes are: "
  ':$$: 'ShowType (RestrictedTo t)
  ':$$: 'ShowType t
  ':<>: If (IsTopLevelBox c)
          ('Text " boxes may appear top-level in a file.")
          ('Text " boxes must be nested.")


-- | Check that all required boxes have been nested.
type family HasAllRequiredBoxes (c :: k) (req :: [k]) (nested :: [k]) :: Bool
     where
       HasAllRequiredBoxes c '[] nested = 'True
       HasAllRequiredBoxes c (r ': restReq) nested =
         If (Find r nested)
            (HasAllRequiredBoxes c restReq nested)
            (TypeError (MissingRequired c r nested))

type IsSubSet base sub = Intersection base sub == sub

-- | The custom (type-) error message for 'HasAllRequiredBoxes.
type MissingRequired c r nested =
  'Text "Boxes of type: "
  ':<>: 'ShowType c
  ':<>: 'Text " require these nested boxes: "
  ':<>: 'ShowType (RequiredNestedBoxes c)
  ':$$: 'Text "but only these box types were nested: "
  ':<>: 'ShowType nested
  ':$$: 'Text "e.g. this type is missing: "
  ':<>: 'ShowType r

-- | Check that all boxes may appear top-level.
type family CheckAllTopLevelOk (ts :: [k]) :: Bool where
   CheckAllTopLevelOk '[] = 'True
   CheckAllTopLevelOk (t ': rest) = CheckTopLevelOk t && CheckAllTopLevelOk rest

-- | Check that the box may appear top-level.
type family CheckTopLevelOk (t :: k) :: Bool where
   CheckTopLevelOk t = IsTopLevelBox t || TypeError (NotTopLevenError t)


-- | The custom (type-) error message indicating that a box may not appear
-- top-level.
type NotTopLevenError c =
        'Text "Boxes of type "
  ':<>: 'ShowType c
  ':<>: 'Text " MUST be nested inside boxes of these types: "
  ':$$: 'ShowType (RestrictedTo c)

-- * 'IsBoxContent' instances

-- | An empty box content can by represented by @()@ (i.e. /unit/).
instance IsBoxContent () where
  boxSize _ = 0
  boxBuilder _ = mempty

-- | Trivial instance for 'ByteString'
instance IsBoxContent B.ByteString where
  boxSize = fromIntegral . B.length
  boxBuilder = byteString
