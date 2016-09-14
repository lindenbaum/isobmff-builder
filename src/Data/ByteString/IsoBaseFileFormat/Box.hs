{-# LANGUAGE UndecidableInstances #-}

-- | Definition of the most basic element in an ISOBMFF file: a /box/.  See
-- Chapter 4 in the standard document.  A box is a container with a type, a
-- size, some data and some nested boxes. The standard defines - among other
-- characteristics - available box types and their semantics, the fields they
-- contain and how they are nested into each other.  This library tries to
-- capture some of these characteristics using modern Haskell type system
-- features, in order to provide compile time checks for (partial) standard
-- compliance.
module Data.ByteString.IsoBaseFileFormat.Box where

import Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.Singletons.Prelude.List                      ((:++),
                                                                    Length)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

-- * Box Type Classes
-- | Base class for all (abstract/phantom/normal-) types that represent boxes
class (KnownSymbol (BoxTypeSymbol t), IsBoxContent (BoxContent t))
   => IsBox (t :: Type) where
  type BoxContent t
  type BoxContent t = t
  toBoxType :: proxy t -> BoxType
  toBoxType _ = parseBoxType (Proxy :: Proxy (BoxTypeSymbol t))

-- | A type family used by the type-level consistency checks. It is required
-- that  an instance of this type family exists for every 'IsBox' instance.
-- This family could not be associative since it is used by type families that
-- cannot have type class constraints.
type family BoxTypeSymbol (t :: k) :: Symbol

-- | Types that go into a box. A box content is a piece of data that can be
-- reused in different instances of 'IsBox'. It has no 'BoxType' and hence
-- defines no box.
class IsBoxContent a  where
  boxSize :: a -> BoxSize
  boxBuilder :: a -> Builder

-- * Box Contents
--  TODO move all IsBoxContent stuff to BoxContent.hs

-- | A type that wraps the contents of a box and the box type.
data Box b where
        Box ::
            !(BoxContent b) -> Box b

instance IsBox b => IsBox (Box b) where
  type BoxContent (Box b) = BoxContent b
  toBoxType _ = toBoxType (Proxy :: Proxy b)

type instance BoxTypeSymbol (Box b) = BoxTypeSymbol b

instance IsBox cnt => IsBoxContent (Box cnt) where
  boxBuilder b@(Box cnt) = sB <> tB <> sExtB <> tExtB <> cntB
    where s = boxSize b
          t = toBoxType b
          sB = boxBuilder s
          sExtB = boxBuilder (BoxSizeExtension s)
          tB = boxBuilder t
          tExtB = boxBuilder (BoxTypeExtension t)
          cntB = boxBuilder cnt
  boxSize b@(Box cnt) = sPayload + boxSize (BoxSizeExtension sPayload)
    where sPayload =
            boxSize (BoxSize undefined) + boxSize t + boxSize cnt +
            boxSize (BoxTypeExtension t)
          t = toBoxType b

instance (Default (BoxContent b)) => Default (Box b) where
  def = Box def

-- | Compose 'BoxContent' and 'Boxes' under the Constraint that they are
-- composable.
data ContainerBox b (bs :: [Type]) where
  ContainerBox :: IsBox b => !(BoxContent b) -> !(Boxes bs) -> ContainerBox b bs

instance (IsBox b) => IsBox (ContainerBox b bs)

type instance BoxTypeSymbol (ContainerBox b bs) = BoxTypeSymbol b

instance IsBoxContent (ContainerBox b bs) where
    boxSize (ContainerBox c bs) = boxSize (c :+ bs)
    boxBuilder (ContainerBox c bs) = boxBuilder (c :+ bs)

-- | A heterogenous collection of boxes.
data Boxes (boxTypes :: [Type]) where
    NoBoxes :: Boxes '[]
    (:.) :: IsBox l => !(Box l) -> !(Boxes r) -> Boxes (Box l ': r)
    -- | Create a 'Boxes' collection from two 'Box'es
    (:<>) :: !(Boxes l) -> !(Boxes r) -> Boxes (l :++ r)
    (:|) :: (IsBox l, IsBox r) => !(Box l) -> !(Box r) -> Boxes '[Box l, Box r]

infixr 1 :<>
infixr 2 :.
infixr 2 :|
infixr 3 $:

-- | Apply a function to a 'Boxes' collection containing only a single 'Box'.
($:) :: IsBox l => (Boxes '[Box l] -> r) -> Box l -> r
($:) f = f . singletonBox

-- | Create a 'Boxes' collection with a single 'Box'.
singletonBox :: IsBox l =>  Box l -> Boxes '[Box l]
singletonBox b = b :. NoBoxes

-- | Get the elements in a type level array
typeListLength :: forall a proxy (ts :: [k]) . (KnownNat (Length ts), Num a)
               => proxy ts -> a
typeListLength _ = fromIntegral (natVal (Proxy :: Proxy (Length ts)))

instance IsBoxContent (Boxes bs) where
  boxSize NoBoxes = 0
  boxSize (l :. r) = boxSize l + boxSize r
  boxSize (l :| r) = boxSize l + boxSize r
  boxSize (l :<> r) = boxSize l + boxSize r
  boxBuilder NoBoxes = mempty
  boxBuilder (l :. r) = boxBuilder l <> boxBuilder r
  boxBuilder (l :| r) = boxBuilder l <> boxBuilder r
  boxBuilder (l :<> r) = boxBuilder l <> boxBuilder r

-- | A box that contains no fields, but nested boxes.
containerBox :: (IsBox t)
             => BoxContent t -> Boxes ts -> Box (ContainerBox t ts)
containerBox c bs = Box (ContainerBox c bs)

-- * Box Size and Type
-- | The size of the box. If the size is limited to a (fixed) value, it can be
-- provided as a 'Word64' which will be represented as either a 32bit compact
-- size or as 64 bit /largesize/. If 'UnlimitedSize' is used, the box extends to
-- the end of the file.
data BoxSize
  = UnlimitedSize
  | BoxSize !Word64
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
newtype BoxSizeExtension =
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
    StdType !FourCc
  |
    -- | CustomBoxType defines custom @boxType@s in `Box`es.
    CustomBoxType !String
  deriving (Show,Eq)

-- | Create a box type from a 'Symbol'. Parse the  symbol value, if it's a four
-- charachter code, then return that as 'StdType' otherwise parse a UUID (TODO)
-- and return a 'CustomBoxType'.
parseBoxType :: KnownSymbol t => proxy t -> BoxType
parseBoxType px = StdType (fromString (symbolVal px))

-- | A type containin a printable four letter character code. TODO replace impl with U32Text
newtype FourCc =
  FourCc (Char,Char,Char,Char)
  deriving (Show,Eq)

instance IsString FourCc where
  fromString !str
    | length str == 4 =
      let [!a,!b,!c,!d] = str
      in FourCc (a,b,c,d)
    | otherwise =
      error ("cannot make a 'FourCc' of a String which isn't exactly 4 bytes long: " ++
             show str ++ " has a length of " ++ show (length str))

instance IsBoxContent FourCc where
  boxSize _ = 4
  boxBuilder (FourCc (!a,!b,!c,!d)) = putW a <> putW b <> putW c <> putW d
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
newtype BoxTypeExtension =
  BoxTypeExtension BoxType

instance IsBoxContent BoxTypeExtension where
  boxSize (BoxTypeExtension !(StdType _)) = 0
  boxSize (BoxTypeExtension !(CustomBoxType _)) = 16 * 4
  boxBuilder (BoxTypeExtension !(StdType _)) = mempty
  boxBuilder (BoxTypeExtension !(CustomBoxType str)) =
    mconcat (map (word8 . fromIntegral . fromEnum)
                 (take (16 * 4) str) ++
             repeat (word8 0))

-- * 'IsBoxContent' instances
-- | An empty box content can by represented by @()@ (i.e. /unit/).
instance IsBoxContent () where
  boxSize _ = 0
  boxBuilder _ = mempty

-- | Trivial instance for 'ByteString'
instance IsBoxContent B.ByteString where
  boxSize = fromIntegral . B.length
  boxBuilder = byteString-- -- | A list, a maybe, and every other 'Foldable' of contents is a content.
                         -- instance (Foldable f, IsBoxContent t) => IsBoxContent (f t) where
                         --   boxSize = foldr' (\e acc -> acc + boxSize e) 0
                         --   boxBuilder = foldMap boxBuilder

-- | This 'Text' instance writes a null terminated UTF-8 string.
instance IsBoxContent T.Text where
  boxSize = (1+) . fromIntegral . T.length
  boxBuilder txt = boxBuilder (T.encodeUtf8 txtNoNulls) <> word8 0
    where txtNoNulls = T.map (\c -> if c == '\0' then ' ' else c) txt

-- | This instance writes zero bytes for 'Nothing' and delegates on 'Just'.
instance IsBoxContent a => IsBoxContent (Maybe a) where
  boxSize = maybe 0 boxSize
  boxBuilder = maybe mempty boxBuilder

-- * Box concatenation

-- | Box content composition
data a :+ b = !a :+ !b

infixr 3 :+

instance (IsBoxContent p,IsBoxContent c) => IsBoxContent (p :+ c) where
  boxSize    (p :+ c) = boxSize    p +  boxSize    c
  boxBuilder (p :+ c) = boxBuilder p <> boxBuilder c

instance (Default a, Default b) => Default (a :+ b) where
  def = def :+ def

-- * Tagged boxes

instance IsBoxContent c => IsBoxContent (Tagged s c) where
  boxSize = boxSize . untag
  boxBuilder = boxBuilder . untag

-- * List Box Content

-- | A list of things that renders to a size field with the number of elements
-- and the sequence of elements. This type is index with the size field type.
newtype ListContent sizeType contentType =
  ListContent [contentType]

instance (Num sizeType, IsBoxContent sizeType, IsBoxContent contentType)
  => IsBoxContent (ListContent sizeType contentType) where
    boxSize (ListContent es) =
      boxSize (fromIntegral (length es) :: sizeType) + sum (boxSize <$> es)
    boxBuilder (ListContent es) =
      boxBuilder (fromIntegral (length es) :: sizeType)
      <> fold (boxBuilder <$> es)

instance Default (ListContent sizeTupe contentType) where
  def = ListContent []

-- * Words and Integer

instance IsBoxContent Word8 where
  boxSize _ = 1
  boxBuilder = word8

instance IsBoxContent Word16 where
  boxSize _ = 2
  boxBuilder = word16BE

instance IsBoxContent Word32 where
  boxSize _ = 4
  boxBuilder = word32BE

instance IsBoxContent Word64 where
  boxSize _ = 8
  boxBuilder = word64BE

instance IsBoxContent Int8 where
  boxSize _ = 1
  boxBuilder = int8

instance IsBoxContent Int16 where
  boxSize _ = 2
  boxBuilder = int16BE

instance IsBoxContent Int32 where
  boxSize _ = 4
  boxBuilder = int32BE

instance IsBoxContent Int64 where
  boxSize _ = 8
  boxBuilder = int64BE

-- * Boxes of Bits

instance IsBoxContent (BuilderBox tag) where
  boxSize (MkBuilderBox !s _) = fromIntegral s
  boxBuilder (MkBuilderBox _ !b) = b

instance (KnownSymbol (BoxTypeSymbol tag)) => IsBox (BuilderBox tag) where

type instance BoxTypeSymbol (BuilderBox tag) = BoxTypeSymbol tag

-- * Type Layout Rule Matchers

-- | Mandatory, container box, exactly one
type OM b bs = ContainerBox b bs
-- | Optional, container box, zero or one
type OO b bs = OnceOptionalX (ContainerBox b bs)
-- | Mandatory, container box, one or more
type SM b bs = SomeMandatoryX (ContainerBox b bs)
-- | Optional, container box, zero or more
type SO b bs = SomeOptionalX (ContainerBox b bs)

-- | Mandatory, exactly one, no children
type OM_ b = Box b
-- | Optional, zero or one, no children
type OO_ b = OnceOptionalX (Box b)
-- | Mandatory, one or more, no children
type SM_ b = SomeMandatoryX (Box b)
-- | Optional, zero or more, no children
type SO_ b = SomeOptionalX (Box b)

----
type instance IsRuleConform (Box b) (Box r) = BoxTypeSymbol b == BoxTypeSymbol r
----
type instance IsRuleConform (Boxes bs) (Boxes rs) = IsRuleConform bs rs -- TODO
----
type instance IsRuleConform (Box b) (ContainerBox b' rules)
  = IsContainerBox b
    && IsRuleConform (Box b) (Box b')
    && IsRuleConform (ChildBoxes b) (Boxes rules)
type family IsContainerBox t :: Bool where
  IsContainerBox (ContainerBox a as) = 'True
  IsContainerBox b = 'False
type family ChildBoxes c where
  ChildBoxes (ContainerBox a as) = Boxes as
