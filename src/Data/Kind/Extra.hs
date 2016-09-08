{-# LANGUAGE UndecidableInstances #-}
-- | Kind-level utilities to guide and simplify programming at the type level
module Data.Kind.Extra
  ( type IsA
  , type IsAn
  , type Return
  , type Extract
  , type (~~>)
  , type (-->)
  , type (-->|)
  ) where

import Data.Kind (type Type)

-- | A kind alias to turn a data type used as kind type to a kind signature
-- matching other data types (umh.. kinds?) whose data-type signature ends in
-- @foo -> Type@.
type IsA (foo :: Type) = (foo -> Type :: Type)

-- | An alias to 'IsA'
type IsAn (oo :: Type) = (IsA oo :: Type)

-- | A type family for generating the (promoted) types of a phantom data
--  type(kind) from other data types that have a kind that /ends/ in e.g.
--  @'IsA' Foo@.
--
-- Complete example:
--
-- @
-- data PrettyPrinter c where
--   RenderText :: Symbol -> PrettyPrinter Symbol
--   WithColor :: Color -> PrettyPrinter c -> PrettyPrinter c
--
-- data Color = Black | White
--
-- data ColoredText :: Color -> Symbol -> IsA (PrettyPrinter Symbol)
--
-- type instance Extract (ColoredText c txt) = 'WithColor c ('RenderText txt)
-- @
--
type family Extract (t :: IsA foo) :: foo

-- | A type @foo@, of course, @'IsA' foo@ 'Return'. All other good names, like
-- 'Pure', 'Id' or 'Return' are taken.
data Return (a :: foo) :: IsA foo
type instance Extract (Return foo :: IsA fooKind) = foo

-- | Coerce a type, that @'IsA' foo@ to a type that @'IsA' bar@, using
-- '~~>'.
--
--  When 'Extract'uated, also 'Extract'uate the parameter, which @'IsA' foo@, and
-- '~~>' a @bar@.
--
-- Instead of just allowing to pass the destination kind directly as a type,
-- accept only 'Promoted'; this makes clear that the parameter is thrown away
-- after ripping the type information of it. 'Promoted is exactly equal to e.g.
-- 'KProxy', but the name 'KProxy' just looks confusing.
data (:->:) (t :: IsA foo) bar :: IsA bar
type instance Extract (foo :->: bar) = (~~>) (Extract foo) bar

data FMap (f :: i -> o) :: IsAn i -> IsAn o
type instance Extract (FMap f i) = f (Extract i)

data Pure (f :: o) :: IsAn o
type instance Extract (Pure f) = f
type Return f = Pure f

data Duplicate :: IsAn a -> IsAn (IsAn a)
type instance Extract (Duplicate f) = f

data Extend :: (IsA foo -> bar) -> IsA foo -> IsA bar
type instance Extract (Extend f aFoo) = f aFoo

data (:<@>) :: IsA (a -> b) -> IsAn a -> IsA b
type instance Extract (f :<@> a) = (Extract f) (Extract a)

-- | Define how a @foo@ could be converted to something that @'IsA' bar@.
-- This is used in the evaluation of ':->:'.
type family (~~>) (x :: foo) bar :: bar
type instance (~~>) (x :: foo) foo = x
infixl 9 ~~>

-- | Alias for ':-->:' that lifts the burden of some nasty typing.
type (-->) (x :: IsA foo) (p :: Type) = (((:->:) x p) :: IsA p)
infixl 9 -->

 -- | Alias for @'Extract' (foo ':-->:' bar)@.
type (x :: IsA foo) -->| (p :: Type) = (Extract (x --> p) :: p)
infixl 9 -->|

-- ** Standard Types

-- | Either use the value from @Just@ or return a fallback value(types(kinds))
data FromMaybe :: forall x . IsAn x -> Maybe x -> IsAn x
type instance Extract (FromMaybe fallback ('Just t)) = t
type instance Extract (FromMaybe fallback 'Nothing) = Extract fallback
