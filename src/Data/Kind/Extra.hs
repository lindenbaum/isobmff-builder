{-# LANGUAGE UndecidableInstances #-}
-- | Kind-level utilities to guide and simplify programming at the type level
module Data.Kind.Extra
  ( type IsA
  , type IsAn
  , type Eval
  , type (~~>)
  , type (-->)
  , type Promote
  , Promoted(..)
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
-- type instance Eval (ColoredText c txt) = 'WithColor c ('RenderText txt)
-- @
--
type family Eval (t :: IsA foo) :: foo

-- | A type @foo@, of course, @'IsA' foo@ 'Itself'. All other good names, like
-- 'Pure', 'Id' or 'Return' are taken.
data Itself :: forall foo . foo -> IsA foo
type instance Eval (Itself foo) = foo

-- | Coerce a type, that @'IsA' foo@ to a type that @'IsA' bar@, using
-- '~~>'.
--
--  When 'Eval'uated, also 'Eval'uate the parameter, which @'IsA' foo@, and
-- '~~>' a @bar@.
--
-- Instead of just allowing to pass the destination kind directly as a type,
-- accept only 'Promoted'; this makes clear that the parameter is thrown away
-- after ripping the type information of it. 'Promoted is exactly equal to e.g.
-- 'KProxy', but the name 'KProxy' just looks confusing.
data (:-->:) (f :: IsA foo) bar :: IsA bar
type instance Eval (foo :-->: bar) = (~~>) (Eval foo) bar

-- | Define how a @foo@ could be converted to something that @'IsA' bar@.
-- This is used in the evaluation of ':-->:'.
type family (~~>) (x :: foo) bar :: bar
type instance (~~>) (x :: foo) foo = x

-- | Alias for ':-->:' that lifts the burden of some nasty typing.
type (-->) (x :: IsA foo) (p :: Type) = (((:-->:) x p) :: IsA p)
infixl 3 -->

 -- | Alias for @'Eval' (foo ':-->:' bar)@.
type (x :: IsA foo) -->| (p :: Type) = (Eval (x --> p) :: p)
infixl 1 -->|

-- | Use promoted types of the kind @bar@. This data type isn't strictly
-- required, but it help express explicitly that @bar@ is used only as kind.
--
-- 'Promoted' is defined like 'KProxy'.
--
-- There is no way to pass anything but __kind__ information about the __type__
-- parameter.
data Promoted :: forall foo . foo -> Type where
  MkPromoted :: Promoted foo

-- | An alias for 'MkPromoted' that accepts the type to promote as explicit type
-- parameter.
type Promote (foo :: Type) = ('MkPromoted :: Promoted foo)

-- ** Standard Types

-- | Either use the value from @Just@ or return a fallback value(types(kinds))
data FromMaybe :: forall x . IsAn x -> Maybe x -> IsAn x
type instance Eval (FromMaybe fallback ('Just t)) = t
type instance Eval (FromMaybe fallback 'Nothing) = Eval fallback
