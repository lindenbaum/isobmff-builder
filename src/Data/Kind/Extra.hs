{-# LANGUAGE UndecidableInstances #-}
-- | Kind-level utilities to guide and simplify programming at the type level
module Data.Kind.Extra
  ( type IsA
  , type IsAn
  , type Eval
  , type Convert
  , type (~~>)
  , type (-->)
  , type (-->|)
  ) where

import Data.Proxy (KProxy(..))
import Data.Kind (type Type)

-- | A kind alias to turn a data type used as kind type to a kind signature
-- matching other data types (umh.. kinds?) whose data-type signature ends in
-- @foo -> Type@.
type IsA foo = foo -> Type

-- | An alias to 'IsA'
type IsAn oo = IsA oo

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
data Itself :: foo -> IsA foo
type instance Eval (Itself foo) = foo

-- | Coerce a type, that @'IsA' foo@ to a type that @'IsA' bar@, using
-- 'CoerceTo'.
--
--  When 'Eval'uated, also 'Eval'uate the parameter, which @'IsA' foo@, and
-- 'CoerceTo' a @bar@.
--
-- Consider using 'Promoted', like e.g.: type ToColor x = x :-->: Promoted Color
data (:-->:) :: IsA foo -> IsA bar -> IsA bar
type instance Eval (foo :-->: bar) = CoerceTo (Eval foo) bar

-- | Alias for ':-->:' that lifts the burden of creating a 'KProxy' from the
-- caller.
type (x :: IsA foo) --> bar = (:-->:) x (A bar)
infixl 3 -->

 -- | Alias for @'Eval' (foo ':-->:' A bar)@.
type foo -->| bar = Eval (foo --> A bar)
infixl 1 -->|

-- | A type @foo@ (of kind 'Type') might give rise to a 'Promote'd type of
-- __kind__ @foo@.
data A foo :: IsA (Promoted foo)

type instance Eval (A foo :: IsA promotedFoo) = Promoted foo

-- | In certain cases this can replace the ugly 'KProxy', note that this is in
-- many ways like a 'KProxy', since there is no way to pass anything but
-- __kind__ information about the __type__ parameter.
data Promoted bar

-- | Define how a @foo@ could be converted to something that @'IsA' bar@
type family CoerceTo (x :: foo) (p :: Promoted bar) :: bar

-- ** Standard Types

-- | Either use the value from @Just@ or return a fallback value(types(kinds))
data FromMaybe :: IsAn x -> Maybe x -> IsAn x
type instance Eval (FromMaybe fallback ('Just t)) = t
type instance Eval (FromMaybe fallback 'Nothing) = Eval fallback
