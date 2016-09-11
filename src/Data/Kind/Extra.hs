{-# LANGUAGE UndecidableInstances #-}
-- | Kind-level utilities to guide and simplify programming at the type level
module Data.Kind.Extra
  ( type A
  , type IsA
  , type IsAn
  , type Return
  , type Pure
  , type Eval
  , type (:->)
  , type Apply
  , type (:$)
  , type ($~)
  , type CoerceTo
  , type (~~>)
  , type Optional
  , type CoerceListTo
  ) where

import Data.Kind (type Type)

-- * Symbolic Types and Type Functions

-- | A /symbolic/ type, i.e. a wrapper around a (poly kinded) type to be
-- produced by 'Eval' instances.
--
-- All data types, e.g. @data Point2 x y :: Type@ can be made into /symbolic
-- representations/ of other types, by adding a /symbolic/ type parameter:
-- @data Point2 x y :: A Vec2 -> Type@.
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
data A :: forall foo . foo -> Type where
  MkA :: A foo

-- | Type alias for 'A' such that @data Point2 x y :: A Vec2 -> Type@ becomes
-- @data Point2 x y :: IsA Vec2@
type IsA (foo :: k) = (A foo -> Type :: Type)

-- | An alias to 'IsA'
type IsAn (oo :: k) = (IsA oo :: Type)

-- | An open type family to turn /symbolic/ type representations created with
-- 'A' or 'IsA' into the actual types.
type family Eval (t :: A foo -> Type) :: foo

-- | A type @foo@, @'IsA' foo@.
data Pure (f :: o) :: IsAn o
type instance Eval (Pure f) = f
type Return f = Pure f

-- | A symbolic type-level function.
data (:->) foo bar

-- | An open family of functions from @foo@ to @bar@
type family (f :: IsA (foo :-> bar)) $~ (x :: foo) :: bar

-- | An alias for '$~'
type Apply f x = f $~ x

-- | Symbolic function application
data (:$) :: IsA (foo :-> bar) -> IsA foo -> IsA bar
type instance Eval ((f :: IsA (foo :-> bar)) :$ (x :: IsA foo)) =
  f $~ (Eval x)

-- | Define how a @foo@ could be converted to a @bar@.
type family CoerceTo (bar :: j) (x :: IsA (foo :: k)) :: IsA bar

-- | An alias for 'CoerceTo'.
type (x :: IsA foo) ~~> bar = (CoerceTo bar x :: IsA bar)
infixl 0 ~~>

-- | Either use the value from @Just@ or return a fallback value(types(kinds))
data Optional :: IsA t -> Maybe (IsA s) -> IsA t
type instance Eval (Optional ignored ('Just s) :: IsA dest) = Eval (s ~~> dest)
type instance Eval (Optional t 'Nothing :: IsA dest) = Eval t

-- | Coerce the elements of a list all to a @bar@.
type family CoerceListTo bar (xs :: [IsA foo]) :: [IsA bar] where
  CoerceListTo bar '[] = '[]
  CoerceListTo bar (x ': xs) = CoerceTo bar x ': CoerceListTo bar xs

type ImplOf c = A (c 'MkA) -> Type

data Term
type IsATerm = A Term -> Type

data Base :: IsATerm where
  MkBase :: Base super

data Class :: ImplOf Base where
  MkClass :: Class super

data SubClass ::  ImplOf Class where
  MkSubClass :: SubClass super

type instance Eval (SubClass) = 'MkClass

data SubSubClass :: ImplOf SubClass where
  MkSubSubClass :: SubSubClass super


type instance Eval (SubSubClass) = 'MkSubClass

data WantsBase :: IsA Term -> ImplOf Class -> Type
