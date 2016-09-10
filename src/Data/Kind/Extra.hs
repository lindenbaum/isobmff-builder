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
  , type FromA
  -- , type CoercionTo
  -- , type As
  -- , type (-->)
  , type Optional
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
data A :: forall foo . foo -> Type

-- | Type alias for 'A' such that @data Point2 x y :: A Vec2 -> Type@ becomes
-- @data Point2 x y :: IsA Vec2@
type IsA (foo :: k) = (A foo -> Type :: Type)

-- | An alias to 'IsA'
type IsAn (oo :: k) = (IsA oo :: Type)

-- | An open type family to turn /symbolic/ type representations created with
-- 'A' or 'IsA' into the actual types.
type family Eval (t :: IsA foo) :: foo

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
type family CoerceTo bar (x :: foo) :: bar
type instance CoerceTo (IsA bar) (x :: IsA foo) = Return (CoerceTo bar (Eval x))

-- | Automatic coercion to a symbolic type inferred from the context.
data FromA (x :: IsA foo) :: IsA bar
type instance Eval (FromA (x::IsA foo) :: IsA bar) = CoerceTo bar (Eval x)

-- | An alias for 'CoerceTo'.
type (x :: foo) ~~> bar = (CoerceTo bar x :: bar)
infixl 0 ~~>
-- TODO probably dead code:
-- -- | Symbolic coercion of type @foo@ to __kind__ @bar@ via the `CoerceTo` type
-- -- family.
-- --
-- -- @
-- -- data Foo (n :: Nat)
-- -- data Bar = MkBar Nat
-- --
-- -- type instance CoerceTo Bar (Foo n) = 'MkBar n
-- --
-- -- type Fun1As (x :: Foo Nat) = Fun2 (x `As` Bar)
-- -- type Fun1Op (x :: Foo Nat) = Fun2 (x --> Bar)
-- -- type Fun1   (x :: Foo Nat) = Fun2 (CoercionTo Bar $~ x)
-- --
-- -- type Fun2 (x :: Bar)     = ...
-- -- @
-- --
-- data CoercionTo (bar :: Type) :: forall (foo :: Type) . IsA (foo :-> bar)
-- type instance CoercionTo bar $~ foo = CoerceTo bar foo

-- -- | Alias for the application of a 'CoercionTo'
-- type As (x :: foo) bar = (CoercionTo bar :: IsA (foo :-> bar)) $~ x

-- -- | Alias for the application of a 'CoercionTo'
-- type (x :: foo) --> bar = (CoercionTo bar :: IsA (foo :-> bar)) $~ x

-- infixl 0 -->

-- | Either use the value from @Just@ or return a fallback value(types(kinds))
data Optional :: IsA t -> Maybe s -> IsA t
type instance Eval (Optional ignored ('Just s) :: IsA dest) = s ~~> dest
type instance Eval (Optional t 'Nothing :: IsA dest) = Eval t
