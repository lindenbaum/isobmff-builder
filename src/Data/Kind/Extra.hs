{-# LANGUAGE UndecidableInstances #-}
-- | Kind-level utilities to guide and simplify programming at the type level
module Data.Kind.Extra
  ( type A(..)
  , type IsA
  , type IsAn
  , type Return
  , type Pure
  , type Eval
  , type (:->)
  , type Apply
  , type (^$^)
  , type ($~)
  , type (:>>=:)
  , type (:>>>:)
  , type (:^>>>:)
  , type (:>>>^:)
  , type (:^>>>^:)
  , type Extract
  , type Optional
  , type FoldMap
  , type Fun1
  , type Fun2
  , type Fun3
  , type Fun4
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
infixl 0 $~

-- | An alias for '$~'
type Apply f x = f $~ x

-- | Symbolic function application
data (^$^) :: IsA (foo :-> bar) -> IsA foo -> IsA bar
infixl 0 ^$^
type instance Eval ((f :: IsA (foo :-> bar)) ^$^ (x :: IsA foo)) =
  f $~ (Eval x)

-- | Compose functions
data (:>>>:) :: IsA (good :-> better) -> IsA (better :-> best) -> IsA (good :-> best)
infixl 1 :>>>:
type instance (f :>>>: g) $~ x = g $~ (f $~ x)

-- | Eval Input & Compose
data (:^>>>:) :: IsA (good :-> better) -> IsA (better :-> best) -> IsA (IsA good :-> best)
infixl 1 :^>>>:
type instance (f :^>>>: g) $~ x = g $~ (f $~ Eval x)

-- | Compose and 'Return'
data (:>>>^:) :: IsA (good :-> better) -> IsA (better :-> best) -> IsA (good :-> IsA best)
infixl 1 :>>>^:
type instance (f :>>>^: g) $~ x = Return (g $~ (f $~ x))

-- | Eval compose and return
data (:^>>>^:) :: IsA (good :-> better) -> IsA (better :-> best) -> IsA (IsA good :-> IsA best)
infixl 1 :^>>>^:
type instance (f :^>>>^: g) $~ x = f :>>>: g ^$^ x

-- | A function that applies 'Eval'
data Extract :: IsA (IsA x :-> x)
type instance Extract $~ x = Eval x

-- | Eval and ApplyCompose functions
data (:>>=:) :: IsA foo -> IsA (foo :-> IsA bar) -> IsA bar
infixl 1 :>>=:
type instance Eval (x :>>=: f) = Eval (f $~ Eval x)

-- | Either use the value from @Just@ or return a fallback value(types(kinds))
data Optional :: IsA t -> IsA (s :-> IsA t) -> IsA (Maybe s :-> IsA t)

type instance (Optional fallback f) $~ ('Just s) = f $~ s
type instance (Optional fallback f) $~ 'Nothing = fallback

-- | Coerce the elements of a list all to a @bar@.
type family
  FoldMap
          (append :: IsA (bar :-> IsA (bar :-> bar)))
          (zero :: bar)
          (f :: IsA (foo :-> bar))
          (xs :: [(foo :: Type)]) :: (bar :: Type) where
  FoldMap append zero f '[]       = zero
  FoldMap append zero f (x ': xs) = append $~ (f $~ x) $~ FoldMap append zero f xs


--  TODONT safe coercions (with undecidable instances) could be done only with a
--  type-level equivilant of a type class dictionary, A good place for that
--  might be 'A'. For now I only had trouble with 'CoerceTo' because it is open
--  and the compiler often used up __all__ main memory when an instance was
--  missing.

-- | Like @TyCon1@ from Data.Singletons
data Fun1 :: (a -> IsA b)
            -> IsA (a :-> IsA b)
type instance (Fun1 f) $~ x = (f x)

data Fun2 :: (a -> b -> IsA c)
            -> IsA (a :-> IsA (b :-> IsA c))
type instance (Fun2 f) $~ x = Fun1 (f x)

data Fun3 :: (a -> b -> c -> IsA d)
            -> IsA (a :-> IsA (b :-> IsA (c :-> IsA d)))
type instance (Fun3 f) $~ x = Fun2 (f x)

data Fun4 :: (a -> b -> c -> d -> IsAn e)
            -> IsA (a :-> IsA (b :-> IsA (c :-> IsA (d :-> IsAn e))))
type instance (Fun4 f) $~ x = Fun3 (f x)
