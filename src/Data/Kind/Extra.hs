{-# LANGUAGE UndecidableInstances #-}
-- | Kind-level utilities to guide and simplify programming at the type level
module Data.Kind.Extra
  ( type IsA
  , type IsAn
  , type Generates
  , type Eval
  , type FromA
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

-- | An alias to 'IsA'
type Generates foo = IsA foo

-- | A type family for generating the (promoted) types of a phantom data
--  type(kind) from other data types that have a kind that /ends/ in e.g.
--  @'Generates' Foo@.
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
type family Eval (t :: Generates foo) :: foo

-- | Convert something that 'Generates'  @foo@ to something that 'Generates' @bar@
data FromA foo :: KProxy bar -> Generates bar

-- | Alias for 'FromA' that lifts the burden of creating a 'KProxy' from the
-- caller.
type (~~>) foo bar = FromA foo ('KProxy :: KProxy bar)
infixl 3 ~~>
-- | Alias for 'Eval' that evaluates the first parameter of 'FromA' and '~~>'
-- respectively.
type (-->) foo bar = FromA (Eval foo) ('KProxy :: KProxy bar)
infixl 2 -->

-- | Alias for 'Eval' that evaluates '-->'. This is the evalutated full
-- conversion from @IsA foo@ to @IsA bar@, i.e. @bar@ in this example..
type (-->|) foo bar = Eval (FromA (Eval foo) ('KProxy :: KProxy bar))
infixl 1 -->|

-- ** Standard Types

-- | Either use the value from @Just@ or return a fallback value(types(kinds))
data FromMaybe :: IsAn x -> Maybe x -> IsAn x
type instance Eval (FromMaybe fallback ('Just t)) = t
type instance Eval (FromMaybe fallback 'Nothing) = Eval fallback
