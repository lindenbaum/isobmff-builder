module KindsExtraSpec (spec) where

import Data.Type.Pretty
import GHC.TypeLits
import Data.Kind.Extra
import Test.HSpec

spec :: HSpec
spec =
  describe "Examples of KindsExtraSpec"
     it "reenders a test dog" $
        showPretty (Proxy @(DogToPretty Lessy))

-- ** Examples

data Animal where
  MkAnimal :: PrettyType -> Animal

type instance Eval ('MkAnimal info ~~> PrettyType) =
  "Got an animal" <:$$--> info

type instance Eval ('MkMammel i ~~> Animal) =
  'MkAnimal ("Specifically a Mammal" <:$$--> i)

data Mammal where
  MkMammel :: PrettyType -> Mammal

type instance Eval ('MkDog name ~~> Mammal) =
  'MkMammel ("A dog named" <:> PutStr name)

data Dog where
  MkDog :: Symbol -> Dog

data Lessy :: IsA Dog
type instance Eval Lessy = 'MkDog "Lessy!"

type DogToAnimal d =  d --> Mammal -->| Animal
type DogToPretty d = d --> Mammal --> Animal -->| PrettyType
