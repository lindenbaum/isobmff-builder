-- | Brand/Box-validation
module Data.ByteString.IsoBaseFileFormat.Brands.Types
        where

import Data.ByteString.IsoBaseFileFormat.Boxes.Box
import Data.ByteString.IsoBaseFileFormat.Util.TypeLayout ()

-- | A class that describes (on the type level) how a box can be nested into
-- other boxes (see 'Boxes).
class IsBrand brand where
  -- | The layout that an IsBoxContent instance has to have, before 'packMedia' accepts it
  type BoxLayout brand
  mediaBuilder
    :: forall t proxy . (IsBoxContent t, IsRuleConform t (BoxLayout brand) ~ 'True)
    => proxy brand -> t -> Builder
  mediaBuilder _ t = boxBuilder t
