module Data.Clustering.Hierarchical
    (Clustered(..)
    ) where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

-- | Data structure for storing hierarchical clusters.
-- Shamelessly inspired by @bio@ package.
data Clustered score datum =
    Leaf datum
  | Branch score (Clustered score datum) (Clustered score datum)
    deriving (Eq, Ord, Show)

instance Functor (Clustered score) where
    fmap f (Leaf d)         = Leaf (f d)
    fmap f (Branch s c1 c2) = Branch s (fmap f c1) (fmap f c2)

instance Foldable (Clustered score) where
    foldMap f (Leaf d)         = f d
    foldMap f (Branch s c1 c2) = foldMap f c1 `mappend` foldMap f c2

instance Traversable (Clustered score) where
    traverse f (Leaf d)         = Leaf <$> f d
    traverse f (Branch s c1 c2) = Branch s <$> traverse f c1 <*> traverse f c2


-- | The linkage type determines how the distance between
-- clusters will be calculated.
data Linkage =
    SingleLinkage   -- ^ The distance between two clusters @c1@
                    -- and @c2@ is the /minimum/ distance between
                    -- an element of @c1@ and an element of @c2@.
  | CompleteLinkage -- ^ The distance between two clusters @c1@
                    -- and @c2@ is the /maximum/ distance between
                    -- an element of @c1@ and an element of @c2@.
  | AverageLinkage  -- ^ The distance between two clusters @c1@
                    -- and @c2@ is the /arithmetic average/
                    -- between the distances of all elements in
                    -- @c1@ to all elements in @c2@.
    deriving (Eq, Ord, Show, Enum)
