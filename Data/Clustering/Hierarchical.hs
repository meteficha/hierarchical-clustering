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


