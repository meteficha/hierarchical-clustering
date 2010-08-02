module Data.Clustering.Hierarchical
    (Dendrogram(..)
    ,Linkage(..)
    ) where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

-- | Data structure for storing hierarchical clusters.
data Dendrogram score datum =
    Leaf datum
  | Branch score (Dendrogram score datum) (Dendrogram score datum)
    deriving (Eq, Ord, Show)

-- | Does not recalculate the scores!
instance Functor (Dendrogram score) where
    fmap f (Leaf d)         = Leaf (f d)
    fmap f (Branch s c1 c2) = Branch s (fmap f c1) (fmap f c2)

instance Foldable (Dendrogram score) where
    foldMap f (Leaf d)         = f d
    foldMap f (Branch _ c1 c2) = foldMap f c1 `mappend` foldMap f c2

instance Traversable (Dendrogram score) where
    traverse f (Leaf d)         = Leaf <$> f d
    traverse f (Branch s c1 c2) = Branch s <$> traverse f c1 <*> traverse f c2


-- | The linkage type determines how the distance between
-- clusters will be calculated.
data Linkage =
    SingleLinkage
  -- ^ The distance between two clusters @a@ and @b@ is the
  -- /minimum/ distance between an element of @a@ and an element
  -- of @b@.
  | CompleteLinkage
  -- ^ The distance between two clusters @a@ and @b@ is the
  -- /maximum/ distance between an element of @a@ and an element
  -- of @b@.
  | UPGMA
  -- ^ Unweighted Pair Group Method with Arithmetic mean, also
  -- called \"average linkage\".  The distance between two
  -- clusters @a@ and @b@ is the /arithmetic average/ between the
  -- distances of all elements in @a@ to all elements in @b@.
  | FakeAverageLinkage
  -- ^ This method is usually wrongly called \"average linkage\".
  -- The distance between cluster @a = a1 U a2@ (that is, cluster
  -- @a@ was formed by the linkage of clusters @a1@ and @a2@) and
  -- an old cluster @b@ is @(d(a1,b) + d(a2,b)) / 2@.  So when
  -- clustering two elements to create a cluster, this method is
  -- the same as UPGMA.  However, in general when joining two
  -- clusters this method assigns equal weights to @a1@ and @a2@,
  -- while UPGMA assigns weights proportional to the number of
  -- elements in each cluster.  See, for example:
  --
  -- *
  -- <http://www.cs.tau.ac.il/~rshamir/algmb/00/scribe00/html/lec08/node21.html>,
  -- which defines the real UPGMA and gives the equation to
  -- calculate the distance between an old and a new cluster.
  --
  -- *
  -- <http://github.com/JadeFerret/ai4r/blob/master/lib/ai4r/clusterers/average_linkage.rb>,
  -- code for \"average linkage\" on ai4r library implementing
  -- what we call here @FakeAverageLinkage@ and not UPGMA.
    deriving (Eq, Ord, Show, Enum)
