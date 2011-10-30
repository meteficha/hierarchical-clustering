module Data.Clustering.Hierarchical.Internal.Types
    ( Dendrogram(..)
    , Linkage(..)
    , Distance
    ) where

-- from base
import Control.Applicative ((<$>), (<*>))
import Data.Foldable (Foldable (..))
import Data.Monoid (mappend)
import Data.Traversable (Traversable(..))

-- | Data structure for storing hierarchical clusters.  The
-- distance between clusters is stored on the branches.
-- Distances between leafs are the distances between the elements
-- on those leafs, while distances between branches are defined
-- by the linkage used (see 'Linkage').
data Dendrogram a =
    Leaf a
    -- ^ The leaf contains the item @a@ itself.
  | Branch {-# UNPACK #-} !Distance (Dendrogram a) (Dendrogram a)
    -- ^ Each branch connects two clusters/dendrograms that are
    -- @d@ distance apart.
    deriving (Eq, Ord, Show)

-- | A distance is simply a synonym of 'Double' for efficiency.
type Distance = Double

-- | Does not recalculate the distances!
instance Functor Dendrogram where
    fmap f (Leaf d)         = Leaf (f d)
    fmap f (Branch s c1 c2) = Branch s (fmap f c1) (fmap f c2)

instance Foldable Dendrogram where
    foldMap f (Leaf d)         = f d
    foldMap f (Branch _ c1 c2) = foldMap f c1 `mappend` foldMap f c2

instance Traversable Dendrogram where
    traverse f (Leaf d)         = Leaf <$> f d
    traverse f (Branch s c1 c2) = Branch s <$> traverse f c1 <*> traverse f c2


-- | The linkage type determines how the distance between
-- clusters will be calculated.  These are the linkage types
-- currently available on this library.
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
