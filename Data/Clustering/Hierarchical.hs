module Data.Clustering.Hierarchical
    (Dendrogram(..)
    ,Linkage(..)
    ,completeDendrogram
    ) where

import qualified Data.IntMap as IM
import Control.Applicative ((<$>), (<*>))
import Control.Monad.ST (runST)
import Data.Array (listArray, (!))
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.Monoid (mappend)
import Data.Traversable (Traversable(..))

import Data.Clustering.Hierarchical.Internal.DistanceMatrix

-- | Data structure for storing hierarchical clusters.
data Dendrogram d a =
    Leaf a
    -- ^ The leaf contains the item @a@ itself.
  | Branch d (Dendrogram d a) (Dendrogram d a)
    -- ^ Each branch connects two clusters/dendrograms that are
    -- @d@ distance apart.
    deriving (Eq, Ord, Show)

-- | Does not recalculate the distances!
instance Functor (Dendrogram d) where
    fmap f (Leaf d)         = Leaf (f d)
    fmap f (Branch s c1 c2) = Branch s (fmap f c1) (fmap f c2)

instance Foldable (Dendrogram d) where
    foldMap f (Leaf d)         = f d
    foldMap f (Branch _ c1 c2) = foldMap f c1 `mappend` foldMap f c2

instance Traversable (Dendrogram d) where
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


-- | Calculates distances between clusters according to the
-- chosen linkage.
clusterDistance :: (Fractional d, Ord d) => Linkage -> ClusterDistance d
clusterDistance SingleLinkage      = \_ (_, d1) (_, d2) _ -> d1 `min` d2
clusterDistance CompleteLinkage    = \_ (_, d1) (_, d2) _ -> d1 `max` d2
clusterDistance FakeAverageLinkage = \_ (_, d1) (_, d2) _ -> (d1 + d2) / 2
clusterDistance UPGMA              = \_ (b1,d1) (b2,d2) _ ->
                                       let n1 = fromIntegral (size b1)
                                           n2 = fromIntegral (size b2)
                                       in (n1 * d1 + n2 * d2) / (n1 + n2)


-- | Calculates a complete, rooted dendrogram for a list of items and a distance
-- function.
completeDendrogram :: (Fractional d, Ord d) => Linkage ->
                      [a] -> (a -> a -> d) -> Dendrogram d a
completeDendrogram linkage items dist = runST (act ())
    where
      n     = length items
      cdist = clusterDistance linkage
      act _ = do
        let xs = listArray (1, n) items
        fromDistance (dist `on` (xs !)) n >>= go xs (n-1) IM.empty
      go xs i ds dm = do
        ((c1,c2), distance) <- findMin dm
        cu <- mergeClusters cdist dm (c1,c2)
        let dendro c = case size c of
                         1 -> Leaf (xs ! key c)
                         _ -> ds IM.! key c
            d1 = dendro c1
            d2 = dendro c2
            du = Branch distance d1 d2
        case i of
          1 -> return du
          _ -> let ds' = IM.insert (key cu) du $
                         IM.delete (key c1) $
                         IM.delete (key c2) ds
               in go xs (i-1) ds' dm
