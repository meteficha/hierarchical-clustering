module Data.Clustering.Hierarchical
    (-- * Dendrogram data type
     Dendrogram(..)
    ,elements
    ,cutAt
     -- * Linkage data type
    ,Linkage(..)
     -- * Generic clustering function
    ,dendrogram
     -- * Functions for specific linkages
    ,singleLinkage
    ,completeLinkage
    ,upgma
    ,fakeAverageLinkage
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

-- | Data structure for storing hierarchical clusters.  The
-- distance between clusters is stored on the branches.
-- Distances between leafs are the distances between the elements
-- on those leafs, while distances between branches are defined
-- by the linkage used (see 'Linkage').
data Dendrogram d a =
    Leaf a
    -- ^ The leaf contains the item @a@ itself.
  | Branch d (Dendrogram d a) (Dendrogram d a)
    -- ^ Each branch connects two clusters/dendrograms that are
    -- @d@ distance apart.
    deriving (Eq, Ord, Show)

-- | List of elements in a dendrogram.
elements :: Dendrogram d a -> [a]
elements = go []
    where
      go acc (Leaf x)       = x : acc
      go acc (Branch _ l r) = go (go acc r) l

-- | @dendro \`cutAt\` threshold@ cuts the dendrogram @dendro@ at
-- all branches which have distances strictly greater than
-- @threshold@.
--
-- For example, suppose we have
--
-- @
-- dendro = Branch 0.8
--            (Branch 0.5
--              (Branch 0.2
--                (Leaf 'A')
--                (Leaf 'B'))
--              (Leaf 'C'))
--            (Leaf 'D')
-- @
--
-- Then:
--
-- @
-- dendro \`cutAt\` 0.9 == dendro \`cutAt\` 0.8 == [dendro] -- no changes
-- dendro \`cutAt\` 0.7 == dendro \`cutAt\` 0.5 == [Branch 0.5 (Branch 0.2 (Leaf \'A\') (Leaf \'B\')) (Leaf \'C\'), Leaf \'D\']
-- dendro \`cutAt\` 0.4 == dendro \`cutAt\` 0.2 == [Branch 0.2 (Leaf \'A\') (Leaf \'B\'), Leaf \'C\', Leaf \'D\']
-- dendro \`cutAt\` 0.1 == [Leaf \'A\', Leaf \'B\', Leaf \'C\', Leaf \'D\'] -- no branches at all
-- @
cutAt :: Ord d => Dendrogram d a -> d -> [Dendrogram d a]
cutAt dendro threshold = go [] dendro
    where
      go acc x@(Leaf _)                        = x : acc
      go acc x@(Branch d l r) | d <= threshold = x : acc
                              | otherwise      = go (go acc r) l  -- cut!


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


-- Some cluster distances
cdistSingleLinkage      :: Ord d => ClusterDistance d
cdistSingleLinkage      = \_ (_, d1) (_, d2) _ -> d1 `min` d2

cdistCompleteLinkage    :: Ord d => ClusterDistance d
cdistCompleteLinkage    = \_ (_, d1) (_, d2) _ -> d1 `max` d2

cdistUPGMA              :: Fractional d => ClusterDistance d
cdistUPGMA              = \_ (b1,d1) (b2,d2) _ ->
                            let n1 = fromIntegral (size b1)
                                n2 = fromIntegral (size b2)
                            in (n1 * d1 + n2 * d2) / (n1 + n2)

cdistFakeAverageLinkage :: Fractional d => ClusterDistance d
cdistFakeAverageLinkage = \_ (_, d1) (_, d2) _ -> (d1 + d2) / 2


-- | /O(n^3)/ Calculates a complete, rooted dendrogram for a list
-- of items and a linkage type.  If your distance type has an
-- 'Ord' instance but not a 'Fractional' one, then please use
-- specific functions 'singleLinkage' or 'completeLinkage' that
-- have less restrictive types.
dendrogram :: (Ord d, Fractional d)
           => Linkage        -- ^ Linkage type to be used.
           -> [a]            -- ^ Items to be clustered.
           -> (a -> a -> d)  -- ^ Distance function between items.
           -> Dendrogram d a -- ^ Complete dendrogram.
dendrogram linkage = dendrogram' cdist
    where
      cdist = case linkage of
                SingleLinkage      -> cdistSingleLinkage
                CompleteLinkage    -> cdistCompleteLinkage
                FakeAverageLinkage -> cdistFakeAverageLinkage
                UPGMA              -> cdistUPGMA

-- | /O(n^3)/ Like 'dendrogram', but specialized to single
-- linkage (see 'SingleLinkage') which does not require
-- 'Fractional'.
singleLinkage :: Ord d => [a] -> (a -> a -> d) -> Dendrogram d a
singleLinkage = dendrogram' cdistSingleLinkage

-- | /O(n^3)/ Like 'dendrogram', but specialized to complete
-- linkage (see 'CompleteLinkage') which does not require
-- 'Fractional'.
completeLinkage :: Ord d => [a] -> (a -> a -> d) -> Dendrogram d a
completeLinkage = dendrogram' cdistCompleteLinkage

-- | /O(n^3)/ Like 'dendrogram', but specialized to 'UPGMA'.
upgma :: (Fractional d, Ord d) => [a] -> (a -> a -> d) -> Dendrogram d a
upgma = dendrogram' cdistUPGMA

-- | /O(n^3)/ Like 'dendrogram', but specialized to fake average
-- linkage (see 'FakeAverageLinkage').
fakeAverageLinkage :: (Fractional d, Ord d) => [a]
                   -> (a -> a -> d) -> Dendrogram d a
fakeAverageLinkage = dendrogram' cdistFakeAverageLinkage



-- | Worker function to create dendrograms based on a
-- 'ClusterDistance' (and not a 'Linkage').
dendrogram' :: Ord d => ClusterDistance d
            -> [a] -> (a -> a -> d) -> Dendrogram d a
dendrogram' cdist items dist = runST (act ())
    where
      n = length items
      act _noMonomorphismRestrictionPlease = do
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
