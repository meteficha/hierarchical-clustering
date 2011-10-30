module Data.Clustering.Hierarchical
    (-- * Dendrogram data type
     Dendrogram(..)
    ,Distance
    ,elements
    ,cutAt
     -- * Linkage data type
    ,Linkage(..)
     -- * Clustering function
    ,dendrogram
    ) where

import Data.Clustering.Hierarchical.Internal.Types (Dendrogram(..), Linkage(..), Distance)
import qualified Data.Clustering.Hierarchical.Internal.DistanceMatrix as DM
import qualified Data.Clustering.Hierarchical.Internal.Optimal as O

-- | List of elements in a dendrogram.
elements :: Dendrogram a -> [a]
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
--                (Leaf \'A\')
--                (Leaf \'B\'))
--              (Leaf \'C\'))
--            (Leaf \'D\')
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
cutAt :: Dendrogram a -> Distance -> [Dendrogram a]
cutAt dendro threshold = go [] dendro
    where
      go acc x@(Leaf _)                        = x : acc
      go acc x@(Branch d l r) | d <= threshold = x : acc
                              | otherwise      = go (go acc r) l  -- cut!


-- | Calculates a complete, rooted dendrogram for a list of items
-- and a linkage type.  The following are the time and space
-- complexities for each linkage:
--
-- ['SingleLinkage'] /O(n^2)/ time and /O(n)/ space, using the
--   SLINK algorithm.  This algorithm is optimal in both space
--   and time and gives the same answer as the naive algorithm
--   using a distance matrix.
--
-- ['CompleteLinkage'] /O(n^3)/ time and /O(n^2)/ space, using
--   the naive algorithm with a distance matrix.  Use 'CLINK' if
--   you need more performance.
--
-- [Complete linkage with 'CLINK'] /O(n^2)/ time and /O(n)/
--   space, using the CLINK algorithm.  Note that this algorithm
--   doesn't always give the same answer as the naive algorithm
--   using a distance matrix, but it's much faster.
--
-- ['UPGMA'] /O(n^3)/ time and /O(n^2)/ space, using the naive
--   algorithm with a distance matrix.
--
-- ['FakeAverageLinkage'] /O(n^3)/ time and /O(n^2)/ space, using
-- the naive algorithm with a distance matrix.
dendrogram :: Linkage              -- ^ Linkage type to be used.
           -> [a]                  -- ^ Items to be clustered.
           -> (a -> a -> Distance) -- ^ Distance function between items.
           -> Dendrogram a         -- ^ Complete dendrogram.
dendrogram SingleLinkage      = O.singleLinkage
dendrogram CompleteLinkage    = DM.completeLinkage
dendrogram CLINK              = O.completeLinkage
dendrogram UPGMA              = DM.upgma
dendrogram FakeAverageLinkage = DM.fakeAverageLinkage
