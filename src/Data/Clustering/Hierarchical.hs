module Data.Clustering.Hierarchical
    (-- * Dendrogram data type
     Dendrogram(..)
    ,elements
    ,cutAt
     -- * Linkage data type
    ,Linkage(..)
     -- * Generic clustering function
    ,DM.dendrogram
     -- * Functions for specific linkages
    ,DM.singleLinkage
    ,DM.completeLinkage
    ,DM.upgma
    ,DM.fakeAverageLinkage
    ) where

import Data.Clustering.Hierarchical.Internal.Types (Dendrogram(..), Linkage(..))
import qualified Data.Clustering.Hierarchical.Internal.DistanceMatrix as DM

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
cutAt :: Ord d => Dendrogram d a -> d -> [Dendrogram d a]
cutAt dendro threshold = go [] dendro
    where
      go acc x@(Leaf _)                        = x : acc
      go acc x@(Branch d l r) | d <= threshold = x : acc
                              | otherwise      = go (go acc r) l  -- cut!
