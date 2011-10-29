module Data.Clustering.Hierarchical.Internal.DistanceMatrix
    (Cluster(..)
    ,Item
    ,DistMatrix(..)
    ,ClusterDistance
    ,fromDistance
    ,findMin
    ,mergeClusters
    ) where

import qualified Data.IntMap as IM
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Array.ST (STArray, newArray, newListArray, readArray, writeArray)
import Data.List (delete, tails)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)


mkErr :: String -> a
mkErr = error . ("Data.Clustering.Hierarchical.Internal.DistanceMatrix." ++)

-- | Internal (to this package) type used to represent a cluster
-- (of possibly just one element).  The @key@ should be less than
-- or equal to all @more@ elements.
data Cluster = Cluster {key  :: !Item  -- ^ Element used as key.
                       ,more :: [Item] -- ^ Other elements in the cluster.
                       ,size :: !Int   -- ^ At least one, the @key@.
                       }
               deriving (Eq, Ord, Show)

-- | An element of a cluster.
type Item = IM.Key

-- | Creates a singleton cluster.
singleton :: Item -> Cluster
singleton k = Cluster {key = k, more = [], size = 1}

-- | Joins two clusters, returns the 'key' that didn't become
-- 'key' of the new cluster as well.  Clusters are not monoid
-- because we don't have 'mempty'.
merge :: Cluster -> Cluster -> (Cluster, Item)
merge c1 c2 = let (kl,km) = if key c1 < key c2
                            then (key c1, key c2)
                            else (key c2, key c1)
              in (Cluster {key  = kl
                          ,more = km : more c1 ++ more c2
                          ,size = size c1 + size c2}
                 ,km)




-- | A distance matrix.
data DistMatrix s d = DM {matrix   :: STArray s (Item, Item) d
                         ,active   :: STRef   s [Item]
                         ,clusters :: STArray s Item Cluster}


-- | /O(n^2)/ Creates a list of possible combinations between the
-- given elements.
combinations :: [a] -> [(a,a)]
combinations xs = [(a,b) | (a:as) <- tails xs, b <- as]


-- | /O(n^2)/ Constructs a new distance matrix from a distance
-- function and a number @n@ of elements.  Elements will be drawn
-- from @[1..n]@
fromDistance :: Ord d => (Item -> Item -> d) -> Item -> ST s (DistMatrix s d)
fromDistance _ n | n < 2 = mkErr "fromDistance: n < 2 is meaningless"
fromDistance dist n = do
  matrix_ <- newArray ((1,2), (n-1,n)) (mkErr "fromDistance: undef element")
  active_ <- newSTRef [1..n]
  forM_ (combinations [1..n]) $ \x -> writeArray matrix_ x (uncurry dist x)
  clusters_ <- newListArray (1,n) (map singleton [1..n])
  return $ DM {matrix   = matrix_
              ,active   = active_
              ,clusters = clusters_}


-- | /O(n^2)/ Returns the minimum distance of the distance
-- matrix.  The first key given is less than the second key.
findMin :: Ord d => DistMatrix s d -> ST s ((Cluster, Cluster), d)
findMin dm = readSTRef (active dm) >>= go1 . combinations
    where
      matrix_ = matrix dm
      choose b i m' = if m' < snd b then (i, m') else b
      go1 (i:is)   = readArray matrix_ i >>= go2 is . (,) i
      go1 []       = mkErr "findMin: empty DistMatrix"
      go2 i b | i `seq` b `seq` False = undefined
      go2 (i:is) b = readArray matrix_ i >>= go2 is . choose b i
      go2 []     b = do c1 <- readArray (clusters dm) (fst $ fst b)
                        c2 <- readArray (clusters dm) (snd $ fst b)
                        return ((c1, c2), snd b)


-- | Type for functions that calculate distances between
-- clusters.
type ClusterDistance d =
       (Cluster, d)   -- ^ Cluster B1 and distance from A to B1
    -> (Cluster, d)   -- ^ Cluster B2 and distance from A to B2
    -> d              -- ^ Distance from A to (B1 U B2).


-- | /O(n)/ Merges two clusters, returning the new cluster and
-- the new distance matrix.
mergeClusters :: (Ord d)
              => ClusterDistance d
              -> DistMatrix s d
              -> (Cluster, Cluster)
              -> ST s Cluster
mergeClusters cdist (DM matrix_ active_ clusters_) (b1, b2) = do
  let (bu, kl) = b1 `merge` b2
      b1k = key b1
      b2k = key b2
      km  = key bu
      ix i j | i < j     = (i,j)
             | otherwise = (j,i)

  -- Calculate new distances
  activeV <- readSTRef active_
  forM_ activeV $ \k -> when (k `notElem` [b1k, b2k]) $ do
      -- a   <- readArray clusters_ k
      d_a_b1 <- readArray matrix_ $ ix k b1k
      d_a_b2 <- readArray matrix_ $ ix k b2k
      let d = cdist (b1, d_a_b1) (b2, d_a_b2)
      d `seq` writeArray matrix_ (ix k km) d

  -- Save new cluster, invalidate old one
  writeArray clusters_ km bu
  writeArray clusters_ kl $ mkErr "mergeClusters: invalidated"
  writeSTRef active_ $ delete kl activeV

  -- Return new cluster.
  return bu
