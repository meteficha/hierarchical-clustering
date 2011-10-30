{-# LANGUAGE BangPatterns #-}

module Data.Clustering.Hierarchical.Internal.DistanceMatrix
    (dendrogram
    ,singleLinkage
    ,completeLinkage
    ,upgma
    ,fakeAverageLinkage
    ) where

-- from base
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array (listArray, (!))
import Data.Array.ST (STArray, newArray, newListArray, readArray, writeArray)
import Data.Function (on)
import Data.List (delete, tails, (\\))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- from containers
import qualified Data.IntMap as IM

-- from this package
import Data.Clustering.Hierarchical.Internal.Types

mkErr :: String -> a
mkErr = error . ("Data.Clustering.Hierarchical.Internal.DistanceMatrix." ++)

-- | Internal (to this package) type used to represent a cluster
-- (of possibly just one element).  The @key@ should be less than
-- or equal to all elements of the cluster.
data Cluster = Cluster { key  :: {-# UNPACK #-} !Item  -- ^ Element used as key.
                       , size :: {-# UNPACK #-} !Int   -- ^ At least one, the @key@.
                       }
               deriving (Eq, Ord, Show)

-- | An element of a cluster.
type Item = IM.Key

-- | Creates a singleton cluster.
singleton :: Item -> Cluster
singleton k = Cluster {key = k, size = 1}

-- | Joins two clusters, returns the 'key' that didn't become
-- 'key' of the new cluster as well.  Clusters are not monoid
-- because we don't have 'mempty'.
merge :: Cluster -> Cluster -> (Cluster, Item)
merge c1 c2 = let (kl,km) = if key c1 < key c2
                            then (key c1, key c2)
                            else (key c2, key c1)
              in (Cluster {key  = kl
                          ,size = size c1 + size c2}
                 ,km)




-- | A distance matrix.
data DistMatrix s d =
    DM { matrix   :: {-# UNPACK #-} !(STArray s (Item, Item) d)
       , active   :: {-# UNPACK #-} !(STRef   s [Item])
       , clusters :: {-# UNPACK #-} !(STArray s Item Cluster)
       }


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
findMin dm = readSTRef (active dm) >>= go1
    where
      matrix_ = matrix dm
      choose b i m' = if m' < snd b then (i, m') else b

      go1 is@(i1:i2:_) = do di <- readArray matrix_ (i1, i2) -- initial
                            ((b1, b2), d) <- go2 is ((i1, i2), di)
                            c1 <- readArray (clusters dm) b1
                            c2 <- readArray (clusters dm) b2
                            return ((c1, c2), d)
      go1 _            = mkErr "findMin: empty DistMatrix"

      go2 (i1:is@(_:_)) !b = go3 i1 is b >>= go2 is
      go2 _              b = return b

      go3 i1 (i2:is) !b = readArray matrix_ (i1,i2) >>= go3 i1 is . choose b (i1,i2)
      go3 _  []       b = return b



-- | Type for functions that calculate distances between
-- clusters.
type ClusterDistance d =
       (Cluster, d)   -- ^ Cluster B1 and distance from A to B1
    -> (Cluster, d)   -- ^ Cluster B2 and distance from A to B2
    -> d              -- ^ Distance from A to (B1 U B2).


-- Some cluster distances
cdistSingleLinkage      :: Ord d => ClusterDistance d
cdistSingleLinkage      = \(_, d1) (_, d2) -> d1 `min` d2

cdistCompleteLinkage    :: Ord d => ClusterDistance d
cdistCompleteLinkage    = \(_, d1) (_, d2) -> d1 `max` d2

cdistUPGMA              :: Fractional d => ClusterDistance d
cdistUPGMA              = \(b1,d1) (b2,d2) ->
                            let n1 = fromIntegral (size b1)
                                n2 = fromIntegral (size b2)
                            in (n1 * d1 + n2 * d2) / (n1 + n2)

cdistFakeAverageLinkage :: Fractional d => ClusterDistance d
cdistFakeAverageLinkage = \(_, d1) (_, d2) -> (d1 + d2) / 2



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
  forM_ (activeV \\ [b1k, b2k]) $ \k -> do
      -- a   <- readArray clusters_ k
      d_a_b1 <- readArray matrix_ $ ix k b1k
      d_a_b2 <- readArray matrix_ $ ix k b2k
      let d = cdist (b1, d_a_b1) (b2, d_a_b2)
      writeArray matrix_ (ix k km) $! d

  -- Save new cluster, invalidate old one
  writeArray clusters_ km bu
  writeArray clusters_ kl $ mkErr "mergeClusters: invalidated"
  writeSTRef active_ $ delete kl activeV

  -- Return new cluster.
  return bu


-- | Worker function to create dendrograms based on a
-- 'ClusterDistance'.
dendrogram' :: Ord d => ClusterDistance d
            -> [a] -> (a -> a -> d) -> Dendrogram d a
dendrogram' _ []  _ = mkErr "dendrogram': empty input list"
dendrogram' _ [x] _ = Leaf x
dendrogram' cdist items dist = runST (act ())
    where
      n = length items
      act _noMonomorphismRestrictionPlease = do
        let xs = listArray (1, n) items
            im = IM.fromDistinctAscList $ zip [1..] $ map Leaf items
        fromDistance (dist `on` (xs !)) n >>= go (n-1) im
      go !i !ds !dm = do
        ((c1,c2), distance) <- findMin dm
        cu <- mergeClusters cdist dm (c1,c2)
        let dendro c = IM.updateLookupWithKey (\_ _ -> Nothing) (key c)
            (Just d1, !ds')  = dendro c1 ds
            (Just d2, !ds'') = dendro c2 ds'
            du = Branch distance d1 d2
        case i of
          1 -> return du
          _ -> let !ds''' = IM.insert (key cu) du ds''
               in du `seq` go (i-1) ds''' dm


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
