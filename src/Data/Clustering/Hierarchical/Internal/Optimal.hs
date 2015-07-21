{-# LANGUAGE BangPatterns, FlexibleContexts #-}

-- | Implementations that are optimal in space and time.
module Data.Clustering.Hierarchical.Internal.Optimal
    ( singleLinkage
    , completeLinkage
    ) where

-- from base
import Prelude hiding (pi)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (forM_, liftM3, when)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, listArray, (!))
import Data.Array.ST (STUArray, newArray_, newListArray,
                      readArray, writeArray,
                      getElems, getBounds) -- getAssocs
import Data.List (sortBy)
import Data.Maybe (fromMaybe)

-- from containers
import qualified Data.IntMap as IM

-- from this package
import Data.Clustering.Hierarchical.Internal.Types


mkErr :: String -> a
mkErr = error . ("Data.Clustering.Hierarchical.Internal.Optimal." ++)


type Index = Int

data PointerRepresentation s a =
  PR { pi     :: {-# UNPACK #-} !(STUArray s Index Index)
     , lambda :: {-# UNPACK #-} !(STUArray s Index Distance)
     , em     :: {-# UNPACK #-} !(STUArray s Index Distance)
     , elm    :: {-# UNPACK #-} !(Array Index a)
     }

-- debugPR :: Show a => PointerRepresentation s a -> ST s String
-- debugPR pr = do
--   pis     <- getAssocs (pi pr)
--   lambdas <- getAssocs (lambda pr)
--   ems     <- getAssocs (em pr)
--   return $ unlines [ "pi     = " ++ show pis
--                    , "lambda = " ++ show lambdas
--                    , "em     = " ++ show ems
--                    , "elm    = " ++ show (elm pr)
--                    ]

initPR :: Index -> Array Index a -> ST s (PointerRepresentation s a)
initPR n xs' = ($ xs') <$> liftM3 PR (newArray_ (1, n)) (newArray_ (1, n)) (newArray_ (1, n))

indexDistance :: [a] -> (a -> a -> Distance)
              -> (Index, Array Index a, Index -> Index -> Distance)
indexDistance xs dist = (n, xs', dist')
    where
      !n = length xs
      !xs' = listArray (1, n) xs
      dist' i j = dist (xs' ! i) (xs' ! j)


infinity :: Distance
infinity = 1 / 0


-- | /O(n^2)/ time and /O(n)/ space.  See 'singleLinkage' on this module.
slink :: [a] -> (a -> a -> Distance) -> ST s (PointerRepresentation s a)
slink xs dist = initPR n xs' >>= go 1
    where
      (n, xs', dist') = indexDistance xs dist

      go !i !pr | i == n + 1 = return pr
                | otherwise  = do
        writeArray (pi pr)     i i
        writeArray (lambda pr) i infinity
        forM_ [1..i-1] $ \j ->
          writeArray (em pr) j (dist' j i)
        forM_ [1..i-1] $ \j -> do
          lambda_j <- readArray (lambda pr) j
          em_j     <- readArray (em pr)     j
          pi_j     <- readArray (pi pr)     j
          em_pi_j  <- readArray (em pr)     pi_j
          if lambda_j >= em_j then do
            writeArray (em pr)     pi_j (em_pi_j `min` lambda_j)
            writeArray (lambda pr) j    em_j
            writeArray (pi pr)     j    i
           else
            writeArray (em pr)     pi_j (em_pi_j `min` em_j)
        forM_ [1..i-1] $ \j -> do
          pi_j        <- readArray (pi pr)     j
          lambda_j    <- readArray (lambda pr) j
          lambda_pi_j <- readArray (lambda pr) pi_j
          when (lambda_j >= lambda_pi_j) $
            writeArray (pi pr) j i
        go (i+1) pr


-- | /O(n^2)/ time and /O(n)/ space. See 'completeLinkage' on this module.
clink :: [a] -> (a -> a -> Distance) -> ST s (PointerRepresentation s a)
clink xs dist = initPR n xs' >>= go 1
    where
      (n, xs', dist') = indexDistance xs dist

      go !i !pr | i == n + 1 = return pr
                | i == 1     = do writeArray (pi pr)     1 1
                                  writeArray (lambda pr) 1 infinity
                                  go 2 pr
                | otherwise  = do
        -- First part
        writeArray (pi pr)     i i
        writeArray (lambda pr) i infinity
        forM_ [1..i-1] $ \j ->
          writeArray (em pr) j (dist' j i)
        forM_ [1..i-1] $ \j -> do
          lambda_j <- readArray (lambda pr) j
          em_j     <- readArray (em pr)     j
          when (lambda_j < em_j) $ do
            pi_j     <- readArray (pi pr)     j
            em_pi_j  <- readArray (em pr)     pi_j
            writeArray (em pr) pi_j (em_pi_j `max` em_j)
            writeArray (em pr) j    infinity

        -- Loop a
        a <- readArray (em pr) (i-1) >>= go_a_loop (i-1) pr (i-1)

        -- Loop b
        b <- readArray (pi pr)     a
        c <- readArray (lambda pr) a
        writeArray (pi pr)     a i
        writeArray (lambda pr) a =<< readArray (em pr) a
        go_b_loop i pr a b c

        -- Final part
        forM_ [1..i-1] $ \j -> do
          pi_j    <- readArray (pi pr) j
          pi_pi_j <- readArray (pi pr) pi_j
          when (pi_pi_j == i) $ do
            lambda_j    <- readArray (lambda pr) j
            lambda_pi_j <- readArray (lambda pr) pi_j
            when (lambda_j >= lambda_pi_j) $
              writeArray (pi pr) j i

        -- Recurse
        go (i+1) pr

      -- Loop a's core
      go_a_loop 0 _ a _ = return a
      go_a_loop !j !pr !a !em_a = do
        pi_j     <- readArray (pi pr)     j
        lambda_j <- readArray (lambda pr) j
        em_pi_j  <- readArray (em pr)     pi_j
        if lambda_j >= em_pi_j then do
          em_j <- readArray (em pr) j
          if em_j < em_a then
            go_a_loop (j-1) pr j em_j
           else
            go_a_loop (j-1) pr a em_a
         else do
          writeArray (em pr) j infinity
          go_a_loop (j-1) pr a em_a

      -- Loop b's core
      go_b_loop !i !pr !a !b !c
          | a >= i - 1 = return ()
          | b <  i - 1 = do pi_b     <- readArray (pi pr)     b
                            lambda_b <- readArray (lambda pr) b
                            writeArray (pi pr)     b i
                            writeArray (lambda pr) b c
                            go_b_loop i pr a pi_b lambda_b
          | otherwise  = do writeArray (pi pr)     b i
                            writeArray (lambda pr) b c
                            return ()


-- | /O(n log n)/ time and /O(n)/ space. Construct a 'Dendrogram'
-- from a 'PointerRepresentation'.
buildDendrogram :: PointerRepresentation s a
                -> ST s (Dendrogram a)
buildDendrogram pr = do
  (1,n) <- getBounds (lambda pr)
  lambdas <- getElems (lambda pr)
  pis     <- getElems (pi pr)
  let sorted = sortBy (\(_,l1,_) (_,l2,_) -> l1 `compare` l2) $
               zip3 [1..] lambdas pis
  index <- newListArray (1,n) [1..]
  let go im [] =
        case IM.toList im of
          [(_,x)] -> return x
          _       -> mkErr "buildDendrogram: final never here"
      go im ((i, (j,lambda_j,pi_j)):rest) = do
        left_i  <- readArray index j
        right_i <- readArray index pi_j
        writeArray (index `asTypeOf` pi pr) pi_j (negate i)
        let (left,  im')  | left_i > 0  = (Leaf $ elm pr ! left_i, im)
                          | otherwise   = first (fromMaybe e1) $
                                          IM.updateLookupWithKey (\_ _ -> Nothing) ix im
                          where ix = negate left_i
            (right, im'') | right_i > 0 = (Leaf $ elm pr ! right_i, im')
                          | otherwise   = first (fromMaybe e2) $
                                          IM.updateLookupWithKey (\_ _ -> Nothing) ix im'
                          where ix = negate right_i
            im''' = IM.insert i (Branch lambda_j left right) im''
            e1 = mkErr "buildDendrogram: never here 1"
            e2 = mkErr "buildDendrogram: never here 2"
        go im''' rest
  go IM.empty (zip [1..n-1] sorted)


-- | /O(n^2)/ time and /O(n)/ space. Calculates a complete,
-- rooted dendrogram for a list of items using single linkage
-- with the SLINK algorithm.  This algorithm is optimal in space
-- and time.
--
-- [Reference] R. Sibson (1973). \"SLINK: an optimally efficient
--   algorithm for the single-link cluster method\". /The/
--   /Computer Journal/ (British Computer Society) 16 (1):
--   30-34.
singleLinkage :: [a] -> (a -> a -> Distance) -> Dendrogram a
singleLinkage []  _   = mkErr "singleLinkage: empty input"
singleLinkage [x] _   = Leaf x
singleLinkage xs dist = runST (slink xs dist >>= buildDendrogram)


-- | /O(n^2)/ time and /O(n)/ space. Calculates a complete, rooted dendrogram for a list
-- of items using complete linkage with the CLINK algorithm.  This
-- algorithm is optimal in space and time.
--
-- [Reference] D. Defays (1977). \"An efficient algorithm for a
--   complete link method\". /The Computer Journal/ (British
--   Computer Society) 20 (4): 364-366.
completeLinkage :: [a] -> (a -> a -> Distance) -> Dendrogram a
completeLinkage []  _   = mkErr "completeLinkage: empty input"
completeLinkage [x] _   = Leaf x
completeLinkage xs dist = runST (clink xs dist >>= buildDendrogram)
