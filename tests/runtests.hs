{-# LANGUAGE Rank2Types #-}

-- from base
import qualified Control.Exception as E
import Control.Monad (when)
import Data.List (delete, sort, nub)
import Text.Printf (printf)
import Text.Show.Functions ()

-- from hspec
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)

-- from HUnit
import Test.HUnit

-- from QuickCheck
import Test.QuickCheck ((==>), Property)

-- from this package
import Data.Clustering.Hierarchical
import qualified Data.Clustering.Hierarchical.Internal.DistanceMatrix as DM
import qualified Data.Clustering.Hierarchical.Internal.Optimal as O


main :: IO ()
main = hspecX $ do
         test_cutAt
         test_dendrogram

test_cutAt :: Specs
test_cutAt =
    describe "cutAt" $ do
      let dendro      :: Dendrogram Char
          dendro      = Branch 0.8 d_0_8_left d_0_8_right
          d_0_8_left  =   Branch 0.5 d_0_5_left d_0_5_right
          d_0_5_left  =     Branch 0.2 d_0_2_left d_0_2_right
          d_0_2_left  =       Leaf 'A'
          d_0_2_right =       Leaf 'B'
          d_0_5_right =     Leaf 'C'
          d_0_8_right =   Leaf 'D'

      let testFor threshold expected =
              it (printf "works for 'dendro' with threshold %0.1f" threshold) $
                 dendro `cutAt` threshold ~?= expected

      testFor 0.9 [dendro]
      testFor 0.8 [dendro]
      testFor 0.7 [d_0_8_left, d_0_8_right]
      testFor 0.5 [d_0_8_left, d_0_8_right]
      testFor 0.4 [d_0_5_left, d_0_5_right, d_0_8_right]
      testFor 0.2 [d_0_5_left, d_0_5_right, d_0_8_right]
      testFor 0.1 [d_0_2_left, d_0_2_right, d_0_5_right, d_0_8_right]

test_dendrogram :: Specs
test_dendrogram = do
    describe "Optimal's singleLinkage" $ do
      basicDendrogramTests O.singleLinkage
      prop "really is single linkage" $
        propCorrectLinkage O.singleLinkage singleLink

    describe "Optimal's completeLinkage" $ do
      basicDendrogramTests O.completeLinkage
      prop "really is complete linkage" $
        propCorrectLinkage O.completeLinkage completeLink

    describe "DistanceMatrix's singleLinkage" $ do
      basicDendrogramTests DM.singleLinkage
      prop "really is single linkage" $
        propCorrectLinkage DM.singleLinkage singleLink

    describe "DistanceMatrix's completeLinkage" $ do
      basicDendrogramTests DM.completeLinkage
      prop "really is complete linkage" $
        propCorrectLinkage DM.completeLinkage completeLink

    describe "DistanceMatrix's upgma" $ do
      basicDendrogramTests DM.upgma
      prop "really is UPGMA" $
        propCorrectLinkage DM.upgma upgma

    describe "DistanceMatrix's fakeAverageLinkage" $ do
      basicDendrogramTests DM.fakeAverageLinkage

    describe "Optimal and DistanceMatrix" $ do
      let test f1 f2 = \ps -> length ps >= 2 ==>
                              f1 ps euclideanDist ==== f2 ps euclideanDist
      prop "agree on singleLinkage"   $ test O.singleLinkage   DM.singleLinkage
      prop "agree on completeLinkage" $ test O.completeLinkage DM.completeLinkage


basicDendrogramTests :: (forall a. [a] -> (a -> a -> Distance) -> Dendrogram a) -> Specs
basicDendrogramTests f = do
  it "fails for an empty input" $
     assertErrors (f [] (\_ _ -> zero))
  it "works for one element" $
     Leaf () == f [()] undefined
  prop "always returns the elements we gave" $
     \points ->
         not (null points) ==>
         elements (f points euclideanDist) `isPermutationOf` points
  prop "works for examples where all elements have the same distance" $
     \xs' fixedDist ->
         let xs = nub xs'

             okay :: Dendrogram Char -> [Char] -> Maybe [Char]
             okay (Leaf z)       ys | z `elem` ys    = Just (delete z ys)
             okay (Branch d l r) ys | d ~= fixedDist = okay l ys >>= okay r
             okay _ _ = Nothing

             dist x y | x == y    = error "shouldn't calculate (dist x x)"
                      | otherwise = fixedDist

         in not (null xs) ==> okay (f xs dist) xs == Just []

----------------------------------------------------------------------

type P = (Double, Double)

propCorrectLinkage :: ([P] -> (P -> P -> Distance) -> Dendrogram P)
                   -> (D P -> [P] -> [P] -> Distance)
                   -> ([P] -> Property)
propCorrectLinkage f link =
    \xs -> length xs >= 2 ==> correctLinkage link d (f xs d)
        where d = euclideanDist

type D a = a -> a -> Distance

correctLinkage :: (D a -> [a] -> [a] -> Distance) -> D a -> Dendrogram a -> Bool
correctLinkage link dist = go
    where
      go (Branch d l r) = go l && go r &&
                          link dist (elements l) (elements r) ~= d
      go (Leaf _) = True

singleLink, completeLink, upgma :: D a -> [a] -> [a] -> Distance
singleLink   dist xs ys = minimum [x `dist` y | x <- xs, y <- ys]
completeLink dist xs ys = maximum [x `dist` y | x <- xs, y <- ys]
upgma        dist xs ys = sum [x `dist` y | x <- xs, y <- ys] /
                          fromIntegral (length xs * length ys)

----------------------------------------------------------------------

isPermutationOf :: Ord a => [a] -> [a] -> Bool
isPermutationOf xs ys = sort xs == sort ys

euclideanDist :: P -> P -> Double
euclideanDist (x1,y1) (x2,y2) = sqrt $ sq (x1-x2) + sq (y1-y2)
    where sq x = x * x

(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) < 1e-5

zero :: Double
zero = 0

assertErrors :: a -> Assertion
assertErrors x = do
    b <- E.catch (E.evaluate x >> return True)
                 (\(E.ErrorCall _) -> return False {- Ok -})
    when b $ assertFailure "Didn't raise an 'error'."


-- | Compare two dendrograms without being concerned about
-- permutations.
(====) :: Eq a => Dendrogram a -> Dendrogram a -> Bool
Leaf x1         ==== Leaf x2         = x1 == x2
Branch d1 l1 r1 ==== Branch d2 l2 r2 = d1 ~= d2 && ((l1 ==== l2 && r1 ==== r2) ||
                                                    (l1 ==== r2 && r1 ==== l2))
_ ==== _ = False