{-# LANGUAGE Rank2Types #-}

-- from base
import qualified Control.Exception as E
import Control.Monad (when)
import Data.List (delete, sort)
import Text.Printf (printf)
import Text.Show.Functions ()

-- from hspec
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)

-- from HUnit
import Test.HUnit

-- from QuickCheck
import Test.QuickCheck ((==>))

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
    describe "Optimal's completeLinkage" $ do
      basicDendrogramTests O.completeLinkage
    describe "DistanceMatrix's singleLinkage" $ do
      basicDendrogramTests DM.singleLinkage
    describe "DistanceMatrix's completeLinkage" $ do
      basicDendrogramTests DM.completeLinkage
    describe "DistanceMatrix's upgma" $ do
      basicDendrogramTests DM.upgma
    describe "DistanceMatrix's fakeAverageLinkage" $ do
      basicDendrogramTests DM.fakeAverageLinkage

    describe "Optimal and DistanceMatrix" $ do
      let test f1 f2 = \ps -> length ps >= 2 ==>
                              f1 ps euclideanDist ==== f2 ps euclideanDist
      prop "agree on singleLinkage"   $ test O.singleLinkage   DM.singleLinkage
      prop "agree on completeLinkage" $ test O.completeLinkage DM.completeLinkage


basicDendrogramTests :: (forall a. [a] -> (a -> a -> Double) -> Dendrogram a) -> Specs
basicDendrogramTests f = do
  it "fails for an empty input" $
     assertErrors (f [] (\_ _ -> zero))
  it "works for one element" $
     Leaf () == f [()] (\_ _ -> zero)
  prop "always returns the elements we gave" $
     \points ->
         not (null points) ==>
         elements (f points euclideanDist) `isPermutationOf` points
  prop "works for examples where all elements have the same distance" $
     \xs fixedDist ->
         let okay :: Dendrogram Char -> [Char] -> Maybe [Char]
             okay (Leaf z)       ys | z `elem` ys    = Just (delete z ys)
             okay (Branch d l r) ys | d ~= fixedDist = okay l ys >>= okay r
             okay _ _ = Nothing
         in not (null xs) ==> okay (f xs (\_ _ -> fixedDist)) xs == Just []


isPermutationOf :: Ord a => [a] -> [a] -> Bool
isPermutationOf xs ys = sort xs == sort ys

euclideanDist :: (Double, Double) -> (Double, Double) -> Double
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