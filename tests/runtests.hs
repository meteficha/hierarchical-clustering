-- from base
import qualified Control.Exception as E
import Control.Monad (when)
import Data.List (sort)
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


main :: IO ()
main = hspecX $ do
         test_cutAt
         test_dendrogram

test_cutAt :: Specs
test_cutAt =
    describe "cutAt" $ do
      let dendro      :: Dendrogram Double Char
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
    describe "dendrogram SingleLinkage" $ do
      basicDendrogramTests SingleLinkage
    describe "dendrogram CompleteLinkage" $ do
      basicDendrogramTests CompleteLinkage
    describe "dendrogram UPGMA" $ do
      basicDendrogramTests UPGMA
    describe "dendrogram FakeAverageLinkage" $ do
      basicDendrogramTests FakeAverageLinkage


basicDendrogramTests :: Linkage -> Specs
basicDendrogramTests linkage = do
  let f = dendrogram linkage
  it "fails for an empty input" $
     assertErrors (f [] (\_ _ -> zero))
  it "works for one element" $
     Leaf () == f [()] (\_ _ -> zero)
  prop "always returns the elements we gave" $
     \xs dist ->
         not (null (xs :: [Double])) ==>
         elements (f xs ((abs .) . dist)) `isPermutationOf` xs

isPermutationOf :: Ord a => [a] -> [a] -> Bool
isPermutationOf xs ys = sort xs == sort ys

zero :: Double
zero = 0

assertErrors :: a -> Assertion
assertErrors x = do
    b <- E.catch (E.evaluate x >> return True)
                 (\(E.ErrorCall _) -> return False {- Ok -})
    when b $ assertFailure "Didn't raise an 'error'."