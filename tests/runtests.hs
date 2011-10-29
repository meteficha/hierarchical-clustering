-- from base
import qualified Control.Exception as E
import Control.Monad (when)
import Data.List (sort)
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
         describe "dendrogram SingleLinkage" $ do
           basicTests SingleLinkage
         describe "dendrogram CompleteLinkage" $ do
           basicTests CompleteLinkage
         describe "dendrogram UPGMA" $ do
           basicTests UPGMA
         describe "dendrogram FakeAverageLinkage" $ do
           basicTests FakeAverageLinkage

basicTests :: Linkage -> Specs
basicTests linkage = do
  let f = dendrogram linkage
  it "fails for an empty input" $
     assertErrors (f [] (\_ _ -> zero))
  it "works for one element" $
     Leaf () == f [()] (\_ _ -> zero)
  prop "returns the elements we gave" $
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