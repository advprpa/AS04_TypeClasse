{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where
import Test.Hspec ( hspec, describe, it, shouldBe )
import Data.Monoid
import Map.TreeMap

main :: IO ()
main = hspec $ describe "TreeMap" $ do
  it "toList empty == []" $  
    toList empty `shouldBe` [] @(String,Integer)
  it "toList (insert 'a' 12 empty) == [('a',12)]" $
    toList (insert 'a' 12 empty) `shouldBe` [('a',12:: Int)]
  it "get 'a' (insert 'a' 12 empty) == Just 12" $
    get 'a' (insert 'a' 12 empty) `shouldBe` Just (12::Int)
  it "get 'b' (insert 'a' 12 empty) == Nothing" $
    get 'b' (insert 'a' (12 :: Int) empty) `shouldBe` Nothing
  it "get 'a' (insertOrMerge 'a' (Sum 2) (insert 'a' (Sum 1) empty)) == Just (Sum 3)" $
    get 'a' (insertOrMerge 'a' (Sum 2) (insert 'a' (Sum 1) empty)) `shouldBe` Just (Sum (3:: Int))
