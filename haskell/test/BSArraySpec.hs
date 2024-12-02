{-# LANGUAGE OverloadedStrings #-}
module BSArraySpec where

import Test.Hspec
import qualified BSArray as BS
import Control.Exception (evaluate)


spec :: Spec
spec = do
  describe "Array \"ABC\\nDEF\"  (no final newline)" $ do
    let bsa = BS.makeBSarray  "ABC\nDEF"
    it "has the right num cols"  $ do
      BS.cols bsa `shouldBe` 3
    it "has the right num rows"  $ do
      BS.rows bsa `shouldBe` 2
    describe "lookup" $ do
      it "returns first element" $ do
        BS.lookup bsa (0,0) `shouldBe` 'A'
      it "returns correct last element" $ do
        BS.lookup bsa (1,2) `shouldBe` 'F'
      it "throws error with negative index" $ do
        evaluate (BS.lookup bsa (-1,2)) `shouldThrow` errorCall "Negative row index!"
    describe "lookupMaybe" $ do
      it "returns first element (Just)" $ do
        BS.lookupMaybe bsa (0,0) `shouldBe` Just 'A'
      it "returns Nothing when out of range (cols)"  $ do
        BS.lookupMaybe bsa (0,3) `shouldBe` Nothing
      it "returns Nothing when out of range (rows)"  $ do
        BS.lookupMaybe bsa (2,0) `shouldBe` Nothing
      it "returns Nothing when out of range <0 (cols)"  $ do
        BS.lookupMaybe bsa (0,-1) `shouldBe` Nothing
      it "returns Nothing when out of range <0 (rows)"  $ do
        BS.lookupMaybe bsa (0,-1) `shouldBe` Nothing

  describe "Array with final newline" $ do
    let bsa = BS.makeBSarray  "ABC\nDEF\n"
    it "returns first element" $ do
      BS.lookup bsa (0,0) `shouldBe` 'A'
    it "has the right num cols"  $ do
      BS.cols bsa `shouldBe` 3
    it "has the right num rows"  $ do
      BS.rows bsa `shouldBe` 2
    it "returns correct last element" $ do
      BS.lookup bsa (1,2) `shouldBe` 'F'
    it " .row works correct" $ do
      BS.row bsa 1 `shouldBe` "DEF"

  describe "intIndex" $ do 
    let bsa = BS.makeBSarray  "ABC\nDEF\n"
    it "0,0 -> 0" $ do 
      BS.intIndex bsa (0,0) `shouldBe` 0 
    it "1,2 -> 6" $ do 
      BS.intIndex bsa (1,2) `shouldBe` 5 
  