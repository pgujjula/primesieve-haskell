-- |
-- Copyright   : 2021 Preetham Gujjula
-- License     : BSD3
-- Maintainer  : libraries@mail.preetham.io
-- Stability   : experimental
module Main (main) where

import qualified Test.Math.NumberTheory.Prime.Sieve (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "" [Test.Math.NumberTheory.Prime.Sieve.tests]
