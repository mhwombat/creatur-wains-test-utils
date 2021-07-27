------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.TestUtils
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck test utilities.
--
------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.TestUtils
  (
    arb8BitDouble,
    arb8BitInt,
    prop_serialize_round_trippable,
    prop_genetic_round_trippable,
    prop_genetic_round_trippable2,
    prop_diploid_identity,
    prop_show_read_round_trippable,
    prop_diploid_expressable,
    prop_diploid_readable,
    prop_makeSimilar_works,
    sizedArbWeights,
    sizedArbResponse,
    arbResponse
  ) where

import qualified ALife.Creatur.Genetics.BRGCWord8 as W8
import           ALife.Creatur.Genetics.Diploid   (Diploid, express)
import           ALife.Creatur.Util               (fromEither)
import           ALife.Creatur.Wain.PlusMinusOne  (PM1Double, doubleToPM1)
import           ALife.Creatur.Wain.Response      (Response (..))
import           ALife.Creatur.Wain.UnitInterval
    (UIDouble, doubleToUI, interval)
import           ALife.Creatur.Wain.Util
    (scaleFromWord8, scaleWord8ToInt)
import           ALife.Creatur.Wain.Weights       (Weights, makeWeights)
import           Control.DeepSeq                  (NFData, deepseq)
import           Control.Monad.State.Lazy         (runState)
import           Data.Serialize                   (Serialize, decode, encode)
import           Data.Word                        (Word8)
import           Test.QuickCheck

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
instance Arbitrary PM1Double where
  arbitrary = doubleToPM1 <$> choose interval

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
instance Arbitrary UIDouble where
  arbitrary = doubleToUI <$> choose interval

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
arb8BitDouble :: (Double, Double) -> Gen Double
arb8BitDouble interval' = do
  x <- arbitrary :: Gen Word8
  return $ scaleFromWord8 interval' x

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
arb8BitInt :: (Int, Int) -> Gen Int
arb8BitInt interval' = do
  x <- arbitrary :: Gen Word8
  return $ scaleWord8ToInt interval' x

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
prop_serialize_round_trippable :: (Eq a, Serialize a) => a -> Property
prop_serialize_round_trippable x = property $ x' == Right x
  where bs = encode x
        x' = decode bs

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
prop_genetic_round_trippable :: (Eq g, W8.Genetic g, Show g) =>
  (g -> g -> Bool) -> g -> Property
prop_genetic_round_trippable eq g = property $
  g' `eq` g && null leftover
  where x = W8.write g
        (result, (_, i, _)) = runState W8.get (x, 0, [])
        leftover = drop i x
        g' = fromEither (error "read returned Nothing") $ result

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
prop_genetic_round_trippable2
  :: W8.Genetic g => Int -> [Word8] -> g -> Property
prop_genetic_round_trippable2 n xs dummy = length xs >= n
  ==> xs' == take n xs
  where Right g = W8.read xs
        xs' = W8.write (g `asTypeOf` dummy)

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
prop_diploid_identity :: Diploid g => (g -> g -> Bool) -> g -> Property
prop_diploid_identity eq g = property $ express g g `eq` g

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
prop_show_read_round_trippable
  :: (Read a, Show a) => (a -> a -> Bool) -> a -> Property
prop_show_read_round_trippable eq x
  = property $ (read . show $ x) `eq` x

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
prop_diploid_expressable
  :: (Diploid g, W8.Genetic g, NFData g) => g -> g -> Property
prop_diploid_expressable a b = property $ deepseq (express a b) True

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
prop_diploid_readable
  :: (Diploid g, W8.Genetic g, NFData g)
    => g -> g -> Property
prop_diploid_readable a b = property $ deepseq (c `asTypeOf` a) True
  where ga = W8.write a
        gb = W8.write b
        (Right c) = W8.runDiploidReader W8.getAndExpress (ga, gb)

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
prop_makeSimilar_works
  :: (a -> a -> UIDouble) -> (a -> UIDouble -> a -> a) -> a -> UIDouble
    -> a -> Property
prop_makeSimilar_works diff makeSimilar x r y
  = property $ diffAfter <= diffBefore
  where diffBefore = diff x y
        y' = makeSimilar x r y
        diffAfter = diff x y'

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
sizedArbWeights :: Int -> Gen Weights
sizedArbWeights n = fmap makeWeights $ vectorOf n arbitrary

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
instance Arbitrary Weights where
  arbitrary = sized sizedArbWeights

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
sizedArbResponse :: Gen a -> Int -> Gen (Response a)
sizedArbResponse genAction n = do
  nObjects <- choose (0, n)
  let nConditions = n - nObjects
  arbResponse nObjects nConditions genAction

-- IMPORTANT: Keep the code for this function in sync with the
-- version in creatur-wains
arbResponse :: Int -> Int -> Gen a -> Gen (Response a)
arbResponse nObjects nConditions genAction = do
  s <- vectorOf nObjects arbitrary
  a <- genAction
  o <- vectorOf nConditions arbitrary
  return $ Response s a o
