{-# LANGUAGE ScopedTypeVariables #-}
module Ambiguity where

import System.Random
import Control.Monad.Random.Class hiding (fromList)
import Data.ReinterpretCast
import Data.IntMap hiding (map, split, foldl)
import Data.Bits
import Data.Fixed


type AmbiGenReal = Double

-- | Draw a value from the cauchy distribution.
cauchyDraw :: Floating a => a -> a -> a -> a
cauchyDraw offset scale y = scale * tan (pi * (y - 1/2)) + offset


cauchyDrawM :: (MonadRandom m, Random a, Floating a) => a -> a -> m a
cauchyDrawM offset scale
  = do y <- getRandomR (0, 1)
       return $ cauchyDraw offset scale y


-- | State for an ambiguous number generator. Mostly this includes
--   parameters for the Cauchy distribution that we draw from at each
--   step (the offset, and scale). There are other parametrs for how
--   the scale changes over time, and some state for previous draws,
--   which are used as seeds for the ambiguity generator.
data AmbiGenState =
  AmbiGenState { genOffset :: AmbiGenReal  -- ^ Offset for the cauchy distribution ("location").
               , genScale  :: AmbiGenReal  -- ^ Scale for the cauchy distribution.
               , genPhi    :: AmbiGenReal  -- ^ Small multiplicative scaling factor for adjusting the scale.
               , genPsi    :: AmbiGenReal  -- ^ Small additive scaling factor for adjusting the scale.
               , genSeed1  :: AmbiGenReal  -- ^ Last draw.
               , genSeed2  :: AmbiGenReal  -- ^ Second last draw.
               , genSeed3  :: AmbiGenReal  -- ^ Third last draw.
               , genSeed4  :: AmbiGenReal  -- ^ Fourth last draw.
               , genDraws  :: Integer      -- ^ Number of draws.
               }


instance Show AmbiGenState where
  show (AmbiGenState offset scale phi psi seed1 seed2 seed3 seed4 numDraws)
    = concat ["offset: ",
              show offset,
              ", scale: ",
              show scale,
              " seeds: ",
              show (seed1, seed2, seed3, seed4),
              " draw: ",
              show numDraws]


-- | Initialize the ambiguity generator.
--   This will also run the first couple of steps to get the seeds.
mkAmbiGen :: MonadRandom m => AmbiGenReal -> AmbiGenReal -> AmbiGenReal -> AmbiGenReal -> m AmbiGenState
mkAmbiGen phi psi startOffLow startOffHigh
  = do offset <- getRandomR (startOffLow, startOffHigh)
       seed4 <- cauchyDrawM offset scale
       seed3 <- cauchyDrawM seed4 scale
       seed2 <- cauchyDrawM seed3 scale
       seed1 <- cauchyDrawM seed2 scale

       return $ AmbiGenState seed1 scale phi psi seed1 seed2 seed3 seed4 5
    where
      scale = 1


data ContinuousState =
  ContinuousState { contAmbiGen :: AmbiGenState
                  , contPrevRealizations :: [AmbiGenReal]
                  }


mkContinuousState :: MonadRandom m => AmbiGenReal -> AmbiGenReal -> AmbiGenReal -> AmbiGenReal -> m ContinuousState
mkContinuousState phi psi startOffLow startOffHigh
  = do initGen <- mkAmbiGen phi psi startOffLow startOffHigh
       (prevDraws, states) <- unzip <$> generateWithStates initGen 100

       return $ ContinuousState (last states) prevDraws


nextContinuous :: MonadRandom m => ContinuousState -> m (Double, ContinuousState)
nextContinuous (ContinuousState gen prev)
  = do let maxReal = maximum prev
       let minReal = minimum prev

       (draw, newGen) <- nextAmbi gen

       let newPrev = take 100 $ (draw : prev)

       let value = if maxReal == minReal
                   then maxReal - fromInteger (floor maxReal)
                   else if draw < maxReal && draw > minReal
                        then (draw - minReal) / (maxReal - minReal)
                        else ((toContinuous (minReal, maxReal) draw) - minReal) / (maxReal - minReal)

       return (value, ContinuousState newGen newPrev)


nextBit :: MonadRandom m => ContinuousState -> m (Int, ContinuousState)
nextBit (ContinuousState gen prev)
  = do index <- getRandomR (0, length prev - 1)
       let pivot = prev !! index

       (draw, newGen) <- nextAmbi gen

       let newPrev = take 100 $ (draw : prev)
       let value = if draw > pivot then 1 else 0

       return (value, ContinuousState newGen newPrev)


-- | Generate an ambiguous AmbiGenReal, and the next state of the ambiguity generator.
nextAmbi :: MonadRandom m => AmbiGenState -> m (AmbiGenReal, AmbiGenState)
nextAmbi (AmbiGenState offset scale phi psi seed1 seed2 seed3 seed4 numDraws)
  = do draw <- cauchyDrawM offset scale

       let offset' = draw
       let scale' = phi * seed1 + psi

       let nextGen = AmbiGenState offset' scale' phi psi draw seed1 seed2 seed3 (numDraws+1)

       return (draw, nextGen)


randomShuffle :: forall m a. MonadRandom m => [a] -> m [a]
randomShuffle l
  = map (l !!) <$> selectSequence
  where
    selectSequence :: m [Int]
    selectSequence = do rs <- getRandomRs (0, length l - 1)
                        return $ take (length l) rs


generateWithStates :: MonadRandom m => AmbiGenState -> Int -> m [(AmbiGenReal, AmbiGenState)]
generateWithStates ambi 0 = return []
generateWithStates ambi n
  = do s@(v, ambi') <- nextAmbi ambi
       rest <- generateWithStates ambi' (n-1)

       return $ s : rest


generate :: MonadRandom m => AmbiGenState -> Int -> m [AmbiGenReal]
generate ambi n = map fst <$> generateWithStates ambi n


generateR :: (Integral a, MonadRandom m) => AmbiGenState -> Int -> (a, a) -> m [a]
generateR ambi n range = map (toRange range) <$> generate ambi n


generateContinuousR :: (RealFrac a, MonadRandom m) => AmbiGenState -> Int -> (a, a) -> m [a]
generateContinuousR ambi n range = map (toContinuous range) <$> generate ambi n


ambiSkip :: MonadRandom m => Int -> AmbiGenState -> m AmbiGenState
ambiSkip skipAmount ambi
  = do states <- generateWithStates ambi (skipAmount+1)
       return $ snd (states !! skipAmount)


generateContinuousState :: MonadRandom m => ContinuousState -> Int -> m [(AmbiGenReal, ContinuousState)]
generateContinuousState gen 0 = return []
generateContinuousState gen n
  = do s@(r, gen') <- nextContinuous gen
       rest <- generateContinuousState gen' (n-1)

       return $ s : rest


generateContinuous :: MonadRandom m => ContinuousState -> Int -> m [AmbiGenReal]
generateContinuous gen n = map fst <$> generateContinuousState gen n


generateBitsState :: MonadRandom m => ContinuousState -> Int -> m [(Int, ContinuousState)]
generateBitsState gen 0 = return []
generateBitsState gen n
  = do s@(r, gen') <- nextBit gen
       rest <- generateBitsState gen' (n-1)

       return $ s : rest


generateBits :: MonadRandom m => ContinuousState -> Int -> m [Int]
generateBits gen n = map fst <$> generateBitsState gen n



toRange :: (Integral a, Num b, RealFrac b) => (a, a) -> b -> a
toRange (lo, hi) x
  | lo > hi = toRange (hi, lo) x
  | otherwise = floor x `mod` (hi - lo + 1) + lo


toContinuous :: (RealFrac a, Num b, RealFrac b) => (a, a) -> b -> a
toContinuous (lo, hi) x
  | lo > hi = toContinuous (hi, lo) x
  | otherwise = (realToFrac x) `mod'` (hi - lo + 1) + lo


zeroMap :: Integer -> IntMap Int
zeroMap range = fromList [(x, 0) | x <- [1 .. fromIntegral range]]


countMap :: Integer -> [Int] -> IntMap Int
countMap range ls = Prelude.foldr (adjust (+1)) (zeroMap range) ls


drawCount :: Integer -> Integer -> (StdGen -> Integer -> Integer -> [Integer]) -> IO (IntMap Int)
drawCount range num generator
  = do source <- newStdGen
       let draw = generator source num range

       return (countMap range (map fromIntegral draw))


countToHistogram :: IntMap Int -> [Int]
countToHistogram = map snd . toAscList


drawHistogram :: Integer -> Integer -> (StdGen -> Integer -> Integer -> [Integer]) -> IO [Int]
drawHistogram range num generator
  = fmap countToHistogram (drawCount range num generator)


proportion :: Integer -> [Int] -> Float
proportion y (x : xs) = fromIntegral x / fromIntegral y
