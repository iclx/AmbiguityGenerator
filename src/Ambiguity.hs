module Ambiguity where

import System.Random
import Data.ReinterpretCast
import Data.IntMap hiding (map, split)


-- | Draw a value from the cauchy distribution.
cauchyDraw :: Floating a => a -> a -> a -> a
cauchyDraw offset scale y = scale * tan (pi * (y - 1/2)) + offset


-- | State for an ambiguous number generator. Mostly this includes
--   parameters for the Cauchy distribution that we draw from at each
--   step (the offset, and scale). There are other parametrs for how
--   the scale changes over time, and some state for previous draws,
--   which are used as seeds for the ambiguity generator.
data AmbiGenState s =
  AmbiGenState { genOffset :: Double  -- ^ Offset for the cauchy distribution ("location").
               , genScale :: Double   -- ^ Scale for the cauchy distribution.
               , genPhi :: Double     -- ^ Small scaling factor for adjusting the scale.
               , genPsi :: Double     -- ^ Small scaling factor for adjusting the scale.
               , genSeed1 :: Double   -- ^ Last draw.
               , genSeed2 :: Double   -- ^ Second last draw.
               , genSource :: s       -- ^ Seed for uniform random number generation.
               }
  deriving (Show)


-- | Initialize the ambiguity generator.
--   This will also run the first couple of steps to get the seeds.
mkAmbiGen :: RandomGen s => s -> Double -> Double -> AmbiGenState s
mkAmbiGen source phi psi
  = AmbiGenState offset scale phi psi seed1 seed2 source''
    where seed2 = cauchyDraw 0 1 y
          seed1 = cauchyDraw seed2 1 y'

          offset = seed2
          scale = phi * (abs seed1) + psi

          (y, source') = randomR (0, 1) source
          (y', source'') = randomR (0, 1) source'


-- | Generate an ambiguous Double, and the next state of the ambiguity generator.
nextAmbi :: RandomGen s => AmbiGenState s -> (Double, AmbiGenState s)
nextAmbi (AmbiGenState offset scale phi psi seed1 seed2 source)
  = (draw, AmbiGenState offset' scale' phi psi draw seed1 source')
    where draw = cauchyDraw scale offset y
          (y, source') = randomR (0, 1) source

          offset' = seed1
          scale' = phi * (abs seed2) + psi


generate :: RandomGen s => AmbiGenState s -> Integer -> [Double]
generate _ 0 = []
generate ambi n = r : generate ambi' (n-1)
  where (r, ambi') = nextAmbi ambi


generateR :: RandomGen s => AmbiGenState s -> Integer -> (Integer, Integer) -> [Integer]
generateR ambi n (lo, hi)
  | lo > hi = generateR ambi n (hi, lo)
  | otherwise = map toRange (generate ambi n)
    where toRange x = floor x `mod` (hi - lo + 1) + lo


-- | RandomGen instance for the ambiguity generator.
--   This currently has a few problems.
--
--   1) We just recast the double as an integer. Could add bias.
--   2) The split method isn't really well founded. Generator would
--      probably still be in the same "phase" when split, yielding
--      similar results.
instance RandomGen s => RandomGen (AmbiGenState s) where
  next g = (fromIntegral $ doubleToWord y, g')
    where (y, g') = nextAmbi g

  split (AmbiGenState offset scale phi psi seed1 seed2 source)
    = (AmbiGenState offset scale phi psi seed2 seed1 s1, AmbiGenState offset scale phi psi seed1 seed2 s2)
      where (s1, s2) = split source


zeroMap :: IntMap Int
zeroMap = fromList [(x, 0) | x <- [1..10]]


countMap :: [Int] -> IntMap Int
countMap ls = Prelude.foldr (adjust (+1)) zeroMap ls


drawCount :: Integer -> (StdGen -> Integer -> [Integer]) -> IO (IntMap Int)
drawCount num generator
  = do source <- newStdGen
       let draw = generator source num

       return (countMap (map fromIntegral draw))


countToHistogram :: IntMap Int -> [Int]
countToHistogram = map snd . toAscList


drawHistogram :: Integer -> (StdGen -> Integer -> [Integer]) -> IO [Int]
drawHistogram num generator
  = fmap countToHistogram (drawCount num generator)


paperGen :: StdGen -> Integer -> [Integer]
paperGen s n
  = generateR (mkAmbiGen s 0.0001 0.0001) n (1, 10)

haskellGen :: StdGen -> Integer -> [Integer]
haskellGen s n
  = take (fromIntegral n) $ randomRs (1, 100) (mkAmbiGen s 0.0001 0.0001)
