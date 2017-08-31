module Ambiguity where

import System.Random
import Data.ReinterpretCast
import Data.IntMap hiding (map, split, foldl)
import Data.Bits


type AmbiGenReal = Double

-- | Draw a value from the cauchy distribution.
cauchyDraw :: Floating a => a -> a -> a -> a
cauchyDraw offset scale y = scale * tan (pi * (y - 1/2)) + offset


-- | State for an ambiguous number generator. Mostly this includes
--   parameters for the Cauchy distribution that we draw from at each
--   step (the offset, and scale). There are other parametrs for how
--   the scale changes over time, and some state for previous draws,
--   which are used as seeds for the ambiguity generator.
data AmbiGenState s =
  AmbiGenState { genOffset :: AmbiGenReal  -- ^ Offset for the cauchy distribution ("location").
               , genScale  :: AmbiGenReal  -- ^ Scale for the cauchy distribution.
               , genPhi    :: AmbiGenReal  -- ^ Small scaling factor for adjusting the scale.
               , genPsi    :: AmbiGenReal  -- ^ Small scaling factor for adjusting the scale.
               , genSeed1  :: AmbiGenReal  -- ^ Last draw.
               , genSeed2  :: AmbiGenReal  -- ^ Second last draw.
               , genSeed3  :: AmbiGenReal  -- ^ Third last draw.
               , genSeed4  :: AmbiGenReal  -- ^ Fourth last draw.
               , genDraws  :: Integer      -- ^ Number of draws.
               , genSource :: s            -- ^ Seed for uniform random number generation.
               }


instance Show (AmbiGenState s) where
  show (AmbiGenState offset scale phi psi seed1 seed2 seed3 seed4 numDraws source''')
    = "offset: " ++ show offset ++ ", scale: " ++ show scale ++ " seeds:" ++ show (seed1, seed2, seed3, seed4)


-- | Initialize the ambiguity generator.
--   This will also run the first couple of steps to get the seeds.
mkAmbiGen :: RandomGen s => s -> AmbiGenReal -> AmbiGenReal -> AmbiGenState s
mkAmbiGen source phi psi
  = AmbiGenState offset scale phi psi seed1 seed2 seed3 seed4 5 source5
    where seed4 = cauchyDraw 0 1 y1
          seed3 = cauchyDraw seed4 1 y2
          seed2 = cauchyDraw seed3 1 y3
          seed1 = cauchyDraw seed2 1 y4

          offset = seed1
          scale = 1

          (y,  source1) = randomR (-100 :: Double, 100) source
          (y1, source2) = randomR (0, 1) source1
          (y2, source3) = randomR (0, 1) source2
          (y3, source4) = randomR (0, 1) source3
          (y4, source5) = randomR (0, 1) source4


-- | Generate an ambiguous AmbiGenReal, and the next state of the ambiguity generator.
nextAmbi :: RandomGen s => AmbiGenState s -> (AmbiGenReal, AmbiGenState s)
nextAmbi (AmbiGenState offset scale phi psi seed1 seed2 seed3 seed4 numDraws source)
  = (draw', nextGen)
    where draw = cauchyDraw offset scale y
          (y, source') = randomR (0, 1) source

          threshold = fromIntegral numDraws ** 4

          rescale = if abs seed1 > threshold
                    then if (floor seed3) `mod` 2 == 0 then True else False
                    else False

          offset' = if rescale
                    then sin (1 / (abs seed3 + 1))
                    else draw'

          scale' = if rescale
                   then 1
                   else abs seed2 / (fromIntegral numDraws) + psi

          draw' = if rescale then sin (1 / (abs draw + 1)) else draw
          nextGen = AmbiGenState offset' scale' phi psi draw' seed1 seed2 seed3 (numDraws+1) source'


generateShuffle :: RandomGen s => AmbiGenState s -> Int -> [AmbiGenReal]
generateShuffle gen n = randomShuffle n gen' . takeForShuffle $ realizations
  where
    gen' = last states
    (realizations, states) = unzip $ generateState gen

    takeForShuffle :: (Num a, Ord a) => [a] -> [a]
    takeForShuffle = takeForShuffleHelper False n []

    takeForShuffleHelper :: (Num a, Ord a) => Bool -> Int -> [a] -> [a] -> [a]
    takeForShuffleHelper _ k rest []= rest
    takeForShuffleHelper True 0 rest (x : xs) = x : rest
    takeForShuffleHelper True k rest (x : xs) = takeForShuffleHelper True (k-1) (x : rest) xs
    takeForShuffleHelper False k rest (x : xs)
      = if abs x > 1000
        then takeForShuffleHelper True (k-1) (x : rest) xs
        else takeForShuffleHelper False (k-1) (x : rest) xs


generateShuffleR :: (Integral a, RandomGen s) => AmbiGenState s -> Int -> (a, a) -> [a]
generateShuffleR gen n range = map (toRange range) (generateShuffle gen n)


-- | Take a possibly infinite list and shuffle it ambiguously using a stopping condition.
shuffle :: RandomGen s => (a -> Bool) -> Int -> AmbiGenState s -> [a] -> [a]
shuffle test n gen l = randomShuffle n gen $ takeWhile (not . test) l


randomShuffle :: RandomGen s => Int -> AmbiGenState s -> [a] -> [a]
randomShuffle n gen l = map (l !!) selectSequence
  where
    selectSequence :: [Int]
    selectSequence = generateR gen n (0, n-1)


generate :: RandomGen s => AmbiGenState s -> Int -> [AmbiGenReal]
generate ambi n = take n . map fst $ generateState ambi


generateR :: (Integral a, RandomGen s) => AmbiGenState s -> Int -> (a, a) -> [a]
generateR ambi n range = take n . map fst $ generateStateR ambi range


generateStateR :: (Integral a, RandomGen s) => AmbiGenState s -> (a, a) -> [(a, AmbiGenState s)]
generateStateR ambi range = map tupleToRange $ generateState ambi
  where tupleToRange (v, ambi) = (toRange range v, ambi)


generateState :: RandomGen s => AmbiGenState s -> [(AmbiGenReal, AmbiGenState s)]
generateState ambi = (r, ambi') : generateState ambi'
  where (r, ambi') = nextAmbi ambi


toRange :: Integral a => (a, a) -> AmbiGenReal -> a
toRange (lo, hi) x
  | lo > hi = toRange (hi, lo) x
  | otherwise = floor x `mod` (hi - lo + 1) + lo


-- | RandomGen instance for the ambiguity generator.
--   This currently has a few problems.
--
--   1) We just recast the double as an integer. Could add bias.
--   2) The split method isn't really well founded. Generator would
--      probably still be in the same "phase" when split, yielding
--      similar results.
instance RandomGen s => RandomGen (AmbiGenState s) where
  next g = (fromIntegral nextInt, g')
    where
      nextInt = foldl (\x y -> 2 *x + y) 0 bits
      (bits, states) = unzip . take 32 $ generateStateR g (0, 1)
      g' = last states

  split (AmbiGenState offset scale phi psi seed1 seed2 seed3 seed4 numDraws source)
    = (AmbiGenState offset scale phi psi seed2 seed1 seed4 seed3 numDraws s1, AmbiGenState offset scale phi psi seed1 seed2 seed3 seed4 numDraws s2)
      where (s1, s2) = split source


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


paperGen :: StdGen -> Int -> Integer ->  [Integer]
paperGen s n range
  = generateR (mkAmbiGen s (1 / fromIntegral n) 0.0001) n (1, fromIntegral range)
