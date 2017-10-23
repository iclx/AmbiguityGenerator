module Ambiguity where

import System.Random
import Data.ReinterpretCast
import Data.IntMap hiding (map, split, foldl)
import Data.Bits
import Data.Fixed


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
               , genPhi    :: AmbiGenReal  -- ^ Small multiplicative scaling factor for adjusting the scale.
               , genPsi    :: AmbiGenReal  -- ^ Small additive scaling factor for adjusting the scale.
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
mkAmbiGen :: RandomGen s => s -> AmbiGenReal -> AmbiGenReal -> AmbiGenReal -> AmbiGenReal -> AmbiGenState s
mkAmbiGen source phi psi startOffLow startOffHigh
  = ambiInit
    where
      ambiInit = AmbiGenState offset scale phi psi seed1 seed2 seed3 seed4 5 source5

      seed4 = cauchyDraw y scale y1
      seed3 = cauchyDraw seed4 scale y2
      seed2 = cauchyDraw seed3 scale y3
      seed1 = cauchyDraw seed2 scale y4

      offset = seed1
      scale = 1

      (y,  source1) = randomR (startOffLow :: Double, startOffHigh :: Double) source
      (y1, source2) = randomR (0, 1) source1
      (y2, source3) = randomR (0, 1) source2
      (y3, source4) = randomR (0, 1) source3
      (y4, source5) = randomR (0, 1) source4


data ContinuousState s =
  ContinuousState { contAmbiGen :: AmbiGenState s
                  , contPrevRealizations :: [AmbiGenReal]
                  , contRandomGen :: s
                  }


mkContinuousState :: RandomGen s => s -> AmbiGenReal -> AmbiGenReal -> AmbiGenReal -> AmbiGenReal -> ContinuousState s
mkContinuousState source phi psi startOffLow startOffHigh
  = ContinuousState (last states) prevDraws randGen
    where
      (ambiGen, randGen) = split source
      initGen = mkAmbiGen ambiGen phi psi startOffLow startOffHigh
      (prevDraws, states) = unzip . take 100 $ generateState initGen


nextContinuous :: RandomGen s => ContinuousState s -> (Double, ContinuousState s)
nextContinuous (ContinuousState gen prev randGen)
  = (value, ContinuousState newGen newPrev randGen)
    where
      maxReal = maximum prev
      minReal = minimum prev

      (draw, newGen) = nextAmbi gen

      newPrev = take 100 $ (draw : prev)

      value = if maxReal == minReal
              then maxReal - fromInteger (floor maxReal)
              else if draw < maxReal && draw > minReal
                   then (draw - minReal) / (maxReal - minReal)
                   else ((toContinuous (minReal, maxReal) draw) - minReal) / (maxReal - minReal)


nextBit :: RandomGen s => ContinuousState s -> (Int, ContinuousState s)
nextBit (ContinuousState gen prev randGen)
  = (value, ContinuousState newGen newPrev newRandGen)
    where
      (index, newRandGen) = randomR (0, length prev - 1) randGen
      pivot = prev !! index

      (draw, newGen) = nextAmbi gen

      newPrev = take 100 $ (draw : prev)

      value = if draw > pivot then 1 else 0


-- | Generate an ambiguous AmbiGenReal, and the next state of the ambiguity generator.
nextAmbi :: RandomGen s => AmbiGenState s -> (AmbiGenReal, AmbiGenState s)
nextAmbi (AmbiGenState offset scale phi psi seed1 seed2 seed3 seed4 numDraws source)
  = (draw', nextGen)
    where draw = cauchyDraw offset scale y
          (y, source') = randomR (0, 1) source

          threshold = max (fromIntegral numDraws ** 4) (10 ** 5)

          rescale = if abs seed1 > threshold
                    then False -- if (floor seed3) `mod` 2 == 0 then True else False
                    else False

          offset' = if rescale
                    then sin seed3
                    else draw'

          scale' = if rescale
                   then 1
                   else phi * seed1 + psi -- abs seed2 / (fromIntegral numDraws) + psi

          draw' = if rescale then sin (1 / (abs draw + 1)) else draw
          nextGen = AmbiGenState offset' scale' phi psi draw' seed1 seed2 seed3 (numDraws+1) source'


-- | Generate a shuffled list of ambiguous realizations.
generateShuffle :: RandomGen s => AmbiGenState s -> Int -> [AmbiGenReal]
generateShuffle gen n = randomShuffle n gen' $ realizations
  where
    gen' = genSource $ last states
    (realizations, states) = unzip . takeForShuffle $ generateState gen

    -- Want to take at least 2 * n for shuffling.
    -- Take 2 * n, and then test if we need more.
    takeForShuffle :: (Num a, RealFrac a, RandomGen s) => [(a, AmbiGenState s)] -> [(a, AmbiGenState s)]
    takeForShuffle = takeForShuffleHelper (2 * n) []

    takeForShuffleHelper :: (Num a, RealFrac a, RandomGen s) => Int -> [(a, AmbiGenState s)] -> [(a, AmbiGenState s)] -> [(a, AmbiGenState s)]
    takeForShuffleHelper k rest [] = rest

    takeForShuffleHelper 0 rest (x : xs)
      = if toRange (0, 1) (fst x) == 0
        then x : rest
        else takeForShuffleHelper 0 (x : rest) xs

    takeForShuffleHelper k rest (x : xs) = takeForShuffleHelper (k-1) (x : rest) xs


-- | Generate shuffled ambiguous values with finite support.
generateShuffleR :: (Integral a, RandomGen s) => AmbiGenState s -> Int -> (a, a) -> [a]
generateShuffleR gen n range = map (toRange range) (generateShuffle gen n)


-- | Generate shuffled continuous values.
generateShuffleContinuousR :: (RealFrac a, RandomGen s) => AmbiGenState s -> Int -> (a, a) -> [a]
generateShuffleContinuousR gen n range = map (toContinuous range) (generateShuffle gen n)


randomShuffle :: RandomGen s => Int -> s -> [a] -> [a]
randomShuffle n gen l =
  map (l !!) selectSequence
  where
    selectSequence :: [Int]
    selectSequence = take n $ randomRs (0, length l - 1) gen


shuffleAndTake :: RandomGen s => Int -> s -> [a] -> [a]
shuffleAndTake n gen l = randomShuffle n gen $ take (4 * n) l


generate :: RandomGen s => AmbiGenState s -> Int -> [AmbiGenReal]
generate ambi n = take n . map fst $ generateState ambi


generateR :: (Integral a, RandomGen s) => AmbiGenState s -> Int -> (a, a) -> [a]
generateR ambi n range = take n . map fst $ generateStateR ambi range


generateContinuousR :: (RealFrac a, RandomGen s) => AmbiGenState s -> Int -> (a, a) -> [a]
generateContinuousR ambi n range = take n . map fst $ generateStateContinuousR ambi range


generateStateContinuousR :: (RealFrac a, RandomGen s) => AmbiGenState s -> (a, a) -> [(a, AmbiGenState s)]
generateStateContinuousR ambi range = map tupleToRange $ generateState ambi
  where tupleToRange (v, ambi) = (toContinuous range v, ambi)


generateStateR :: (Integral a, RandomGen s) => AmbiGenState s -> (a, a) -> [(a, AmbiGenState s)]
generateStateR ambi range = map tupleToRange $ generateState ambi
  where tupleToRange (v, ambi) = (toRange range v, ambi)


generateState :: RandomGen s => AmbiGenState s -> [(AmbiGenReal, AmbiGenState s)]
generateState ambi = (r, ambi') : generateState ambi'
  where (r, ambi') = nextAmbi ambi


generateContinuous :: RandomGen s => ContinuousState s -> [Double]
generateContinuous gen = v : generateContinuous gen'
  where (v, gen') = nextContinuous gen


generateBits :: RandomGen s => ContinuousState s -> [Int]
generateBits = map fst . generateBitsState


generateBitsState :: RandomGen s => ContinuousState s -> [(Int, ContinuousState s)]
generateBitsState gen = (v, gen') : generateBitsState gen'
  where (v, gen') = nextBit gen


toRange :: (Integral a, Num b, RealFrac b) => (a, a) -> b -> a
toRange (lo, hi) x
  | lo > hi = toRange (hi, lo) x
  | otherwise = floor x `mod` (hi - lo + 1) + lo


toContinuous :: (RealFrac a, Num b, RealFrac b) => (a, a) -> b -> a
toContinuous (lo, hi) x
  | lo > hi = toContinuous (hi, lo) x
  | otherwise = (realToFrac x) `mod'` (hi - lo + 1) + lo



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
