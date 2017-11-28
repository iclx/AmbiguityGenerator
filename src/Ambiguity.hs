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
       (prevDraws, states) <- fmap unzip <$> sequence $ take 100 $ generateState initGen

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


-- | Generate a shuffled list of ambiguous realizations.
generateShuffle :: forall m. MonadRandom m => AmbiGenState -> Int -> m [AmbiGenReal]
generateShuffle gen n
  = do (realizations, states) <- fmap unzip <$> takeForShuffle $ generateState gen
       randomShuffle n realizations
  where
    -- Want to take at least 2 * n for shuffling.
    -- Take 2 * n, and then test if we need more.
    takeForShuffle :: (Num a, RealFrac a) => [m (a, AmbiGenState)] -> m [(a, AmbiGenState)]
    takeForShuffle = takeForShuffleHelper (2 * n) []

    takeForShuffleHelper :: (Num a, RealFrac a) => Int -> [(a, AmbiGenState)] -> [m (a, AmbiGenState)] -> m [(a, AmbiGenState)]
    takeForShuffleHelper k rest [] = return rest

    takeForShuffleHelper 0 rest (gen : gens)
      = do x <- gen
           if toRange (0, 1) (fst x) == 0
             then return $ x : rest
             else takeForShuffleHelper 0 (x : rest) gens

    takeForShuffleHelper k rest (gen : gens)
      = do x <- gen
           takeForShuffleHelper (k-1) (x : rest) gens


-- | Generate shuffled ambiguous values with finite support.
generateShuffleR :: (Integral a, MonadRandom m) => AmbiGenState -> Int -> (a, a) -> m [a]
generateShuffleR gen n range = map (toRange range) <$> generateShuffle gen n


-- | Generate shuffled continuous values.
generateShuffleContinuousR :: (RealFrac a, MonadRandom m) => AmbiGenState -> Int -> (a, a) -> m [a]
generateShuffleContinuousR gen n range = map (toContinuous range) <$> (generateShuffle gen n)


randomShuffle :: forall m a. MonadRandom m => Int -> [a] -> m [a]
randomShuffle n l
  = map (l !!) <$> selectSequence
  where
    selectSequence :: m [Int]
    selectSequence = take n <$> getRandomRs (0, length l - 1)


shuffleAndTake :: MonadRandom m => Int -> [a] -> m [a]
shuffleAndTake n l = randomShuffle n $ take (4 * n) l


shuffleAndTakeSeq :: MonadRandom m => Int -> [m a] -> m [a]
shuffleAndTakeSeq n l
  = do ls <- sequence $ take (4 * n) l
       randomShuffle n ls


takeFstM :: Monad m => Int -> [m (a, b)] -> [m a]
takeFstM n = fmap (fmap fst) . take n


generate :: MonadRandom m => AmbiGenState -> Int -> [m AmbiGenReal]
generate ambi n = takeFstM n $ generateState ambi


generateR :: (Integral a, MonadRandom m) => AmbiGenState -> Int -> (a, a) -> [m a]
generateR ambi n range = takeFstM n $ generateStateR ambi range


generateContinuousR :: (RealFrac a, MonadRandom m) => AmbiGenState -> Int -> (a, a) -> [m a]
generateContinuousR ambi n range = takeFstM n $ generateStateContinuousR ambi range


generateStateContinuousR :: (RealFrac a, MonadRandom m) => AmbiGenState -> (a, a) -> [m (a, AmbiGenState)]
generateStateContinuousR ambi range = fmap tupleToRange <$> generateState ambi
  where tupleToRange (v, ambi) = (toContinuous range v, ambi)


generateStateR :: (Integral a, MonadRandom m) => AmbiGenState -> (a, a) -> [m (a, AmbiGenState)]
generateStateR ambi range = fmap tupleToRange <$> generateState ambi
  where tupleToRange (v, ambi) = (toRange range v, ambi)


generateState :: forall m. MonadRandom m => AmbiGenState -> [m (AmbiGenReal, AmbiGenState)]
generateState ambi = iterate step (nextAmbi ambi)
  where step :: m (AmbiGenReal, AmbiGenState) -> m (AmbiGenReal, AmbiGenState)
        step gen = do (r, ambi') <- gen
                      nextAmbi ambi'


ambiSkip :: MonadRandom m => Int -> AmbiGenState -> m AmbiGenState
ambiSkip skipAmount ambi
  = do states <- sequence $ take skipAmount $ generateState ambi
       return $ snd (states !! skipAmount)


generateContinuous :: forall m. MonadRandom m => ContinuousState -> [m AmbiGenReal]
generateContinuous gen = fmap fst <$> iterate step (nextContinuous gen)
  where step :: m (AmbiGenReal, ContinuousState) -> m (AmbiGenReal, ContinuousState)
        step gen = do (r, ambi') <- gen
                      nextContinuous ambi'


generateBits :: MonadRandom m => ContinuousState -> [m Int]
generateBits gen = fmap fst <$> generateBitsState gen


generateBitsState :: forall m. MonadRandom m => ContinuousState -> [m (Int, ContinuousState)]
generateBitsState gen = iterate step (nextBit gen)
  where step :: m (Int, ContinuousState) -> m (Int, ContinuousState)
        step gen = do (r, ambi') <- gen
                      nextBit ambi'


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
