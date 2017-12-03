{-# LANGUAGE InstanceSigs #-}
module MonadSystemEntropy where


import System.Random
import System.Entropy
import System.IO.Unsafe
import Control.Monad.Random.Class
import Control.Monad.IO.Class
import Control.Monad
import Data.Binary.Get


newtype SystemEntropy a = SystemEntropy {runEntropy :: IO a}


instance Functor SystemEntropy where
  fmap f (SystemEntropy a) = SystemEntropy $ fmap f a


instance Applicative SystemEntropy where
  pure = SystemEntropy . pure
  (SystemEntropy f) <*> (SystemEntropy a) = SystemEntropy $ f <*> a


instance Monad SystemEntropy where
  (SystemEntropy m) >>= f = SystemEntropy $ m >>= (runEntropy . f)


instance MonadIO SystemEntropy where
  liftIO = SystemEntropy


-- Technically not ideal as it relies on the PRNG.
instance MonadRandom SystemEntropy where
  getRandom = SystemEntropy $ fst . random . mkStdGen <$> runEntropy getRandomInt

  getRandomR range = SystemEntropy $ fst . randomR range . mkStdGen <$> runEntropy getRandomInt

  getRandoms
    = do r <- getRandom
         rs <- unsafeInterleaveEntropy getRandoms
         return (r : rs)
                  
  getRandomRs range
    = do r <- getRandomR range
         rs <- unsafeInterleaveEntropy $ getRandomRs range
         return (r : rs)
  

getRandomInt :: SystemEntropy Int
getRandomInt = getWithBytes getInthost


getWithBytes :: Get a -> SystemEntropy a
getWithBytes g
  = feedBytes (runGetIncremental g)
  where feedBytes :: Decoder a -> SystemEntropy a
        feedBytes (Done _ _ a) = return a
        feedBytes (Fail _ _ err) = error err
        feedBytes (Partial f)
          = do byte <- liftIO $ getEntropy 1
               feedBytes (f (Just byte))


unsafeInterleaveEntropy :: SystemEntropy a -> SystemEntropy a
unsafeInterleaveEntropy = liftIO . unsafeInterleaveIO . runEntropy
