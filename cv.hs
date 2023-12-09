import Data.List (genericLength)
import System.Random (newStdGen, randomRs)
import Control.Monad.Random (Rand, RandomGen, evalRand)
import Control.Monad (replicateM)
import Control.Parallel.Strategies (parMap, rseq)

-- Defining bench functions 
bent :: [Double] -> Double
bent xs = sum $ map (\x -> x * x) $ map (\x -> sqrt (abs x)) xs

-- Calculating
avgBent :: [[Double]] -> Double
avgBent xs = sum (map bent xs) / genericLength xs

-- Cross-validation
crossValidation :: RandomGen g => Int -> g -> [[Double]] -> Double
crossValidation k g xs =
  let splits = splitList k xs
      scores = parMap rseq avgBent splits
  in sum scores / genericLength scores

-- Dividing data into k equal parts
splitList :: Int -> [a] -> [[a]]
splitList k xs =
  let n = length xs div k
  in take k $ chunk n xs

-- Main function
main :: IO ()
main = do
  let k = 5 -- number of cross-validation folds
  let n = 100 -- number of data samples
  
  -- Random Data Generation
  g <- newStdGen
  let randomData = evalRand (replicateM n (replicateM 10 (getNextRandom g))) g
  
  -- cross-validation Calculation
  let cvScore = crossValidation k g randomData
  
  putStrLn $ "Average value of benchmark functions: " ++ show cvScore
  where
    -- Generate random numbers from -1 to 1
    getNextRandom :: RandomGen g => g -> Rand g Double
    getNextRandom g = do
      let (r, g') = randomRs (-1, 1) g
      return $ head r