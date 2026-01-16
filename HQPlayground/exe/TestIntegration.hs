module Main where

import HQP
--import HQP.QOp.MatrixSemantics as Sem
--import HQP.QOp.StatevectorSemantics as Sem
import HQP.QOp.StateHmatrixSemantics as Sem
import Programs.Grovers (grover, amplitudeEstimation, mcZ, bitsToInt, estimateA)

import System.Random (mkStdGen, randoms)
import System.CPUTime (getCPUTime)
import Control.Exception (evaluate)
import Control.Monad (forM_)

-- Convert integer to n bits (MSB -> LSB), as [0/1]
intToBits :: Int -> Int -> [Int]
intToBits n x =
  [ (x `div` (2^(n-1-i))) `mod` 2 | i <- [0..n-1] ]

-- Build oracle that flips phase on ONE basis state |bits>
mkSingleMarkedOracle :: Int -> [Int] -> QOp
mkSingleMarkedOracle n bits =
  let negateZeroBits = foldr (⊗) One [ if b == 0 then X else Id 1 | b <- bits ]
  in negateZeroBits ∘ mcZ n ∘ negateZeroBits

-- Build oracle for MULTIPLE marked states by composing single-marked phase flips
mkMultiMarkedOracle :: Int -> [[Int]] -> QOp
mkMultiMarkedOracle n marked =
  foldr (∘) (Id n) (map (mkSingleMarkedOracle n) marked)

-- Pick k distinct indices in [0..dim-1] using a stream of Doubles in [0,1)
pickDistinctIdx :: Int -> Int -> [Double] -> [Int]
pickDistinctIdx dim k us = go [] us
  where
    go acc (u:rest)
      | length acc == k = reverse acc
      | otherwise =
          let i = floor (u * fromIntegral dim)  -- in [0..dim-1]
          in if i `elem` acc then go acc rest else go (i:acc) rest
    go acc [] = reverse acc

-- Grover iteration count from an amplitude estimate a in (0,1)
groverRoundsFromA :: Double -> Int
groverRoundsFromA a =
  let a' = max 1e-12 (min (1 - 1e-12) a)     -- avoid asin/sqrt edge cases
      theta = asin (sqrt a')
      r = (pi / (4 * theta)) - 0.5
  in max 0 (floor r)

-- Check whether measured system bits match one of the marked states
isMarked :: [[Int]] -> [Bool] -> Bool
isMarked marked outs =
  let xs = map fromEnum outs
  in xs `elem` marked

timeMs :: IO a -> IO (a, Double)
timeMs action = do
  t0 <- getCPUTime
  x  <- action
  _  <- evaluate x
  t1 <- getCPUTime
  let ms = fromIntegral (t1 - t0) / 1e9  -- ps -> ms
  pure (x, ms)

main :: IO ()
main = do
  let seed      = 4
      n         = 9    
      m         = 8     -- control qubits for AE precision
      k         = 3     -- number of marked solutions (unknown to the algorithm)
      rounds    = 20    -- how many Grover runs to estimate success rate

      rng0 = randoms (mkStdGen seed) :: [Double]
      dim  = 2^n

      idxs     = pickDistinctIdx dim k rng0
      marked   = map (intToBits n) idxs
      oracle   = mkMultiMarkedOracle n marked
      aPrep    = foldr (⊗) One (replicate n H)
      aTrue    = fromIntegral k / ((2 :: Double) ** fromIntegral n)

      -- Programs
      progAE   = amplitudeEstimation m n aPrep oracle

  putStrLn $ "seed=" ++ show seed ++ "  n=" ++ show n ++ "  m=" ++ show m ++ "  k=" ++ show k
  putStrLn $ "Marked states (as bitstrings): " ++ show marked
  putStrLn $ "True a = k/2^n = " ++ show aTrue

  -- Run amplitude estimation once
  let psiAE0 = ket (replicate (m + n) 0)

  ((_, ctrlBits, _), tAE) <- timeMs (pure (Sem.evalProg progAE psiAE0 rng0))
  let aHat = estimateA m ctrlBits
      rHat = groverRoundsFromA aHat -- the estimated Grover rounds from aHat
      rTrue = groverRoundsFromA aTrue

  putStrLn $ "AE control bits (as Bool list) = " ++ show ctrlBits
  putStrLn $ "Estimated aHat = " ++ show aHat
  putStrLn $ "Grover rounds from aHat: rHat = " ++ show rHat
  putStrLn $ "Grover rounds from aTrue (for comparison): rTrue = " ++ show rTrue
  putStrLn $ "AE cpu_ms = " ++ show tAE

  -- Run Grover using the estimated number of rounds, multiple rounds
  let progG = grover n oracle rHat
      psiG0 = ket (replicate n 0)

  (succCount, tG) <- timeMs $ do
    let go 0 _rng s = pure s
        go t rng s =
          let (_st, outs, rng') = Sem.evalProg progG psiG0 rng
              s' = if isMarked marked outs then s + 1 else s
          in go (t - 1) rng' s'
    go rounds rng0 0

  putStrLn $ "Grover success " ++ show succCount ++ "/" ++ show rounds
  putStrLn $ "Grover cpu_ms (all rounds) = " ++ show tG
