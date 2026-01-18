module Main where

import HQP
--import HQP.QOp.MatrixSemantics as Sem
import HQP.QOp.StatevectorSemantics as Sem
--import HQP.QOp.StateHmatrixSemantics as Sem
import System.Random (mkStdGen, randoms)
import Text.Printf (printf)
import System.CPUTime (getCPUTime)
import Control.Exception (evaluate)
import Control.Monad (forM_)
-- import System.IO (withFile, IOMode(WriteMode), stdout, hFlush)
-- import GHC.IO.Handle (hDuplicate, hDuplicateTo)


import Programs.Grovers
  ( amplitudeEstimation
  , mcZ
  , bitsToInt
  , estimateA
  )

-- Build oracle that flips phase on |solution>
mkOracle :: Int -> [Int] -> QOp
mkOracle n solution =
  let negateZeroBits =
        foldr (⊗) One [ if b == 0 then X else Id 1 | b <- solution ]
  in negateZeroBits ∘ mcZ n ∘ negateZeroBits

-- Uniform A = H^{⊗n}
mkAPrep :: Int -> QOp
mkAPrep n = foldr (⊗) One (replicate n H)

mkSolution :: Int -> [Double] -> [Int]
mkSolution n xs = map (fromEnum . (> 0.8)) (take n xs)

bitsStr :: [Int] -> String
bitsStr = concatMap show

runCase :: Int -> Int -> Int -> IO ()
runCase seed n m = do
  t0 <- getCPUTime

  let rng0 = randoms (mkStdGen seed) :: [Double]
      (solRng, simRng) = splitAt n rng0

      solution = mkSolution n solRng
      oracle   = mkOracle n solution
      aPrep    = mkAPrep n
      prog     = amplitudeEstimation m n aPrep oracle
      psi0     = ket (replicate (m + n) 0) -- this represents |0^(m+n)>

      (_endState, outs, _rng1) = Sem.evalProg prog psi0 simRng

      aTrue :: Double
      aTrue = 1.0 / ((2 :: Double) ** fromIntegral n)

      denom :: Double
      denom = (2 :: Double) ** fromIntegral m

      -- As returned
      y    = bitsToInt outs
      phi  :: Double
      phi  = fromIntegral y / denom
      aHat :: Double
      aHat = estimateA m outs
      err  :: Double
      err  = abs (aHat - aTrue)

      -- -- Reversed (bit-order check)
      -- outsR = reverse outs
      -- yR    = bitsToInt outsR
      -- phiR  :: Double
      -- phiR  = fromIntegral yR / denom
      -- aHatR :: Double
      -- aHatR = estimateA m outsR
      -- errR  :: Double
      -- errR  = abs (aHatR - aTrue)

  _ <- evaluate err -- force evaluation before timing
  t1 <- getCPUTime

  let cpuMs :: Double
      cpuMs = fromIntegral (t1 - t0) / 1e9  -- ps -> ms

  --printf "%d,%d,%d,%s,%d,%.10f,%.10f,%.10f,%.10f,%d,%.10f,%.10f,%.10f,%.3f\n"
  printf "%d,%d,%d,%s,%d,%.10f,%.10f,%.10f,%.10f,%.3f\n"
    seed n m (bitsStr solution)
    y  phi  aHat  err  aTrue
    -- yR phiR aHatR errR
    cpuMs


main :: IO ()
main = do
  let seeds = [4, 2, 42]
      ns    = [3, 4, 5]
      ms    = [5..10]

  putStrLn "seed,n,m,solution,y,phi,aHat,absErr,aTrue,cpu_ms"

  forM_ seeds $ \seed ->
    forM_ ns $ \n ->
      forM_ ms $ \m ->
        runCase seed n m

-- runToCsv :: FilePath -> IO ()
-- runToCsv fp =
--   withFile fp WriteMode $ \h -> do
--     old <- hDuplicate stdout
--     hDuplicateTo h stdout
--     main
--     hFlush stdout
--     hDuplicateTo old stdout