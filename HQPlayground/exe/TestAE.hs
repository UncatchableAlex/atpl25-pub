module Main where

import HQP
--import HQP.QOp.MatrixSemantics as Sem
import HQP.QOp.StatevectorSemantics as Sem
--import HQP.QOp.StateHmatrixSemantics as Sem
import System.Random (mkStdGen, randoms)
import Programs.Grovers (amplitudeEstimation, mcZ, bitsToInt, foldPhase, estimateA)


main :: IO ()
main = do
  let
    rng0 = randoms (mkStdGen 42) :: [Double]

    -- system size
    n = 4

    -- phase-estimation precision (control qubits)
    m = 10

    -- Pick a random "marked" solution, like before
    solution = map (fromEnum . (> 0.8)) $ take n rng0

    -- Oracle that flips phase on |solution>
    negateZeroBits = foldr (⊗) One $ map (\b -> if b == 0 then X else Id 1) solution
    oracle = negateZeroBits ∘ (mcZ n) ∘ negateZeroBits

    -- A for amplitude estimation: H^{⊗n} prepares uniform superposition
    aPrep = foldr (⊗) One (replicate n H)

    -- Build amplitude estimation program (QPE on Grover Q)
    prog = amplitudeEstimation m n aPrep oracle

  putStr $ "Running Amplitude Estimation with " ++ show (prog_qubits prog) ++ " qubits...\n"
  --putStr $ "program = " ++ showProgram prog ++ "\n\n"

  let
    -- Start in |0^(m+n)>
    psi0 = ket (replicate (m + n) 0)

    -- Run program
    (_end_state, outcomes, _rng1) = Sem.evalProg prog psi0 rng0

    -- outcomes are measurement of control register only (m qubits)
    y   = bitsToInt outcomes
    phi = fromIntegral y / (2 ^^ m)

    -- Use the shared helper to produce the amplitude estimate
    aHat = estimateA m outcomes

    -- True amplitude if A is uniform superposition and there is 1 marked item:
    -- a = 1 / 2^n
    aTrue = 1.0 / ((2 :: Double) ** fromIntegral n)

  putStr $ "Control bits (y): " ++ show y ++ " out of " ++ show (2^m) ++ "\n"
  putStr $ "Estimated phase phi: " ++ show phi ++ "\n"
  putStr $ "Estimated a_hat: " ++ show aHat ++ "\n\n"
  putStr $ "True a (1 marked out of 2^n): " ++ show aTrue ++ "\n"
  putStr $ "Marked solution: " ++ show solution ++ "\n"
