module Main where

import HQP
import HQP.QOp.MatrixSemantics as MS
import System.Random(mkStdGen, randoms)
import Programs.Grovers (grover, mcZ)     


main :: IO ()
main = do
    let
        rng0 = randoms (mkStdGen 4) :: [Double]  

        rounds = 5
        n = 7

        -- what is the "solution?"
        solution = map (fromEnum . (> 0.8)) $ take n rng0 

        -- construct the oracle for the solution
        negateZeroBits = foldr (⊗) One $ map (\b -> if b == 0 then X else Id 1) solution
        oracle = negateZeroBits ∘ (mcZ n) ∘ negateZeroBits

        -- get our program for grover's algorithm
        prog = grover n oracle rounds

        -- get our starting state:
        psi0 = ket (replicate n 0)

        -- run our program:
        (end_state, outcomes,_) = evalProg prog psi0 rng0

    --putStr $ "End State: " ++ (show end_state) ++ "\n\n"
    putStr $ "Found solution: " ++ (show (map fromEnum outcomes)) ++ "\n\n"
    putStr $ "Actual Solution: " ++ (show $ solution) ++ "\n\n"
