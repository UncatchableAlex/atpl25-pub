module Programs.Grovers (grover) where
import HQP
import Debug.Trace(trace)

-- Goal: Given a function (f: [2^n] -> {0,1}) that returns 1 for "correct" answers,
-- find an x such that f(x) = 1 using roughly 2^n queries on f. 

-- n: the number of q bits we use (should be the lg of the domain size of f)
-- oracle: our oracle function. oracle|x> = -|x> if f(x) = 1 and oracle|x> = |x> if f(x) = 0.
-- rounds: the number of grover iterations we repeat for
grover :: Nat -> QOp -> Nat -> Program
grover n oracle rounds = 
    let
        -- starting state 
        bigA = (foldr (⊗) One (replicate n H))

        xAll = (foldr (⊗) One (replicate n X))           

        -- phase flip on the zero vector
        sZero = xAll ∘ (mcZ n) ∘ xAll 
        
        -- our oracle operator
        sx = oracle  

        -- magical diffusion operation
        gdiffusion = bigA ∘ sZero ∘ bigA
        
        -- Apply the oracle to our vector of length n, then apply the grover diffusion operator
        giteration = gdiffusion ∘ sx
    in
        [
            -- set all of our bits to 0s
            Initialize [0..n-1] (replicate n False),

            -- builds H^{⊗n). Basically n H gates tensored together get the equilibrium state
            Unitary bigA,

            -- Repeat the grover iterations
            Unitary $ foldr (∘) (Id n) (replicate rounds giteration),

            -- Measure all of the qubits
            Measure [0..n-1]
        ]
        
-- Multi-controlled Z: flips sign when all n qubits are |1⟩
mcZ :: Int -> QOp
mcZ 1 = Z
mcZ n = C (mcZ (n - 1))
