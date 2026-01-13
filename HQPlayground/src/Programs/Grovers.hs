module Programs.Grovers (grover, amplitudeEstimation, mcZ, bitsToInt, foldPhase, estimateA) where
import HQP
import Programs.QFT
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

-- m = number of control qubits (precision)
-- n = number of system qubits
-- a = state preparation A on n qubits (for Grover, this is H^{⊗n})
-- oracle = phase oracle on the system (same kind you already build)
amplitudeEstimation :: Int -> Int -> QOp -> QOp -> Program
amplitudeEstimation m n a oracle =
  let
    -- build Grover diffusion using your A (not necessarily H^⊗n)
    xAll    = foldr (⊗) One (replicate n X)
    sZero   = xAll ∘ (mcZ n) ∘ xAll
    gDiff   = a ∘ sZero ∘ (Adjoint a)

    -- Grover iterate Q (this is what QPE estimates the phase of)
    q       = gDiff ∘ oracle

    -- -- starting state 
    -- bigA = (foldr (⊗) One (replicate n H))

    -- xAll = (foldr (⊗) One (replicate n X))    

    -- -- phase flip on the zero vector
    -- sZero = xAll ∘ (mcZ n) ∘ xAll 
    
    -- -- our oracle operator
    -- sx = oracle  

    -- -- magical diffusion operation
    -- gdiffusion = bigA ∘ sZero ∘ bigA
    
    -- -- Apply the oracle to our vector of length n, then apply the grover diffusion operator
    -- q = gdiffusion ∘ sx

    -- gates on full register (m control + n system)
    hCtrl   = (foldr (⊗) One (replicate m H)) ⊗ (Id n)
    prepA   = (Id m) ⊗ a

    ctrlPows =
      foldr (∘) (Id (m+n)) [ cQpow m n j q | j <- [0..m-1] ]

    invQFT  = (iqft m) ⊗ (Id n)
  in
    [ Initialize [0..m+n-1] (replicate (m+n) False)
    , Unitary $ cleanop $ hCtrl >: prepA >: ctrlPows >: invQFT
    , Measure [0..m-1]          -- measure control register only
    ]
        
-- Multi-controlled Z: flips sign when all n qubits are |1⟩
mcZ :: Int -> QOp
mcZ 1 = Z
mcZ n = C (mcZ (n - 1))

iqft :: Int -> QOp
--iqft m = cleanop (Adjoint (qft m))
iqft m = Adjoint (qftrev m)

-- Q^k (naive composition)
pow :: Int -> QOp -> QOp
pow k u = foldr (∘) (Id (op_qubits u)) (replicate k u)

-- controlled Q^(2^j) where control is qubit j in the control register
-- total register is: [control m qubits] ⊗ [system n qubits]
cQpow :: Int -> Int -> Int -> QOp -> QOp
cQpow m n j q =
  (Id j) ⊗ C ( (Id (m - j - 1)) ⊗ pow (2^j) q )

bitsToInt :: [Bool] -> Int
bitsToInt = foldl (\acc b -> acc*2 + fromEnum b) 0

foldPhase :: Double -> Double
foldPhase x =
  let y = x - fromIntegral (floor x :: Int)   -- mod 1 into [0,1)
  in min y (1 - y)

estimateA :: Int -> [Bool] -> Double
estimateA m outs =
  let y   = bitsToInt outs
      phi = fromIntegral y / (2 ^^ m)

      -- option 1: no half shift
      aNoShift = (sin (pi * foldPhase phi)) ** 2

      -- option 2: with half shift (handles the “-Q” convention)
      aShift   = (sin (pi * foldPhase (phi - 0.5))) ** 2

  in min aNoShift aShift   -- for your “one marked item” tests, pick the small one

