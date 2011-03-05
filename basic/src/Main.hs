{-# LANGUAGE BangPatterns #-}
import MC
import PRNG
import Particle
import Mesh
import Material
import Physical
import Tally
import Source
import Control.Monad
import TryNSave

import System (getArgs)
import Data.List (foldl')

main :: IO ()
main = do
  n <- parseCL
  let tally = runManyP5 infMesh simpleMat prand emptyTally n
  -- let tally = runManyP6 infMesh simpleMat n
  writeTally "tally1" tally

runManyP :: Mesh -> Material -> Word32 -> Tally
runManyP msh mat ntot = let 
  ps = genParticles ntot msh prand
  tallies = map (runParticle msh mat) ps
  in foldr merge emptyTally tallies

-- one at a time, please 
runManyP2 :: Mesh -> Material -> Word32 -> Tally
runManyP2 msh mat n = fst $ foldr history (emptyTally,prand) [1..n]
     where history i (cumTally,rng) = (merge cumTally newTally,rng')
               where (!p,!rng') = genParticle i msh rng
                     !newTally = runParticle msh mat p

runManyP3 :: Mesh -> Material -> Word32 -> Tally
runManyP3 msh mat n = fst $ foldl history (emptyTally,prand) [1..n]
     where history (cumTally,rng) i = (merge cumTally newTally,rng')
               where (p,rng') = genParticle i msh rng
                     newTally = runParticle msh mat p

runManyP4 :: Mesh -> Material -> RNG -> Word32 -> Tally
runManyP4 msh mat rng 0 = emptyTally
runManyP4 msh mat rng n = 
    merge newTally $ runManyP4 msh mat rng' (n-1)
    where (p,rng') = genParticle n msh rng
          newTally = runParticle msh mat p

-- previous chump wasn't tail recursive...sweet!! ~10x --strictness hurts here
-- But wasn't runManyP3 tail recursive? foldl'?
runManyP5 :: Mesh -> Material -> RNG -> Tally -> Word32 -> Tally
runManyP5 msh mat rng t 0 = t
runManyP5 msh mat rng t n = 
    let (p,rng') = genParticle n msh rng
        newTally = runParticle msh mat p
        t' = merge newTally t
    in runManyP5 msh mat rng' t' (n-1)

runManyP6 :: Mesh -> Material -> Word32 -> Tally
runManyP6 msh mat n = fst $ foldl' history (emptyTally,prand) [1..n]
     where history (cumTally,rng) i = (merge cumTally newTally,rng')
               where (p,rng') = genParticle i msh rng
                     newTally = runParticle msh mat p

-- mshType = Cart1D
mshType = Sphere1D

infMesh :: Mesh
infMesh = mshType $ listArray (1,2)
          [CellProps (Position 0.0) (Position 1.0) (bc1D Refl) (bc1D Transp),
           CellProps (Position 1.0) (Position 2.0) (bc1D Transp) (bc1D Refl)]

-- try tuning the scattering and absorption opacities in each cell
simpleMat :: Material
simpleMat =  
  Material $ listArray (1,2) 
    [ MatState (Opacity 0.1) (Opacity 2.0) (Velocity 0.0) (Temperature 1.0),
      MatState (Opacity 1.0) (Opacity 0.5) (Velocity 0.0) (Temperature 2)]

parseCL :: IO Word32
parseCL = do
  args <- getArgs
  let n  = head args
      wn = read n :: Word32
  if wn <= 0
     then error ("first command line argument (n) must be greater than zero,"++
              "you specified n = " ++ show wn ++ " which is obviously <= 0.")
     else return wn
     
-- End of file
