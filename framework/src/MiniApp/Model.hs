{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, DatatypeContexts #-}

module MiniApp.Model where
{-| MiniApp Model

Defines the Model, which is the combination of data and operations
which are sufficient to determine the next event in a particle history.

Note that while this model is written to be polymorphic over the mesh,
this is not required in general.

-}

import Data.Array.IArray
import Data.Function
import Data.List

import Mesh.Classes
import qualified Particle.Classes as P

import Properties
import qualified MonteCarlo as MC

import MiniApp.Particle
import MiniApp.Events
import MiniApp.Physics


-- | The physical model. Consists of a mesh, space properties indexed
-- by mesh cell and the end of time-step
data (Mesh m) => Model m = Model {
      mesh    :: m
    , physics :: Array (MeshCell m) (Data (MeshSpace m))
    , t_final :: Time
    }

-- | Extract the physics data for the Particle's cell.
localPhysics :: (Mesh m) => Model m -> Particle m -> Data (MeshSpace m)
localPhysics model particle = (physics model) ! (cell particle)


-- | Contractors are functions which take a model, a particle and
-- return a candidate Outcome for stepping the particle.
type Contractor model event particle
    = model -> particle -> (event, particle)

-- | Compute outcomes from contractors, and choose the closest one.
step :: (Ord event) => model
     -> [Contractor model event particle]
     -> particle
     -> (event, particle)

step model contractors part
    = minimumBy (compare `on` fst) (map (\f -> f model part) contractors)

-- * Functions which compute outcomes, resulting from interaction with
-- the mesh, the timestep and the medium. Each of these has the same
-- type, called Contractor:
--   Model m -> Particle m -> Outcome m

-- | Compute an Outcome for reaching the timestep end.
timeStepContractor :: (Mesh m) => Contractor (Model m) (Event m) (Particle m)
timeStepContractor model particle =
    let time_left = t_final model - time particle
        distance  = gettingTo time_left (speed particle)
        particle' = P.move particle distance
    in (Event distance Timeout, particle')

-- | Contractor face and boundary crossings in the mesh.
meshContractor = undefined

-- | Contractor for scattering and absorption events.
materialContractor = undefined


-- | A list of contractors that we hand to the step function.
contractors :: (Mesh m) => [Contractor (Model m) (Event m) (Particle m)]
contractors = [timeStepContractor]




