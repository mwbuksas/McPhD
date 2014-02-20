{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, DatatypeContexts #-}

{-| Datatypes for events generated during simulation.

Events of particle motion fall into categories:

 - Collisions: Consist of various kinds of physical scattering and absoroption

 - Boundaries: Geometric interfaces, e.g. cells, mesh boundaries.

 - Timeout: Particle reached end of the time-step.
-}
module MiniApp.Events where

import Data.Function

import Mesh.Classes hiding (Cell)
import Properties

import MiniApp.Physics

-- | Type of Scattering Events
data CollideType = Scatter | Absorb  deriving (Eq, Show, Ord)  -- More kinds to come.
finalCollision :: CollideType -> Bool
finalCollision Scatter = False
finalCollision Absorb  = True

-- | Type of Boundary Events
data BoundaryType = Cell | Escape | Reflect deriving (Eq, Show, Ord)
finalBoundary :: BoundaryType -> Bool
finalBoundary Cell    = False
finalBoundary Escape  = True
finalBoundary Reflect = False


-- | A datatype of all the things that can happen to the particle,
-- along with the information we need to store.
data (Mesh m) => Outcome m = Collide  { collideType   :: CollideType
                                      , deltaMomentum :: Momentum (MeshSpace m)
                                      , energyDep     :: Energy
                                      }
                           | Boundary { boundaryType  :: BoundaryType
                                      , faceIndex     :: MeshFace m
                                      }
                           | Timeout

deriving instance (Mesh m, Show m
                  , Show (MeshSpace m)
                  , Show (MeshFace m)
                  , Show (Momentum (MeshSpace m))) => Show (Outcome m)


-- | Events are the possible particle outcomes, paired with a distance of travel.
data (Mesh m) => Event m = Event { distance :: !Distance , outcome :: Outcome m }

-- | We compare events by their distance
instance (Mesh m) => Eq (Event m) where
  (==) = (==) `on` distance

instance (Mesh m) => Ord (Event m) where
  compare = compare `on` distance

deriving instance (Mesh m, Show m
                  , Show (MeshSpace m)
                  , Show (MeshFace m)
                  , Show (Momentum (MeshSpace m))) => Show (Event m)


-- | Returns True if the event stops the particle streaming.  I don't
-- use a catch-all pattern because I want to be warned if this list is
-- inexhaustive
isFinalOutcome :: (Mesh m) => Outcome m -> Bool
isFinalOutcome Timeout        = True
isFinalOutcome (c@Collide{})  = finalCollision $ collideType c
isFinalOutcome (b@Boundary{}) = finalBoundary  $ boundaryType b

isStopper :: (Mesh m) => Event m -> Bool
isStopper (Event _ outcome) = isFinalOutcome outcome
