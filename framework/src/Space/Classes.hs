{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

module Space.Classes where

import Properties
import Approx ()

{-| A typeclass for types which model a coordinate space, and it's
representation as data types.

A space is defined by three types, which describe positions,
directions and velocities. A point in the space is a value of position
and direction.  We're a little vague on the distinction between the
space and points in the space.

Two operations extract information from the point: position, and
direction.

Function 'make' is used to construct a point from the seperate
position and velocity

Function 'stream' translates a point from it's current position by a
given distance in it's current direction to create a new point.

Function 'scale' is a special operation which converts a direction to a velocity.

-}
class Space s where
  type Position  s :: *
  type Direction s :: *
  type Velocity  s :: *
  position  :: s -> Position s
  direction :: s -> Direction s
  stream    :: s -> Distance -> s
--  scale     :: s -> Direction s -> Double -> Velocity s
  make      :: Position s -> Direction s -> s

-- | Infix operator for streaming.
infix 6 +->
(+->) :: (Space s) => s -> Distance -> s
(+->) = stream
