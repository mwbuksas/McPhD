{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Data types for Three-dimensonal space in Cartesian coordinates

module Space3DCartesian(Distance (..)
			, Position (..)
			, Momentum (..)
			, Motion (..), motion, move
			, Direction (), dir, direction, direction_unsafe, (*->), (+->)
			, Time (..)
			, Speed (..)
			, timeToDistance
			, distanceToTime
			, translate
			) where

import Data.Vector.V3
import Data.Vector.Class

import Data.Array.IArray

import Approx
import NumUnit

-- * Data types

-- | Coordinates
data Coord = X | Y | Z deriving (Show, Eq, Ord, Ix)
-- coords = listArray (X,Z) "xyz" :: Array Coord Char

-- | Position, a 3D vector
newtype Position = Position { pos :: Vector3 } deriving (Eq, Show, Num, Approx)
-- | Momentum, a 3D vector

newtype Momentum = Momentum { mom :: Vector3 } deriving (Eq, Show, Num, Approx)

-- | Motion, a 3D vector
newtype Motion = Motion { mot :: Vector3 } deriving (Eq, Show, Num, Approx)

-- | Distance, scalar
newtype Distance = Distance { dis :: Double  } deriving (Eq, Show, Num, Ord, Approx)

-- | Direction, a 3D vector of magnitude 1.
newtype Direction = Direction { dir :: Vector3 } deriving (Eq, Show, Num, Approx)

-- | Time, elapsed time from beginning of streaming
newtype Time = Time { time :: Double } deriving (Eq, Show, Num, Ord, Approx)

-- | A scalar representing the magnitude of velocity.
newtype Speed = Speed { speed :: Double } deriving (Eq, Show, Num, Ord, Approx)


-- * Construction and manupulation of space quantities

-- | Normalizes the vector when creating a Direction
direction :: Vector3 -> Direction
direction v = Direction (vnormalise v)

-- | User attests that the vector is already normalised
direction_unsafe :: Vector3 -> Direction
direction_unsafe = Direction

-- | Defines normalize operation for Directions
instance NumUnit Direction where
  normalize  = direction  . dir
  magnitude  = magnitude  . dir
  magnitude2 = magnitude2 . dir

-- | Create motion from a distance and direction
motion :: Direction -> Distance -> Motion
motion (Direction dir) (Distance dist) = Motion (dist *| dir)

infix 7 *->
(*->) :: Direction -> Distance -> Motion
(*->) direction distance = motion direction distance

-- | Applies Motion to a Position to find a new Position
move :: Position -> Motion -> Position
move (Position p) (Motion m) = Position (p + m)

infix 6 +->
(+->) :: Position -> Motion -> Position
(+->) position motion = move position motion

-- | Translates object along the direction vector
translate :: Position -- ^ Initial position
	     -> Direction -- ^ Direction vector
	     -> Distance  -- ^ Movement distance
	     -> Position  -- ^ New position
translate position direction distance = position +-> (direction *-> distance)

timeToDistance :: Distance -> Speed -> Time
timeToDistance (Distance distance) (Speed speed) = Time (distance / speed)

distanceToTime :: Time -> Speed -> Distance
distanceToTime (Time time) (Speed speed) = Distance (time * speed)