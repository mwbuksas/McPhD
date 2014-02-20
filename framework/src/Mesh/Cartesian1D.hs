{-# LANGUAGE TypeFamilies #-}

module Mesh.Cartesian1D where

import Data.Sequence as Seq
import Data.Vector.V2

import Mesh.Classes

import Space.Cartesian1D
import Space.Classes
import NormalizedValues
import RandomSamples
import Approx
import Properties

type C1Cell = Int
data C1Dir  = Negative | Positive deriving (Show, Eq)

data Cartesian1DMesh = Cartesian1DMesh
                       {
                         coords  :: Seq Double
                       , low_bc  :: BoundaryCondition
                       , high_bc :: BoundaryCondition
                       } deriving Show

instance Mesh Cartesian1DMesh where
  type MeshCell Cartesian1DMesh  = C1Cell
  type MeshFace Cartesian1DMesh  = C1Dir
  type MeshSpace Cartesian1DMesh = Cartesian1D

  size mesh = (Seq.length $ coords mesh) - 1

  cell_find mesh location =
    let bounds       = coords mesh
        _ :< boundsR = viewl bounds
        pairs        = Seq.zip bounds (boundsR)
    in Seq.findIndexL (cellBoundsTest (==) location) pairs

  cell_neighbor mesh cell Negative
    | cell == 0 = boundary2neighbor $ low_bc mesh
    | otherwise = Cell (cell-1)
  cell_neighbor mesh cell Positive
    | cell == (size mesh)-1 = boundary2neighbor $ high_bc mesh
    | otherwise = Cell (cell+1)

  cell_neighbors mesh cell = [(Negative, cell_neighbor mesh cell Negative),
                              (Positive, cell_neighbor mesh cell Positive)]

  is_in_cell               = inCellTest (==)
  is_approx_in_cell        = inCellTest (~==)
  is_in_mesh mesh location = cellBoundsTest (==) location (left, right)
      where (left :< rest)  = viewl $ coords mesh
            (  _  :> right) = viewr rest

  uniform_sample mesh rand =
      let (position, rand')   = sampleInterval (bounds mesh) rand
          (direction, rand'') = sampleNormalVector2 rand'
      in (Cartesian1D position direction, rand'')

  uniform_sample_cell mesh cell rand =
      let (position, rand')   = sampleInterval (cellBounds mesh cell) rand
          (direction, rand'') = sampleNormalVector2 rand'
      in (Cartesian1D position direction, rand'')

  cell_boundary mesh cell location (Distance distance) =
      let bounds  = cellBounds mesh cell
          cos_dir = v2x $ normalized_value $ direction location
          (x_distance, face) = if cos_dir > 0
                               then (snd bounds - position location, Positive)
                               else (fst bounds - position location, Negative)
      in if abs x_distance < distance * abs cos_dir
         then Just ( Distance (x_distance / cos_dir), face)
         else Nothing



lowerBound :: Cartesian1DMesh -> Double
lowerBound = fst . bounds

upperBound :: Cartesian1DMesh -> Double
upperBound = snd . bounds

width :: Cartesian1DMesh -> Double
width mesh = upperBound mesh - lowerBound mesh

bounds :: Cartesian1DMesh -> (Double, Double)
bounds mesh = (left, right)
    where (left :< _)  = viewl $ coords mesh
          (_ :> right) = viewr $ coords mesh


cellBounds :: Cartesian1DMesh -> C1Cell -> (Double, Double)
cellBounds mesh cell = (coords mesh `Seq.index` cell,
                        coords mesh `Seq.index` (cell+1))

-- | Determine if a position is between to values. Uses direction to break ties.
cellBoundsTest :: (Double -> Double -> Bool)
                  -> Cartesian1D
                  -> (Double, Double)
                  -> Bool
cellBoundsTest comp location (xMin, xMax) =
  let x       = position location
      cos_dir = v2x . normalized_value $ direction location
  in ((x > xMin) || ( (x `comp` xMin) && cos_dir >= 0)) &&
     ((x < xMax) || ( (x `comp` xMax) && cos_dir < 0))


inCellTest :: (Double -> Double -> Bool)
              -> Cartesian1DMesh
              -> C1Cell
              -> Cartesian1D
              -> Bool
inCellTest comp mesh cell location =
  cellBoundsTest comp location (cellBounds mesh cell)
