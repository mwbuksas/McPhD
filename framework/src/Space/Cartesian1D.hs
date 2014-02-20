{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module Space.Cartesian1D where

import Data.Vector.V2

import Space.Classes
import Approx
import Properties
import NormalizedValues


data Cartesian1D = Cartesian1D { cart1d_pos :: Double
                               , cart1d_dir :: Normalized Vector2 
                               }
                   deriving (Eq, Show)

instance Space Cartesian1D where
    type Position  Cartesian1D = Double
    type Direction Cartesian1D = Normalized Vector2
    type Velocity  Cartesian1D = Double

    position  = cart1d_pos
    direction = cart1d_dir

    stream (Cartesian1D pos dir) (Distance d)
        = Cartesian1D (pos + d*dx) dir
          where dx = v2x $ normalized_value dir

instance Approx Cartesian1D where
    within_eps epsilon (Cartesian1D x1 d1) (Cartesian1D x2 d2) =
      (within_eps epsilon x1 x2) && (within_eps epsilon d1 d2)
