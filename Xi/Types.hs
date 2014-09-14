module Xi.Types
  ( F
  , Pos2
  , Pos3
  , Vec2
  , Vec3
  , Vec4
  , Mat2
  , Mat3
  , Mat4
  , Xi(..)
  , Buffer(..)
  , Camera(..)
  , Projection(..)
  ) where

------------------------------------------------------------------------------------------

import Xi.Imports

------------------------------------------------------------------------------------------

type F = CFloat

type Pos2 = V2 F
type Pos3 = V3 F

type Vec2 = V2 F
type Vec3 = V3 F
type Vec4 = V4 F

type Mat2 = M22 F
type Mat3 = M33 F
type Mat4 = M44 F

type Color4f = (F, F, F, F)

data Buffer
  = ColorBuffer
  | DepthBuffer
  | StencilBuffer
  deriving (Show, Eq)

data Xi = Xi
  { xiBackgroundColor :: Color4f
  , xiActiveBuffers   :: [Buffer]
  , xiSwapBuffers     :: IO ()
  , xiModelViewMat    :: Mat4
  , xiProjectionMat   :: Mat4
  }

data Projection
  = Ortho
  | Perspective F -- | Field of View

data Camera = Camera
  { cameraProjection  :: Projection
  , cameraAspectRatio :: F
  , cameraNear        :: F
  , cameraFar         :: F
  , cameraPosition    :: Vec3
  , cameraTarget      :: Vec3
  , cameraUp          :: Vec3
  }

