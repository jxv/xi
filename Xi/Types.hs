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
  , Camera(..)
  , Projection(..)
  ) where

import Foreign.C.Types
import Graphics.Rendering.OpenGL.Raw

import qualified Linear as L

type F = CFloat

type Pos2 = L.V2 F
type Pos3 = L.V3 F

type Vec2 = L.V2 F
type Vec3 = L.V3 F
type Vec4 = L.V4 F

type Mat2 = L.M22 F
type Mat3 = L.M33 F
type Mat4 = L.M44 F

type Color4f = (F, F, F, F)

data Xi = Xi
  { xiClearColor      :: Color4f
  , xiClearBufferBits :: GLenum
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

