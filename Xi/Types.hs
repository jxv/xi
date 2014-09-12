module Xi.Types
  ( Pos2
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

import Graphics.Rendering.OpenGL.Raw
import qualified Linear as L

type Pos2 = L.V2 GLfloat
type Pos3 = L.V3 GLfloat

type Vec2 = L.V2 GLfloat
type Vec3 = L.V3 GLfloat
type Vec4 = L.V4 GLfloat

type Mat2 = L.M22 GLfloat
type Mat3 = L.M33 GLfloat
type Mat4 = L.M44 GLfloat

type Color4f = (GLclampf, GLclampf, GLclampf, GLclampf)

data Xi = Xi
  { xiClearColor      :: Color4f
  , xiClearBufferBits :: GLenum
  , xiSwapBuffers     :: IO ()
  , xiModelViewMat    :: Mat4
  , xiProjectionMat   :: Mat4
  }

data Projection
  = Ortho
  | Perspective GLfloat -- | Field of View

data Camera = Camera
  { cameraProjection   :: Projection
  , cameraScreenWidth  :: GLfloat
  , cameraScreenHeight :: GLfloat
  , cameraNear         :: GLfloat
  , cameraFar          :: GLfloat
  , cameraPosition     :: Vec3
  , cameraTarget       :: Vec3
  , cameraUp           :: Vec3
  }



