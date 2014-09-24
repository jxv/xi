{-# LANGUAGE TemplateHaskell #-}

module Xi.Types where

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
  { _backgroundColor :: Color4f
  , _activeBuffers   :: [Buffer]
  , _swapBuffers     :: IO ()
  }

data Projection
  = Ortho
  | Perspective F -- | Field of View
    deriving (Eq, Show)

data Camera = Camera
  { _projection  :: Projection
  , _aspectRatio :: F
  , _nearDepth   :: F
  , _farDepth    :: F
  , _scale       :: F
  , _pos         :: Vec3
  , _target      :: Vec3
  , _up          :: Vec3
  } deriving (Eq, Show)

type Draw = IO ()
data Font = Font

class Drawable d where
  draw :: d -> IO ()

makeLenses ''Xi
makeLenses ''Camera
