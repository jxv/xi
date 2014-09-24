{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Xi.Camera
  ( defaultOrthoCamera
  , defaultPerspectiveCamera
  , cameraProjectionMat
  , lookAt
  ) where

------------------------------------------------------------------------------------------

import Xi.Imports
import Xi.Types
import Xi.Util

------------------------------------------------------------------------------------------

defaultOrthoCamera :: Camera
defaultOrthoCamera = Camera
  { _projection = Ortho
  , _aspectRatio = 1
  , _nearDepth = 1
  , _farDepth = 20
  , _scale = 1
  , _pos = V3 0 0 0
  , _target = V3 0 0 0
  , _up = V3 0 0 0
  }

defaultPerspectiveCamera :: Camera
defaultPerspectiveCamera = Camera
  { _projection = Perspective 60
  , _aspectRatio = 1
  , _nearDepth = 1
  , _farDepth = 20
  , _scale = 1
  , _pos = V3 0 0 0
  , _target = V3 0 0 0
  , _up = V3 0 0 0
  }

frustum :: Mat4 -> F -> F -> F -> F -> F -> F -> Mat4
frustum mat left right bottom top near far
  | near <= 0 || far <= 0 || dx <= 0 || dy <= 0 || dz <= 0 = mat
  | otherwise = frustMat !*! mat
 where
  dx = right - left
  dy = top - bottom
  dz = far - near
  --
  frustMat = V4
    (V4 ((2 * near) / dx)      0                     0                       0)
    (V4 0                     ((2 * near) / dy)      0                       0)
    (V4 ((right + left) / dx) ((top + bottom) / dy) (-(near + far) / dz)     (-1))
    (V4 0                     0                     ((-2) * near * far / dz) 0)

ortho :: Mat4 -> F -> F -> F -> F -> F -> F -> Mat4
ortho mat left right bottom top near far
  | dx == 0 || dy == 0 || dz == 0 = mat
  | otherwise = orthoMat !*! mat
 where
  dx = right - left
  dy = top - bottom
  dz = far - near
  --
  orthoMat = V4
    (V4 (2 / dx)               0                      0                    0)
    (V4 0                      (2 / dy)               0                    0)
    (V4 0                      0                      (-2 / dz)            0)
    (V4 (-(right + left) / dx) (-(top + bottom) / dy) (-(near + far) / dz) 1)

lookAt :: Vec3 -> Vec3 -> Vec3 -> Mat4
lookAt pos target up =
  let z = normalize (pos - target)
      x = normalize (up `cross` z)
      y = normalize (z `cross` x)
      m = V4 (vector x) (vector y) (vector z) (V4 0 0 0 1)
      pos' = m !* (point (-pos))
      m' = (adjoint m) & _w .~ pos'
  in m'

cameraProjectionMat :: Camera -> Mat4
cameraProjectionMat Camera{ _projection = Ortho, ..} =
  let left = -_aspectRatio
      right = _aspectRatio
      bottom = -1
      top = 1
      near = _nearDepth
      far = _farDepth
  in ortho eye4 left right bottom top near far
cameraProjectionMat Camera{ _projection = Perspective fieldOfViewY, ..} =
  let height = _nearDepth * tan (fieldOfViewY / 360 * pi)
      width = _aspectRatio * height
      left = -width
      right = width
      bottom = -height
      top = height
      near = _nearDepth
      far = _farDepth
  in frustum eye4 left right bottom top near far

