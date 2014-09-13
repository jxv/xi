{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Xi.Camera
  ( cameraProjectionMat
  ) where

------------------------------------------------------------------------------------------

import Xi.Imports
import Xi.Types
import Xi.Util

------------------------------------------------------------------------------------------

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

cameraProjectionMat :: Camera -> Mat4
cameraProjectionMat Camera{ cameraProjection = Ortho, ..} =
  let left = -cameraAspectRatio
      right = cameraAspectRatio
      bottom = -1
      top = 1
      near = cameraNear
      far = cameraFar
  in ortho eye4 left right bottom top near far
cameraProjectionMat Camera{ cameraProjection = Perspective fieldOfViewY, ..} =
  let height = cameraNear * tan (fieldOfViewY / 360 * pi)
      width = cameraAspectRatio * height
      left = -width
      right = width
      bottom = -height
      top = height
      near = cameraNear
      far = cameraFar
  in frustum eye4 left right bottom top near far

