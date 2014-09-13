{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Xi.Camera
  ( cameraProjectionMat
  ) where

------------------------------------------------------------------------------------------

import Control.Lens
import Graphics.Rendering.OpenGL.Raw

import qualified Linear as L

------------------------------------------------------------------------------------------

import Xi.Types
import Xi.Util

------------------------------------------------------------------------------------------

frustum :: Mat4 -> F -> F -> F -> F -> F -> F -> Mat4
frustum mat left right bottom top nearZ farZ
  | nearZ <= 0 || farZ <= 0 || dx <= 0 || dy <= 0 || dz <= 0 = mat
  | otherwise = frustMat L.!*! mat
 where
  dx = right - left
  dy = top - bottom
  dz = farZ - nearZ
  --
  frustMat = L.V4
    (L.V4 ((2 * nearZ) / dx)      0                     0                          0)
    (L.V4 0                     ((2 * nearZ) / dy)      0                          0)
    (L.V4 ((right + left) / dx) ((top + bottom) / dy) (-(nearZ + farZ) / dz)     (-1))
    (L.V4 0                     0                     ((-2) * nearZ * farZ / dz) 0)

ortho :: Mat4 -> F -> F -> F -> F -> F -> F -> Mat4
ortho mat left right bottom top nearZ farZ
  | dx == 0 || dy == 0 || dz == 0 = mat
  | otherwise = orthoMat L.!*! mat
 where
  dx = right - left
  dy = top - bottom
  dz = farZ - nearZ
  --
  orthoMat = L.V4
    (L.V4 (2 / dx)               0                      0                      0)
    (L.V4 0                      (2 / dy)               0                      0)
    (L.V4 0                      0                      (-2 / dz)              0)
    (L.V4 (-(right + left) / dx) (-(top + bottom) / dy) (-(nearZ + farZ) / dz) 1)

cameraProjectionMat :: Camera -> Mat4
cameraProjectionMat Camera{ cameraProjection = Ortho, ..} =
  let left = -cameraAspectRatio
      right = cameraAspectRatio
      bottom = -1
      top = 1
      near = cameraNear
      far = cameraFar
  in ortho L.eye4 left right bottom top near far
cameraProjectionMat Camera{ cameraProjection = Perspective fieldOfViewY, ..} =
  let height = cameraNear * tan (fieldOfViewY / 360 * pi)
      width = cameraAspectRatio * height
      left = -width
      right = width
      bottom = -height
      top = height
      near = cameraNear
      far = cameraFar
  in frustum L.eye4 left right bottom top near far

