module Xi.Util
  ( scale
  , translate
  , rotate
  ) where

------------------------------------------------------------------------------------------

import Control.Lens
import Graphics.Rendering.OpenGL.Raw

import qualified Linear as L

------------------------------------------------------------------------------------------

import Xi.Types

------------------------------------------------------------------------------------------

scale :: Mat4 -> F -> F -> F -> Mat4
scale (L.V4 x y z w) sx sy sz = L.V4
  (x L.^* sx)
  (y L.^* sy)
  (z L.^* sz)
  w

translate :: Mat4 -> F -> F -> F -> Mat4
translate (L.V4 x y z w) tx ty tz = L.V4 x y z $ L.V4
  ((w^.L._x) + (x^.L._x) * tx + (y^.L._x) * ty + (z^.L._x) * tz)
  ((w^.L._y) + (x^.L._y) * tx + (y^.L._y) * ty + (z^.L._y) * tz)
  ((w^.L._z) + (x^.L._z) * tx + (y^.L._z) * ty + (z^.L._z) * tz)
  ((w^.L._w) + (x^.L._w) * tx + (y^.L._w) * ty + (z^.L._w) * tz)

rotate :: Mat4 -> F -> F -> F -> F -> Mat4
rotate mat angle x y z
  | mag > 0 = rotMat L.!*! mat
  | otherwise = mat
 where
  sinAngle = sin $ angle * pi / 180
  cosAngle = cos $ angle * pi / 180
  mag = sqrt $ x * x + y * y + z * z
  --
  x' = x / mag
  y' = y / mag
  z' = z / mag
  --
  xx = x' * x'
  yy = y' * y'
  zz = z' * z'
  xy = x' * y'
  yz = y' * z'
  zx = z' * x'
  xs = x' * sinAngle
  ys = y' * sinAngle
  zs = z' * sinAngle
  oneMinusCos = 1 - cosAngle
  --
  rotMat = L.V4
    (L.V4 ((oneMinusCos * xx) + cosAngle) ((oneMinusCos * xy) - zs)       ((oneMinusCos * zx) + ys)       0)
    (L.V4 ((oneMinusCos * xy) + zs)       ((oneMinusCos * yy) + cosAngle) ((oneMinusCos * yz) - xs)       0)
    (L.V4 ((oneMinusCos * zx) - ys)       ((oneMinusCos * yz) + xs)       ((oneMinusCos * zz) + cosAngle) 0)
    (L.V4 0                               0                               0                               1)

