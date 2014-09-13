module Xi.Util
  ( scale
  , translate
  , rotate
  ) where

------------------------------------------------------------------------------------------

import Xi.Imports
import Xi.Types

------------------------------------------------------------------------------------------

scale :: Mat4 -> F -> F -> F -> Mat4
scale (V4 x y z w) sx sy sz = V4
  (x ^* sx)
  (y ^* sy)
  (z ^* sz)
  w

translate :: Mat4 -> F -> F -> F -> Mat4
translate (V4 x y z w) tx ty tz = V4 x y z $ V4
  ((w^._x) + (x^._x) * tx + (y^._x) * ty + (z^._x) * tz)
  ((w^._y) + (x^._y) * tx + (y^._y) * ty + (z^._y) * tz)
  ((w^._z) + (x^._z) * tx + (y^._z) * ty + (z^._z) * tz)
  ((w^._w) + (x^._w) * tx + (y^._w) * ty + (z^._w) * tz)

rotate :: Mat4 -> F -> F -> F -> F -> Mat4
rotate mat angle x y z
  | mag > 0 = rotMat !*! mat
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
  rotMat = V4
    (V4 ((oneMinusCos * xx) + cosAngle) ((oneMinusCos * xy) - zs)       ((oneMinusCos * zx) + ys)       0)
    (V4 ((oneMinusCos * xy) + zs)       ((oneMinusCos * yy) + cosAngle) ((oneMinusCos * yz) - xs)       0)
    (V4 ((oneMinusCos * zx) - ys)       ((oneMinusCos * yz) + xs)       ((oneMinusCos * zz) + cosAngle) 0)
    (V4 0                               0                               0                               1)

