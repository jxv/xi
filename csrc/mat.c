#include "mat.h"

#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <stdbool.h>


DEFINE_MATX(f, float, sinf, cosf, tanf)
DEFINE_MATX(i, int, sinf, cosf, tanf)
DEFINE_MATX(d, double, sin, cos, tan)
