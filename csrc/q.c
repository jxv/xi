#include "q.h"

#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <stdbool.h>


DEFINE_Q(f, float, sinf, cosf, tanf, acosf)
DEFINE_Q(i, int, sinf, cosf, tanf, acosf)
DEFINE_Q(d, double, sin, cos, tan, acos)
