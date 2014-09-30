#ifndef Q_H
#define Q_H

#include "vec.h"
#include "mat.h"

#define DECLARE_Q(abbr, type)	\
typedef vec4##abbr q##abbr;	\


#define PROTOTYPE_Q(abbr, type)	\
static q##abbr q##abbr##_cons(type x, type y, type z, type w);	\
static void q##abbr##_slerp(q##abbr *a, q##abbr *b, q##abbr *c, type t);	\
static void q##abbr##_rotate(q##abbr *a, q##abbr *b, q##abbr *c);	\
static void q##abbr##_rotated(q##abbr *a, q##abbr *b);	\
static void q##abbr##_scale(q##abbr *a, q##abbr *b, type s);	\
static void q##abbr##_scaled(q##abbr *a, type s);	\
static type q##abbr##_dot(q##abbr *a, q##abbr *b);	\
static void q##abbr##_to_mat3##abbr(mat3##abbr *m, q##abbr *a);	\
static void q##abbr##_to_vec4##abbr(vec4##abbr *v, q##abbr *a);	\
static void q##abbr##_add(q##abbr *a, q##abbr *b, q##abbr *c);	\
static void q##abbr##_added(q##abbr *a, q##abbr *b);	\
static void q##abbr##_subtract(q##abbr *a, q##abbr *b, q##abbr *c);	\
static void q##abbr##_subtracted(q##abbr *a, q##abbr *b);	\
static bool q##abbr##_equal(q##abbr *a, q##abbr *b);	\
static void q##abbr##_normalize(q##abbr *a, q##abbr *b);	\
static void q##abbr##_normalized(q##abbr *a);	\
static void q##abbr##_from_vec3##abbr(q##abbr *a, vec3##abbr *v, vec3##abbr *u);	\
static void q##abbr##_from_angle(q##abbr *a, vec3##abbr *v, float radians);	\
static type * q##abbr##_ptr(q##abbr *a);	\


#define DEFINE_Q(abbr, type, square_root, sine, cosine, arc_cosine)	\
static inline q##abbr q##abbr##_cons(type x, type y, type z, type w) {	\
	q##abbr a;	\
	a.x = x;	\
	a.y = y;	\
	a.z = z;	\
	return a;	\
}	\
static void q##abbr##_slerp(q##abbr *a, q##abbr *b, q##abbr *c, type t) {	\
	type theta, theta_;	\
	q##abbr q, p;	\
	type err = 0.00005f;	\
	type d = q##abbr##_dot(b, c);	\
	\
	if (d > 1 - err) {	\
		q##abbr##_subtract(&q, b, c);	\
		q##abbr##_scaled(&q, t);	\
		q##abbr##_add(a, c, &q);	\
		q##abbr##_normalized(a);	\
		return;	\
	} 	\
	\
	d = d < 0 ? 0 : d;	\
	d = d > 1 ? 1 : d;	\
	\
	theta = arc_cosine(d);	\
	theta_ =  theta * t;	\
	\
	q##abbr##_scale(&p, b, d);	\
	q##abbr##_subtract(&q, c, &p);	\
	q##abbr##_normalized(&q);	\
	\
	q##abbr##_scale(a, b, cosine(theta_));	\
	q##abbr##_scale(&p, &q, sine(theta_));	\
	q##abbr##_added(a, &p);	\
	q##abbr##_normalized(a);	\
}	\
static inline void q##abbr##_rotate(q##abbr *a, q##abbr *b, q##abbr *c) {	\
	a->w = b->w * c->w - b->x * c->x - b->y * c->y - b->z * c->z;	\
	a->x = b->w * c->x + b->x * c->w + b->y * c->z - b->z * c->y;	\
	a->y = b->w * c->y + b->y * c->w + b->z * c->x - b->x * c->z;	\
	a->z = b->w * c->z + b->z * c->w + b->x * c->y - b->y * c->x;	\
	q##abbr##_normalized(a);	\
}	\
static inline void q##abbr##_rotated(q##abbr *a, q##abbr *b) {	\
	q##abbr c = *a;	\
	q##abbr##_rotate(a, &c, b);	\
}	\
static inline void q##abbr##_scale(q##abbr *a, q##abbr *b, type s) {	\
	a->x = b->x * s;	\
	a->y = b->y * s;	\
	a->z = b->z * s;	\
	a->w = b->w * s;	\
} \
static inline void q##abbr##_scaled(q##abbr *a, type s) {	\
	q##abbr##_scale(a, a, s);	\
}	\
static inline type q##abbr##_dot(q##abbr *a, q##abbr *b) {	\
	return a->x * b->x + a->y * b->y + a->z * b->z + a->w * b->w;	\
}	\
static inline void q##abbr##_to_mat3##abbr(mat3##abbr *m, q##abbr *a) {	\
	const type s = 2;	\
	type xs, ys, zs;	\
	type wx, wy, wz;	\
	type xx, xy, xz;	\
	type yy, yz, zz;	\
	\
	xs = a->x * s;	ys = a->y * s;	zs = a->z * s;	\
	wx = a->w * xs;	wy = a->w * ys;	wz = a->w * zs;	\
	xx = a->x * xs;	xy = a->x * ys;	xz = a->x * zs;	\
	yy = a->y * ys;	yz = a->y * zs;	zz = a->z * zs;	\
	\
	m->x.x = 1 - (yy + zz);	m->y.x = xy - wz;	m->z.x = xz + wy;	\
	m->x.y = xy + wz;	m->y.y = 1 - (xx + zz);	m->z.y = yz - wx;	\
	m->x.z = xz - wy;	m->y.z = yz + wx;	m->z.z = 1 - (xx + yy);	\
}	\
static inline void q##abbr##_to_vec4##abbr(vec4##abbr *v, q##abbr *a) {	\
	v->x = a->x;	\
	v->y = a->y;	\
	v->z = a->z;	\
	v->w = a->w;	\
}	\
static inline void q##abbr##_add(q##abbr *a, q##abbr *b, q##abbr *c) {	\
	a->x = b->x + c->x;	\
	a->y = b->y + c->y;	\
	a->z = b->z + c->z;	\
	a->w = b->w + c->w;	\
}	\
static inline void q##abbr##_added(q##abbr *a, q##abbr *b) {	\
	q##abbr##_add(a, a, b);	\
}	\
static inline void q##abbr##_subtract(q##abbr *a, q##abbr *b, q##abbr *c) {	\
	a->x = b->x - c->x;	\
	a->y = b->y - c->y;	\
	a->z = b->z - c->z;	\
	a->w = b->w - c->w;	\
}	\
static inline void q##abbr##_subtracted(q##abbr *a, q##abbr *b) {	\
	q##abbr##_subtract(a, a, b);	\
}	\
static inline bool q##abbr##_equal(q##abbr *a, q##abbr *b) {	\
	return a->x == b->x &&	\
		a->y == b->y &&	\
		a->z == b->z &&	\
		a->w == b->w;	\
}	\
static inline void q##abbr##_normalize(q##abbr *a, q##abbr *b) {	\
	q##abbr##_scale(a, b, 1 / square_root(q##abbr##_dot(b, b)));	\
}	\
static inline void q##abbr##_normalized(q##abbr *a) {	\
	q##abbr##_normalize(a, a);	\
}	\


DECLARE_Q(f, float)
DECLARE_Q(i, int)
DECLARE_Q(d, double)

PROTOTYPE_Q(f, float)
PROTOTYPE_Q(i, int)
PROTOTYPE_Q(d, double)

DEFINE_Q(f, float, sqrtf, sinf, cosf, acosf)
DEFINE_Q(i, int, sqrtf, sinf, cosf, acosf)
DEFINE_Q(d, double, sqrt, sin, cos, acos)

#endif 
