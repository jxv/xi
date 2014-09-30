#ifndef MAT_H
#define MAT_H

#include "vec.h"

#define DECLARE_MAT2(abbr, type)	\
typedef struct	{	\
	vec2##abbr x, y;	\
} mat2##abbr;	\

#define PROTOTYPE_MAT2(abbr, type)	\
static void mat2##abbr##_identity(mat2##abbr *a);	\
static type *mat2##abbr##_ptr(mat2##abbr *a);	\


#define DEFINE_MAT2(abbr, type, sine, cosine, tangent)	\
static inline void mat2##abbr##_identity(mat2##abbr *a) {	\
	memset(mat2##abbr##_ptr(a), 0, sizeof(mat2##abbr));	\
	a->x.x = 1;	\
	a->y.y = 1;	\
}	\
static inline type * mat2##abbr##_ptr(mat2##abbr *a) {	\
	return &a->x.x;	\
}


#define DECLARE_MAT3(abbr, type)	\
typedef struct {	\
	vec3##abbr x, y, z;	\
} mat3##abbr;	\


#define PROTOTYPE_MAT3(abbr, type)	\
static void mat3##abbr##_transpose(mat3##abbr *a, mat3##abbr *b);	\
static void mat3##abbr##_identity(mat3##abbr *a);	\
static void mat3##abbr##_transposed(mat3##abbr *a);	\
static void mat3##abbr##_to_mat4##abbr(mat4##abbr *a, mat3##abbr *b);	\
static type * mat3##abbr##_ptr(mat3##abbr *a);	\


#define DEFINE_MAT3(abbr, type, sine, cosine, tangent)	\
static inline void mat3##abbr##_transpose(mat3##abbr *a, mat3##abbr *b) {	\
	a->x.x = b->x.x;	a->x.y = b->y.x;	a->x.z = b->z.x;	\
	a->y.x = b->x.y;	a->y.y = b->y.y;	a->y.z = b->z.y;	\
	a->z.x = b->x.z;	a->z.y = b->y.z;	a->z.z = b->z.z;	\
}	\
static inline void mat3##abbr##_identity(mat3##abbr *a) {	\
	memset(mat3##abbr##_ptr(a), 0, sizeof(mat3##abbr));	\
	a->x.x = 1;	\
	a->y.y = 1;	\
	a->z.z = 1;	\
}	\
static inline void mat3##abbr##_transposed(mat3##abbr *a) {	\
	mat3##abbr b = *a;	\
	mat3##abbr##_transpose(a, &b);	\
}	\
static inline void mat3##abbr##_to_mat4##abbr(mat4##abbr *a, mat3##abbr *b) {	\
	a->x = vec3##abbr##_to_vec4##abbr(&b->x);	\
	a->y = vec3##abbr##_to_vec4##abbr(&b->y);	\
	a->z = vec3##abbr##_to_vec4##abbr(&b->z);	\
	a->w = vec4##abbr##_cons(0, 0, 0, 1);	\
}	\
static inline type * mat3##abbr##_ptr(mat3##abbr *a) {	\
	return &a->x.x;	\
}	\


#define DECLARE_MAT4(abbr, type)	\
typedef struct {	\
	vec4##abbr x, y, z, w;	\
} mat4##abbr;	\


#define PROTOTYPE_MAT4(abbr, type)	\
static void mat4##abbr##_identity(mat4##abbr *a);	\
static void mat4##abbr##_multiply(mat4##abbr *a, mat4##abbr *b, mat4##abbr *c);	\
static void mat4##abbr##_transpose(mat4##abbr *a, mat4##abbr *b);	\
static void mat4##abbr##_translate(mat4##abbr *a, vec3##abbr *v);	\
static void mat4##abbr##_translate_xyz(mat4##abbr *a, type x, type y, type z);	\
static void mat4##abbr##_rotate(mat4##abbr *a, type radians, vec3##abbr *v);	\
static void mat4##abbr##_rotate_xyz(mat4##abbr *a, type radians, type x, type y, type z);	\
static void mat4##abbr##_frustum(mat4##abbr *m, type left, type right, type bottom, type top, type near, type far);	\
static void mat4##abbr##_perspective(mat4##abbr *m, type field_of_view_y, type aspect, type near, type far);	\
static void mat4##abbr##_multiply_vec4##abbr(vec4##abbr *a, mat4##abbr *m, vec4##abbr *b);	\
static void vec4##abbr##_multiply_mat4##abbr(vec4##abbr *a, vec4##abbr *b, mat4##abbr *m);	\
static void mat4##abbr##_multiplied(mat4##abbr *a, mat4##abbr *b);	\
static void mat4##abbr##_transposed(mat4##abbr *a);	\
static void mat4##abbr##_to_mat3##abbr(mat3##abbr *a, mat4##abbr *b);	\
static type * mat4##abbr##_ptr(mat4##abbr *a);	\


#define DEFINE_MAT4(abbr, type, sine, cosine, tangent)	\
static inline void mat4##abbr##_identity(mat4##abbr *a) {	\
	memset(mat4##abbr##_ptr(a), 0, sizeof(mat4##abbr));	\
	a->x.x = 1;	\
	a->y.y = 1;	\
	a->z.z = 1;	\
	a->w.w = 1;	\
}	\
static inline void mat4##abbr##_multiply(mat4##abbr *a, mat4##abbr *b, mat4##abbr *c) {	\
	a->x.x = b->x.x * c->x.x + b->x.y * c->y.x + b->x.z * c->z.x + b->x.w * c->w.x;	\
	a->x.y = b->x.x * c->x.y + b->x.y * c->y.y + b->x.z * c->z.y + b->x.w * c->w.y;	\
	a->x.z = b->x.x * c->x.z + b->x.y * c->y.z + b->x.z * c->z.z + b->x.w * c->w.z;	\
	a->x.w = b->x.x * c->x.w + b->x.y * c->y.w + b->x.z * c->z.w + b->x.w * c->w.w;	\
	\
	a->y.x = b->y.x * c->x.x + b->y.y * c->y.x + b->y.z * c->z.x + b->y.w * c->w.x;	\
	a->y.y = b->y.x * c->x.y + b->y.y * c->y.y + b->y.z * c->z.y + b->y.w * c->w.y;	\
	a->y.z = b->y.x * c->x.z + b->y.y * c->y.z + b->y.z * c->z.z + b->y.w * c->w.z;	\
	a->y.w = b->y.x * c->x.w + b->y.y * c->y.w + b->y.z * c->z.w + b->y.w * c->w.w;	\
	\
	a->z.x = b->z.x * c->x.x + b->z.y * c->y.x + b->z.z * c->z.x + b->z.w * c->w.x;	\
	a->z.y = b->z.x * c->x.y + b->z.y * c->y.y + b->z.z * c->z.y + b->z.w * c->w.y;	\
	a->z.z = b->z.x * c->x.z + b->z.y * c->y.z + b->z.z * c->z.z + b->z.w * c->w.z;	\
	a->z.w = b->z.x * c->x.w + b->z.y * c->y.w + b->z.z * c->z.w + b->z.w * c->w.w;	\
	\
	a->w.x = b->w.x * c->x.x + b->w.y * c->y.x + b->w.z * c->z.x + b->w.w * c->w.x;	\
	a->w.y = b->w.x * c->x.y + b->w.y * c->y.y + b->w.z * c->z.y + b->w.w * c->w.y;	\
	a->w.z = b->w.x * c->x.z + b->w.y * c->y.z + b->w.z * c->z.z + b->w.w * c->w.z;	\
	a->w.w = b->w.x * c->x.w + b->w.y * c->y.w + b->w.z * c->z.w + b->w.w * c->w.w;	\
}	\
static inline void mat4##abbr##_transpose(mat4##abbr *a, mat4##abbr *b) {	\
	a->x.x = b->x.x;	a->x.y = b->y.x;	a->x.z = b->z.x;	a->x.w = b->w.x;	\
	a->y.x = b->x.y;	a->y.y = b->y.y;	a->y.z = b->z.y;	a->y.w = b->w.y;	\
	a->z.x = b->x.z;	a->z.y = b->y.z;	a->z.z = b->z.z;	a->z.w = b->w.z;	\
	a->w.x = b->x.w;	a->w.y = b->y.w;	a->w.z = b->z.w;	a->w.w = b->w.w;	\
}	\
static inline void mat4##abbr##_translate(mat4##abbr *a, vec3##abbr *v) {	\
	mat4##abbr##_identity(a);	\
	a->w = vec4##abbr##_cons(v->x, v->y, v->z, 1);	\
}	\
static inline void mat4##abbr##_translate_xyz(mat4##abbr *a, type x, type y, type z) {	\
	mat4##abbr##_identity(a);	\
	a->w = vec4##abbr##_cons(x, y, z, 1);	\
}	\
static inline void mat4##abbr##_scale(mat4##abbr *a, vec3##abbr *v) {	\
	mat4##abbr##_identity(a);	\
	a->x.x = v->x;	\
	a->y.y = v->y;	\
	a->z.z = v->z;	\
}	\
static inline void mat4##abbr##_rotate(mat4##abbr *a, type radians, vec3##abbr *v) {	\
	mat4##abbr##_rotate_xyz(a, radians, v->x, v->y, v->z);	\
}	\
static inline void mat4##abbr##_rotate_xyz(mat4##abbr *a, type radians, type x, type y, type z) {	\
	type s, c;	\
	s = sine(radians);	\
	c = cosine(radians);	\
	mat4##abbr##_identity(a);	\
	\
	a->x.x = c + (1 - c) * x * x;	\
	a->x.y = (1 - c) * x * y - z * s;	\
	a->x.z = (1 - c) * x * z + y * s;	\
	a->y.x = (1 - c) * x * y + z * s;	\
	a->y.y = c + (1 - c) * y * y;	\
	a->y.z = (1 - c) * y * z - x * s;	\
	a->z.x = (1 - c) * x * z - y * s;	\
	a->z.y = (1 - c) * y * z + x * s;	\
	a->z.z = c + (1 - c) * z * z;	\
}	\
static inline void mat4##abbr##_frustum(mat4##abbr *m, type left, type right, type bottom, type top, type near, type far) {	\
	type a, b, c, d, e, f;	\
	a = 2 * near / (right - left);	\
	b = 2 * near / (top - bottom);	\
	c = (right + left) / (right - left);	\
	d = (top + bottom) / (top - bottom);	\
	e = -(far + near) / (far - near);	\
	f = -2 * far * near / (far - near);	\
	m->x = vec4##abbr##_cons(a, 0, 0, 0);	\
	m->y = vec4##abbr##_cons(0, b, 0, 0);	\
	m->z = vec4##abbr##_cons(c, d, e, -1);	\
	m->w = vec4##abbr##_cons(0, 0, f, 1);	\
}	\
static inline void mat4##abbr##_perspective(mat4##abbr *m, type field_of_view_y, type aspect, type near, type far) {	\
	type h = tangent(field_of_view_y) * near;	\
	type w = h * aspect;	\
	mat4##abbr##_frustum(m, -w, w, -h, h, near, far);	\
}	\
static inline void mat4##abbr##_look_at(mat4##abbr *m, vec3##abbr *eye, vec3##abbr *target, vec3##abbr *up) {	\
	vec3##abbr x, y, z;	\
	vec4##abbr neye, eye_;	\
	\
	vec3##abbr##_subtract(&z, eye, target);	\
	vec3##abbr##_normalized(&z);	\
	\
	vec3##abbr##_cross(&x, up, &z);	\
	vec3##abbr##_normalized(&x);	\
	\
	vec3##abbr##_cross(&y, &z, &x);	\
	vec3##abbr##_normalized(&y);	\
	\
	m->x = vec3##abbr##_to_vec4##abbr(&x);	\
	m->y = vec3##abbr##_to_vec4##abbr(&y);	\
	m->z = vec3##abbr##_to_vec4##abbr(&z);	\
	m->w = vec4##abbr##_cons(0, 0, 0, 1);	\
	\
	neye = vec4##abbr##_cons(-eye->x, -eye->y, -eye->z, 1);	\
	\
	mat4##abbr##_multiply_vec4##abbr(&eye_, m, &neye);	\
	\
	mat4##abbr##_transposed(m);	\
	m->w = eye_;	\
}	\
static inline void mat4##abbr##_multiply_vec4##abbr(vec4##abbr *a, mat4##abbr *m, vec4##abbr *b) {	\
	a->x = b->x * m->x.x + b->y * m->y.x + b->z * m->z.x + b->w * m->w.x;	\
	a->y = b->x * m->x.y + b->y * m->y.y + b->z * m->z.y + b->w * m->w.y;	\
	a->z = b->x * m->x.z + b->y * m->y.z + b->z * m->z.z + b->w * m->w.z;	\
	a->w = b->x * m->x.z + b->y * m->y.w + b->z * m->z.w + b->w * m->w.w;	\
}	\
static inline void vec4##abbr##_multiply_mat4##abbr(vec4##abbr *a, vec4##abbr *b, mat4##abbr *m) {	\
	mat4##abbr##_multiply_vec4##abbr(a, m, b);	\
}	\
static inline void mat4##abbr##_multiplied(mat4##abbr *a, mat4##abbr *b) {	\
	mat4##abbr c = *a;	\
	mat4##abbr##_multiply(a, &c, b);	\
}	\
static inline void mat4##abbr##_transposed(mat4##abbr *a) {	\
	mat4##abbr b = *a;	\
	mat4##abbr##_transpose(a, &b);	\
}	\
static inline void mat4##abbr##_to_mat3##abbr(mat3##abbr *a, mat4##abbr *b) {	\
	a->x = vec4##abbr##_to_vec3##abbr(&b->x);	\
	a->y = vec4##abbr##_to_vec3##abbr(&b->y);	\
	a->z = vec4##abbr##_to_vec3##abbr(&b->z);	\
}	\
static inline type * mat4##abbr##_ptr(mat4##abbr *a) {	\
	return &a->x.x;	\
}	\


#define DECLARE_MATX(abbr, type)	\
DECLARE_MAT2(abbr, type)	\
DECLARE_MAT3(abbr, type)	\
DECLARE_MAT4(abbr, type)	\


#define PROTOTYPE_MATX(abbr, type)	\
PROTOTYPE_MAT2(abbr, type)	\
PROTOTYPE_MAT3(abbr, type)	\
PROTOTYPE_MAT4(abbr, type)	\


#define DEFINE_MATX(abbr, type, sine, cosine, tangent)	\
DEFINE_MAT2(abbr, type, sine, cosine, tangent)	\
DEFINE_MAT3(abbr, type, sine, cosine, tangent)	\
DEFINE_MAT4(abbr, type, sine, cosine, tangent)	\


DECLARE_MATX(f, float)
DECLARE_MATX(i, int)
DECLARE_MATX(d, double)

PROTOTYPE_MATX(f, float)
PROTOTYPE_MATX(i, int)
PROTOTYPE_MATX(d, double)

DEFINE_MATX(f, float, sinf, cosf, tanf)
DEFINE_MATX(i, int, sinf, cosf, tanf)
DEFINE_MATX(d, double, sin, cos, tan)

#endif
