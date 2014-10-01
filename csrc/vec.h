#ifndef VEC_H
#define VEC_H

#include <stdbool.h>

#define DECLARE_VEC2(abbr, type)	\
typedef struct {	\
	type x, y;	\
} vec2##abbr;	\


#define PROTOTYPE_VEC2(abbr, type)	\
static vec2##abbr vec2##abbr##_cons(type x, type y);	\
static void vec2##abbr##_add(vec2##abbr* a, const vec2##abbr* b, const vec2##abbr* c);	\
static void vec2##abbr##_subtract(vec2##abbr* a, const vec2##abbr* b, const vec2##abbr* c);	\
static void vec2##abbr##_scale(vec2##abbr* a, const vec2##abbr* b, type t);	\
static void vec2##abbr##_normalize(vec2##abbr* a, const vec2##abbr* b);	\
static void vec2##abbr##_added(vec2##abbr* a, const vec2##abbr* b);	\
static void vec2##abbr##_subtracted(vec2##abbr* a, const vec2##abbr* b);	\
static void vec2##abbr##_scaled(vec2##abbr* a, type t);	\
static void vec2##abbr##_normalized(vec2##abbr* a);	\
static type vec2##abbr##_dot(const vec2##abbr* a, const vec2##abbr* b);	\
static type vec2##abbr##_length_squared(const vec2##abbr* a);	\
static type vec2##abbr##_length(const vec2##abbr* a);	\
static bool vec2##abbr##_equal(const vec2##abbr* a, const vec2##abbr* b);	\
static void vec2##abbr##_lerp(vec2##abbr* a, const vec2##abbr* b, const vec2##abbr* c, float t);	\
static type * vec2##abbr##_ptr(vec2##abbr* a);	\
static vec3##abbr vec2##abbr##_to_vec3##abbr(const vec2##abbr* a);	\
static vec4##abbr vec2##abbr##_to_vec4##abbr(const vec2##abbr* a);	\


#define DEFINE_VEC2(abbr, type, square_root)	\
static inline vec2##abbr vec2##abbr##_cons(type x, type y) {	\
	vec2##abbr v;	\
	v.x = x;	\
	v.y = y;	\
	return v;	\
}	\
static inline void vec2##abbr##_add(vec2##abbr* a, const vec2##abbr* b, const vec2##abbr* c) {	\
	a->x = b->x + c->x;	\
	a->y = b->y + c->y;	\
}	\
static inline void vec2##abbr##_subtract(vec2##abbr* a, const vec2##abbr* b, const vec2##abbr* c) {	\
	a->x = b->x - c->x;	\
	a->y = b->y - c->y;	\
}	\
static inline void vec2##abbr##_scale(vec2##abbr* a, const vec2##abbr* b, type s) {	\
	a->x = b->x * s;	\
	a->y = b->y * s;	\
}	\
static inline void vec2##abbr##_normalize(vec2##abbr* a, const vec2##abbr* b) {	\
	type s = square_root(b->x * b->x + b->y * b->y);	\
	a->x = b->x / s;	\
	a->y = b->y / s;	\
}	\
static inline void vec2##abbr##_added(vec2##abbr* a, const vec2##abbr* b) {	\
	vec2##abbr##_add(a, a, b);	\
}	\
static inline void vec2##abbr##_subtracted(vec2##abbr* a, const vec2##abbr* b) {	\
	vec2##abbr##_subtract(a, a, b);	\
}	\
static inline void vec2##abbr##_scaled(vec2##abbr* a, type s) {	\
	vec2##abbr##_scale(a, a, s);	\
}	\
static inline void vec2##abbr##_normalized(vec2##abbr* a) {	\
	vec2##abbr##_normalize(a, a);	\
}	\
static inline type vec2##abbr##_dot(const vec2##abbr* a, const vec2##abbr* b) {	\
	return a->x * b->x + a->y * b->y;	\
}	\
static inline type vec2##abbr##_length_squared(const vec2##abbr* a) {	\
	return a->x * a->x + a->y * a-> y;	\
}	\
static inline type vec2##abbr##_length(const vec2##abbr* a) {	\
	return square_root(vec2##abbr##_length_squared(a));	\
}	\
static inline bool vec2##abbr##_equal(const vec2##abbr* a, const vec2##abbr* b) {	\
	return  \
  	a->x == b->x &&	\
		a->y == b->y;	\
}	\
static inline void vec2##abbr##_lerp(vec2##abbr* a, const vec2##abbr* b, const vec2##abbr* c, float t) {	\
	a->x = b->x * (1 - t) + c->x * t;	\
	a->y = b->y * (1 - t) + c->y * t;	\
}	\
static inline type * vec2##abbr##_ptr(vec2##abbr* a) {	\
	return	&a->x;	\
}	\
static inline vec3##abbr vec2##abbr##_to_vec3##abbr(const vec2##abbr* a) {	\
	vec3##abbr b;	\
	b.x = a->x;	\
	b.y = a->y;	\
	b.z = 0;	\
	return b;	\
}	\
static inline vec4##abbr vec2##abbr##_to_vec4##abbr(const vec2##abbr* a) {	\
	vec4##abbr b;	\
	b.x = a->x;	\
	b.y = a->y;	\
	b.z = 0;	\
	b.w = 0;	\
	return b;	\
}	\


#define DECLARE_VEC3(abbr, type)	\
typedef struct {	\
	type x, y, z;	\
} vec3##abbr;	\


#define PROTOTYPE_VEC3(abbr, type)	\
static vec3##abbr vec3##abbr##_cons(type x, type y, type z);	\
static void vec3##abbr##_add(vec3##abbr* a, const vec3##abbr* b, const vec3##abbr* c);	\
static void vec3##abbr##_subtract(vec3##abbr* a, const vec3##abbr* b, const vec3##abbr* c);	\
static void vec3##abbr##_scale(vec3##abbr* a, const vec3##abbr* b, type t);	\
static void vec3##abbr##_cross(vec3##abbr* a, const vec3##abbr* b, const vec3##abbr* c);	\
static void vec3##abbr##_normalize(vec3##abbr* a, const vec3##abbr* b);	\
static void vec3##abbr##_added(vec3##abbr* a, const vec3##abbr* b);	\
static void vec3##abbr##_subtracted(vec3##abbr* a, const vec3##abbr* b);	\
static void vec3##abbr##_scaled(vec3##abbr* a, type t);	\
static void vec3##abbr##_crossed(vec3##abbr* a, const vec3##abbr* b);	\
static void vec3##abbr##_normalized(vec3##abbr* a);	\
static type vec3##abbr##_dot(vec3##abbr* a, const vec3##abbr* b);	\
static bool vec3##abbr##_equal(const vec3##abbr* a, const vec3##abbr* b);	\
static void vec3##abbr##_lerp(vec3##abbr* a, const vec3##abbr* b, const vec3##abbr* c, float t);	\
static type * vec3##abbr##_ptr(vec3##abbr* a);	\
static vec2##abbr vec3##abbr##_to_vec2##abbr(const vec3##abbr* a);	\
static vec4##abbr vec3##abbr##_to_vec4##abbr(const vec3##abbr* a);	\


#define DEFINE_VEC3(abbr, type, square_root)	\
static inline vec3##abbr vec3##abbr##_cons(type x, type y, type z) {	\
	vec3##abbr v;	\
	v.x = x;	\
	v.y = y;	\
	v.z = z;	\
	return v;	\
}	\
static inline void vec3##abbr##_add(vec3##abbr* a, const vec3##abbr* b, const vec3##abbr* c) {	\
	a->x = b->x + c->x;	\
	a->y = b->y + c->y;	\
	a->z = b->z + c->z;	\
}	\
static inline void vec3##abbr##_subtract(vec3##abbr* a, const vec3##abbr* b, const vec3##abbr* c) {	\
	a->x = b->x - c->x;	\
	a->y = b->y - c->y;	\
	a->z = b->z - c->z;	\
}	\
static inline void vec3##abbr##_scale(vec3##abbr* a, const vec3##abbr* b, type s) {	\
	a->x = b->x * s;	\
	a->y = b->y * s;	\
	a->z = b->z * s;	\
}	\
static inline void vec3##abbr##_cross(vec3##abbr* a, const vec3##abbr* b, const vec3##abbr* c) {	\
	a->x = b->y * c->z - b->z * c->y;	\
	a->y = b->z * c->x - b->x * c->z;	\
	a->z = b->x * c->y - b->y * c->x;	\
}	\
static inline void vec3##abbr##_normalize(vec3##abbr* a, const vec3##abbr* b) {	\
	type s = square_root(a->x * a->x + a->y * a->y + a->z * a->z);	\
	a->x = b->x / s;	\
	a->y = b->y / s;	\
	a->z = b->z / s;	\
}	\
static inline void vec3##abbr##_added(vec3##abbr* a, const vec3##abbr* b) {	\
	vec3##abbr##_add(a, a, b);	\
}	\
static inline void vec3##abbr##_subtracted(vec3##abbr* a, const vec3##abbr* b) {	\
	vec3##abbr##_subtract(a, a, b);	\
}	\
static inline void vec3##abbr##_scaled(vec3##abbr* a, type s) {	\
	vec3##abbr##_scale(a, a, s);	\
}	\
static inline void vec3##abbr##_crossed(vec3##abbr* a, const vec3##abbr* b) {	\
	vec3##abbr##_cross(a, a, b);	\
}	\
static inline void vec3##abbr##_normalized(vec3##abbr* a) {	\
	vec3##abbr##_normalize(a, a);	\
}	\
static inline type vec3##abbr##_dot(vec3##abbr* a, const vec3##abbr* b) {	\
	return a->x * b->x + a->y * b->y + a->z * b->z;	\
}	\
static inline bool vec3##abbr##_equal(const vec3##abbr* a, const vec3##abbr* b) {	\
	return	\
    a->x == b->x &&	\
		a->y == b->y &&	\
		a->z == b->z;	\
}	\
static inline void vec3##abbr##_lerp(vec3##abbr* a, const vec3##abbr* b, const vec3##abbr* c, float t) {	\
	a->x = b->x * (1 - t) + c->x * t;	\
	a->y = b->y * (1 - t) + c->y * t;	\
	a->z = b->z * (1 - t) + c->z * t;	\
}	\
static inline type * vec3##abbr##_ptr(vec3##abbr* a) {	\
	return	&a->x;	\
}	\
static inline vec2##abbr vec3##abbr##_to_vec2##abbr(const vec3##abbr* a) {	\
	vec2##abbr b;	\
	b.x = a->x;	\
	b.y = a->y;	\
	return b;	\
}	\
static inline vec4##abbr vec3##abbr##_to_vec4##abbr(const vec3##abbr* a) {	\
	vec4##abbr b;	\
	b.x = a->x;	\
	b.y = a->y;	\
	b.z = a->z;	\
	b.w = 0;	\
	return b;	\
}	\


#define DECLARE_VEC4(abbr, type)	\
typedef struct {	\
	type x, y, z, w;	\
} vec4##abbr;	\


#define PROTOTYPE_VEC4(abbr, type)	\
static vec4##abbr vec4##abbr##_cons(type x, type y, type z, type w);	\
static void vec4##abbr##_add(vec4##abbr* a, vec4##abbr* b, vec4##abbr* c);	\
static void vec4##abbr##_subtract(vec4##abbr* a, vec4##abbr* b, vec4##abbr* c);	\
static void vec4##abbr##_scale(vec4##abbr* a, vec4##abbr* b, type t);	\
static void vec4##abbr##_normalize(vec4##abbr* a, vec4##abbr* b);	\
static void vec4##abbr##_normalized(vec4##abbr* a);	\
static void vec4##abbr##_added(vec4##abbr* a, vec4##abbr* b);	\
static void vec4##abbr##_subtracted(vec4##abbr* a, vec4##abbr* b);	\
static void vec4##abbr##_scaled(vec4##abbr* a, type t);	\
static type vec4##abbr##_dot(vec4##abbr* a, vec4##abbr* b);	\
static bool vec4##abbr##_equal(vec4##abbr* a, vec4##abbr* b);	\
static void vec4##abbr##_lerp(vec4##abbr* a, vec4##abbr* b, vec4##abbr* c, float t);	\
static type * vec4##abbr##_ptr(vec4##abbr* a);	\
static vec2##abbr vec4##abbr##_to_vec2##abbr(vec4##abbr* a);	\
static vec3##abbr vec4##abbr##_to_vec3##abbr(vec4##abbr* a);	\


#define DEFINE_VEC4(abbr, type, square_root)	\
static inline vec4##abbr vec4##abbr##_cons(type x, type y, type z, type w) {	\
	vec4##abbr v;	\
	v.x = x;	\
	v.y = y;	\
	v.z = z;	\
	v.w = w;	\
	return v;	\
}	\
static inline void vec4##abbr##_add(vec4##abbr* a, vec4##abbr* b, vec4##abbr* c) {	\
	a->x = b->x + c->x;	\
	a->y = b->y + c->y;	\
	a->z = b->z + c->z;	\
	a->w = b->w + c->w;	\
}	\
static inline void vec4##abbr##_subtract(vec4##abbr* a, vec4##abbr* b, vec4##abbr* c) {	\
	a->x = b->x - c->x;	\
	a->y = b->y - c->y;	\
	a->z = b->z - c->z;	\
	a->w = b->w - c->w;	\
}	\
static inline void vec4##abbr##_scale(vec4##abbr* a, vec4##abbr* b, type s) {	\
	a->x = b->x * s;	\
	a->y = b->y * s;	\
	a->z = b->z * s;	\
	a->w = b->w * s;	\
}	\
static inline void vec4##abbr##_normalize(vec4##abbr* a, vec4##abbr* b) {	\
	type s = square_root(a->x * a->x + a->y * a->y + a->z * a->z + a->w * a->w);	\
	a->x = b->x / s;	\
	a->y = b->y / s;	\
	a->z = b->z / s;	\
	a->w = b->w / s;	\
}	\
static inline void vec4##abbr##_normalized(vec4##abbr* a) {	\
	vec4##abbr##_normalize(a, a);	\
}	\
static inline void vec4##abbr##_added(vec4##abbr* a, vec4##abbr* b) {	\
	vec4##abbr##_add(a, a, b);	\
}	\
static inline void vec4##abbr##_subtracted(vec4##abbr* a, vec4##abbr* b) {	\
	vec4##abbr##_subtract(a, a, b);	\
}	\
static inline void vec4##abbr##_scaled(vec4##abbr* a, type s) {	\
	vec4##abbr##_scale(a, a, s);	\
}	\
static inline type vec4##abbr##_dot(vec4##abbr* a, vec4##abbr* b) {	\
	return a->x * b->x + a->y * b->y + a->z * b->z + a->w * b->w;	\
}	\
static inline bool vec4##abbr##_equal(vec4##abbr* a, vec4##abbr* b) {	\
	return	a->x == b->x &&	\
		a->y == b->y &&	\
		a->z == b->z &&	\
		a->w == b->w;	\
}	\
static inline void vec4##abbr##_lerp(vec4##abbr* a, vec4##abbr* b, vec4##abbr* c, float t) {	\
	a->x = b->x * (1 - t) + c->x * t;	\
	a->y = b->y * (1 - t) + c->y * t;	\
	a->z = b->z * (1 - t) + c->z * t;	\
	a->w = b->w * (1 - t) + c->w * t;	\
}	\
static inline type * vec4##abbr##_ptr(vec4##abbr* a) {	\
	return	&a->x;	\
}	\
static inline vec2##abbr vec4##abbr##_to_vec2##abbr(vec4##abbr* a) {	\
	vec2##abbr b;	\
	b.x = a->x;	\
	b.y = a->y;	\
	return b;	\
}	\
static inline vec3##abbr vec4##abbr##_to_vec3##abbr(vec4##abbr* a) {	\
	vec3##abbr b;	\
	b.x = a->x;	\
	b.y = a->y;	\
	b.z = a->z;	\
	return b;	\
}	\


#define DECLARE_VECX(abbr, type)	\
DECLARE_VEC2(abbr, type)	\
DECLARE_VEC3(abbr, type)	\
DECLARE_VEC4(abbr, type)	\


#define PROTOTYPE_VECX(abbr, type)	\
PROTOTYPE_VEC2(abbr, type)	\
PROTOTYPE_VEC3(abbr, type)	\
PROTOTYPE_VEC4(abbr, type)	\


#define DEFINE_VECX(abbr, type, square_root)	\
DEFINE_VEC2(abbr, type, square_root)	\
DEFINE_VEC3(abbr, type, square_root)	\
DEFINE_VEC4(abbr, type, square_root)	\


DECLARE_VECX(f, float)
DECLARE_VECX(i, int)
DECLARE_VECX(d, double)

PROTOTYPE_VECX(f, float)
PROTOTYPE_VECX(i, int)
PROTOTYPE_VECX(d, double)


#endif
