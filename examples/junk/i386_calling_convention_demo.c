#include <stdio.h>

#ifdef _WIN32
#define PACK_PUSH __pragma(pack(push, 1))
#define PACK_POP __pragma(pack(pop))
#define PACK_ATTRIBUTE
#else /* _WIN32 */
#define PACK_PUSH
#define PACK_POP
#define PACK_ATTRIBUTE __attribute__((__packed__))
#endif /* _WIN32 */


#define WRAP(n) stc_val_ ## n
#define STATIC_CHECK(x) do { enum { assertion = 1/!!(x) }; } while (0)

struct b1 { char x[1]; };
struct b2 { char x[2]; };
struct b3 { char x[3]; };
struct b4 { char x[4]; };
struct b5 { char x[5]; };
struct b6 { char x[6]; };
struct b7 { char x[7]; };
struct b8 { char x[8]; };
struct b9 { char x[9]; };
struct b10 { char x[10]; };

struct s1 { short x[1]; };
struct s2 { short x[2]; };
struct s3 { short x[3]; };
struct s4 { short x[4]; };
struct s5 { short x[5]; };

struct i1 { int x[1]; };
struct i2 { int x[2]; };
struct i3 { int x[3]; };

struct b_s { char x; short y; };
struct bbbbs { char x; char y; char t; char u; short z; };

PACK_PUSH
struct bs { char x; short y; } PACK_ATTRIBUTE;
struct sb { short x; char y; } PACK_ATTRIBUTE;
struct bsb { char x; short y; char z; } PACK_ATTRIBUTE;
struct b3sb3 { char x[3]; short y; char z[3]; } PACK_ATTRIBUTE;
struct ssss { short x; short y; short z; short t; } PACK_ATTRIBUTE;
struct b8again { char a; char b; char c; char d; char e; char f; char g; char h; } PACK_ATTRIBUTE;
struct bsssb { char x; short y; short z; short t; char u; } PACK_ATTRIBUTE;
struct bsbbsb { char x; short y; char z; char t; short u; char v; } PACK_ATTRIBUTE;
struct bbbbss { char x; char y; char t; char u; short z; short w; } PACK_ATTRIBUTE;
struct b4ss { char x[4]; short y; short w; } PACK_ATTRIBUTE;
struct bbbsbbb { char x1; char x2; char x3; short y; char z1; char z2; char z3; } PACK_ATTRIBUTE;
struct b3sbbb { char x[3]; short y; char z1; char z2; char z3; } PACK_ATTRIBUTE;
struct bbbsb3 { char x1; char x2; char x3; short y; char z[3]; } PACK_ATTRIBUTE;
struct bbbbbb3 { char x1; char x2; char x3; char x4; char y; char z[3]; } PACK_ATTRIBUTE;
struct bbbbbbb2 { char x1; char x2; char x3; char x4; char y; char w; char z[2]; } PACK_ATTRIBUTE;
PACK_POP
struct d { double x; };
struct f { float x; };
struct f2 { float x; float y; };

struct b3b { char x[3]; char y; };
struct bwrap { char x; char y; char z; char t; char u; struct b3 v; };


/* These comments are about WINDOWS behavior. */

/* The "hidden return pointer" functions take a pointer to struct in
   [ebp+8] and when they return put that pointer's value in eax. */

/* Returns in eax or just al. */
struct b1 func1(void) { struct b1 ret; ret.x[0] = 7; return ret; }
/* Returns in eax or just ax. */
struct b2 func2(void) { struct b2 ret; ret.x[0] = 7; return ret; }
/* Hidden return pointer. */
struct b3 func3(void) { struct b3 ret; ret.x[0] = 7; return ret; }
/* Returns in eax. */
struct b4 func4(void) { struct b4 ret; ret.x[0] = 7; return ret; }
/* Hidden return pointer. */
struct b5 func5(void) { struct b5 ret; ret.x[0] = 7; return ret; }
/* Hidden return pointer. */
struct b6 func6(void) { struct b6 ret; ret.x[0] = 7; return ret; }
/* Hidden return pointer. */
struct b7 func7(void) { struct b7 ret; ret.x[0] = 7; return ret; }
/* Returns in eax:edx. */
struct b8 func8(void) { struct b8 ret; ret.x[0] = 7; return ret; }
/* Hidden return pointer. */
struct b9 func9(void) { struct b9 ret; ret.x[0] = 7; return ret; }
/* Hidden return pointer. */
struct b10 func10(void) { struct b10 ret; ret.x[0] = 7; return ret; }

/* Returns in eax or just ax. */
struct s1 sfunc1(void) { struct s1 ret; ret.x[0] = 7; return ret; }
/* Returns in eax. */
struct s2 sfunc2(void) { struct s2 ret; ret.x[0] = 7; return ret; }
/* Hidden return pointer. */
struct s3 sfunc3(void) { struct s3 ret; ret.x[0] = 7; return ret; }
/* Returns in eax:edx */
struct s4 sfunc4(void) { struct s4 ret; ret.x[0] = 7; return ret; }
/* Hidden return pointer. */
struct s5 sfunc5(void) { struct s5 ret; ret.x[0] = 7; return ret; }

/* Returns in eax. */
struct i1 ifunc1(void) { struct i1 ret; ret.x[0] = 7; return ret; }
/* Returns in eax:edx. */
struct i2 ifunc2(void) { struct i2 ret; ret.x[0] = 7; return ret; }
/* Hidden return pointer. */
struct i3 ifunc3(void) { struct i3 ret; ret.x[0] = 7; return ret; }

/* Returns in eax. */
struct b_s b_sfunc(void) {
  STATIC_CHECK(sizeof(struct b_s) == 4);
  struct b_s ret; ret.y = 7; return ret;
}
/* Returns in eax. */
struct bbbbs bbbbsfunc(void) {
  STATIC_CHECK(sizeof(struct bbbbs) == 6);
  struct bbbbs ret; ret.z = 7; return ret;
}

/* Hidden return pointer. */
struct bs bsfunc(void) {
  STATIC_CHECK(sizeof(struct bs) == 3);
  struct bs ret; ret.y = 7; return ret;
}

/* Hidden return pointer. */
struct sb sbfunc(void) {
  STATIC_CHECK(sizeof(struct sb) == 3);
  struct sb ret; ret.y = 7; return ret;
}

/* Returns in eax. */
struct bsb bsbfunc(void) {
  STATIC_CHECK(sizeof(struct bsb) == 4);
  struct bsb ret; ret.y = 7; return ret;
}

/* Returns in eax:edx. */
struct b3sb3 b3sb3func(void) {
  STATIC_CHECK(sizeof(struct b3sb3) == 8);
  struct b3sb3 ret; ret.y = 7; return ret;
}

struct ssss ssssfunc(void) {
  STATIC_CHECK(sizeof(struct ssss) == 8);
  struct ssss ret; ret.y = 7; return ret;
}

struct b8again b8againfunc(void) {
  STATIC_CHECK(sizeof(struct b8again) == 8);
  struct b8again ret; ret.b = 7; return ret;
}

struct bsssb bsssbfunc(void) {
  STATIC_CHECK(sizeof(struct bsssb) == 8);
  struct bsssb ret; ret.y = 7; return ret;
}

struct bsbbsb bsbbsbfunc(void) {
  STATIC_CHECK(sizeof(struct bsbbsb) == 8);
  struct bsbbsb ret; ret.y = 7; return ret;
}

struct bbbbss bbbbssfunc(void) {
  STATIC_CHECK(sizeof(struct bbbbss) == 8);
  struct bbbbss ret; ret.y = 7; return ret;
}

struct b4ss b4ssfunc(void) {
  STATIC_CHECK(sizeof(struct b4ss) == 8);
  struct b4ss ret; ret.y = 7; return ret;
}

struct bbbsbbb bbbsbbbfunc(void) {
  STATIC_CHECK(sizeof(struct bbbsbbb) == 8);
  struct bbbsbbb ret; ret.y = 7; return ret;
}

struct bbbsb3 bbbsb3func(void) {
  STATIC_CHECK(sizeof(struct bbbsb3) == 8);
  struct bbbsb3 ret; ret.y = 7; return ret;
}

struct b3sbbb b3sbbbfunc(void) {
  STATIC_CHECK(sizeof(struct b3sbbb) == 8);
  struct b3sbbb ret; ret.y = 7; return ret;
}

struct bbbbbb3 bbbbbb3func(void) {
  STATIC_CHECK(sizeof(struct bbbbbb3) == 8);
  struct bbbbbb3 ret; ret.y = 7; return ret;
}

struct bbbbbbb2 bbbbbbb2func(void) {
  STATIC_CHECK(sizeof(struct bbbbbbb2) == 8);
  struct bbbbbbb2 ret; ret.y = 7; return ret;
}

struct b3b b3bfunc(void) {
  STATIC_CHECK(sizeof(struct b3b) == 4);
  struct b3b ret; ret.y = 7; return ret;
}

struct bwrap bwrapfunc(void) {
  STATIC_CHECK(sizeof(struct bwrap) == 8);
  struct bwrap ret; ret.y = 7; return ret;
}

struct d dfunc(void) {
  STATIC_CHECK(sizeof(struct d) == 8);
  struct d ret; ret.x = 3.0; return ret;
}

struct f ffunc(void) {
  STATIC_CHECK(sizeof(struct f) == 4);
  struct f ret; ret.x = 3.0; return ret;
}

struct f2 f2func(void) {
  STATIC_CHECK(sizeof(struct f2) == 8);
  struct f2 ret; ret.x = 3.0; return ret;
}

/* Returns with the value flded. */
float floatfunc(void) {
  return 3.0;
}

double blahfunc(void) {
  return 4.0;
}

float call_floatfunc(void) {
  blahfunc();
  return floatfunc();
}

struct b10 b10_declared(void);

double blah = 1.0/3;

/* Allocates a spot on its own stack frame, and then copies to its
   return pointer, under any optimization setting I've tried.  I don't
   know if there's a reason why it can't just pass its return pointer
   upward.  LINUX: It passes the hidden param to the callee, but
   doesn't assume the callee returns the right return value. */
struct b10 b10_call_declared(void) {
  printf("%f", blah);
  return b10_declared();
}
