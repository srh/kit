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

__pragma(pack(push, 1))
struct bs { char x; short y; };
struct sb { short x; char y; };
struct bsb { char x; short y; char z; };
struct b3sb3 { char x[3]; short y; char z[3]; };
__pragma(pack(pop))

/* The "hidden return pointer" functions take a pointer to struct in
   [ebp+8] and when they return put that pointer's value in eax. */

/* Returns in al. */
struct b1 __stdcall func1(void) { struct b1 ret; ret.x[0] = 7; return ret; }
/* Returns in ax. */
struct b2 __stdcall func2(void) { struct b2 ret; ret.x[0] = 7; return ret; }
/* Hidden return pointer. */
struct b3 __stdcall func3(void) { struct b3 ret; ret.x[0] = 7; return ret; }
/* Returns in eax. */
struct b4 __stdcall func4(void) { struct b4 ret; ret.x[0] = 7; return ret; }
/* HRP */
struct b5 __stdcall func5(void) { struct b5 ret; ret.x[0] = 7; return ret; }
/* HRP */
struct b6 __stdcall func6(void) { struct b6 ret; ret.x[0] = 7; return ret; }
/* HRP */
struct b7 __stdcall func7(void) { struct b7 ret; ret.x[0] = 7; return ret; }
/* Returns in eax:edx. */
struct b8 __stdcall func8(void) { struct b8 ret; ret.x[0] = 7; return ret; }
/* HRP */
struct b9 __stdcall func9(void) { struct b9 ret; ret.x[0] = 7; return ret; }
/* HRP */
struct b10 __stdcall func10(void) { struct b10 ret; ret.x[0] = 7; return ret; }

/* Returns in ax. */
struct s1 __stdcall sfunc1(void) { struct s1 ret; ret.x[0] = 7; return ret; }
/* Returns in eax. */
struct s2 __stdcall sfunc2(void) { struct s2 ret; ret.x[0] = 7; return ret; }
/* HRP */
struct s3 __stdcall sfunc3(void) { struct s3 ret; ret.x[0] = 7; return ret; }
/* Returns in eax:edx. */
struct s4 __stdcall sfunc4(void) { struct s4 ret; ret.x[0] = 7; return ret; }
/* HRP */
struct s5 __stdcall sfunc5(void) { struct s5 ret; ret.x[0] = 7; return ret; }

/* Returns in eax. */
struct i1 __stdcall ifunc1(void) { struct i1 ret; ret.x[0] = 7; return ret; }
/* Returns in eax:edx. */
struct i2 __stdcall ifunc2(void) { struct i2 ret; ret.x[0] = 7; return ret; }
/* HRP */
struct i3 __stdcall ifunc3(void) { struct i3 ret; ret.x[0] = 7; return ret; }

/* Returns in eax. */
struct b_s __stdcall b_sfunc(void) {
  STATIC_CHECK(sizeof(struct b_s) == 4);
  struct b_s ret; ret.y = 7; return ret;
}
/* HRP */
struct bbbbs __stdcall bbbbsfunc(void) {
  STATIC_CHECK(sizeof(struct bbbbs) == 6);
  struct bbbbs ret; ret.z = 7; return ret;
}
/* HRP */
struct bs __stdcall bsfunc(void) {
  STATIC_CHECK(sizeof(struct bs) == 3);
  struct bs ret; ret.y = 7; return ret;
}
/* HRP */
struct sb __stdcall sbfunc() {
  STATIC_CHECK(sizeof(struct sb) == 3);
  struct sb ret; ret.y = 7; return ret;
}
/* HRP, and the callee cleanup is slightly different. */
struct sb __stdcall sbfuncparam(int x) {
  STATIC_CHECK(sizeof(struct sb) == 3);
  struct sb ret; ret.y = 7 + x; return ret;
}
/* Returns in eax. */
struct bsb __stdcall bsbfunc(void) {
  STATIC_CHECK(sizeof(struct bsb) == 4);
  struct bsb ret; ret.y = 7; return ret;
}
/* Returns in eax:edx. */
struct b3sb3 __stdcall b3sb3func(void) {
  STATIC_CHECK(sizeof(struct b3sb3) == 8);
  struct b3sb3 ret; ret.y = 7; return ret;
}

/* Value flded. */
float __stdcall floatfunc(void) {
  return 3.0;
}

struct b10 __stdcall b10_declared(void);

/* Allocates a spot on its own stack frame, and then copies to its
   return pointer, under any optimization setting I've tried.  I don't
   know if there's a reason why it can't just pass its return pointer
   upward. */
struct b10 __stdcall b10_call_declared(void) {
  return b10_declared();
}
