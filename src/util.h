#ifndef KIRA_UTIL_H_
#define KIRA_UTIL_H_

#include <stddef.h>

#ifdef _WIN32
#define PRIz "Iu"
#define NORETURN __declspec(noreturn)
#define ALIGNOF(x) __alignof(x)
#else /* _WIN32 */
#define PRIz "zu"
#define NORETURN __attribute__((__noreturn__))
#define ALIGNOF(x) __alignof__(x)
#endif /* _WIN32 */

#define STATIC_CHECK(x) do { enum { assertion = 1/!!(x) }; } while (0)


NORETURN void report_and_abort(const char *file, int line,
                               const char *msg1, const char *msg2);

#define CHECK(x) do { \
    if (!(x)) { \
      report_and_abort(__FILE__, __LINE__, "CHECK failed: ", #x); \
    } \
  } while (0)

#define CRASH(msg) do { \
    report_and_abort(__FILE__, __LINE__, "CRASH: ", msg); \
  } while (0)

#define UNREACHABLE() CRASH("Unreachable.")

#define DBG(...) do { \
    fprintf(stderr, __VA_ARGS__); \
    fflush(stderr); \
  } while (0)

#define TODO_IMPLEMENT CRASH("Unimplemented.")

/* TODO: Every use of ERR_DBG is a bad error message.  And this is in
   the wrong place. */
#define ERR_DBG(...) DBG(__VA_ARGS__)

void *malloc_mul(size_t a, size_t b);

#endif /* KIRA_UTIL_H_ */
