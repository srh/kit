#ifndef KIRA_UTIL_H_
#define KIRA_UTIL_H_

#ifdef _WIN32
#define PRIz "Iu"
#else /* _WIN32 */
#define PRIz "zu"
#endif /* _WIN32 */

#define STATIC_ASSERT(x) do { enum { assertion = 1/!!(x) }; } while (0)

void report_and_abort(const char *file, int line,
		      const char *msg1, const char *msg2);

#define CHECK(x) do { \
    if (!(x)) { \
      report_and_abort(__FILE__, __LINE__, "CHECK failed: ", #x); \
    } \
  } while (0)

#define CRASH(msg) do { \
    report_and_abort(__FILE__, __LINE__, "CRASH: ", #msg); \
  } while (0)

#define UNREACHABLE() CRASH("Unreachable.")

#define DBG(...) do { \
    fprintf(stderr, __VA_ARGS__); \
    fflush(stderr); \
  } while (0)

#endif /* KIRA_UTIL_H_ */
