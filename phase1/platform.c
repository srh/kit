#include "platform.h"
#include "util.h"

enum target_arch platform_arch(enum target_platform platform) {
  switch (platform) {
  case TARGET_PLATFORM_WIN_32BIT:
  case TARGET_PLATFORM_LINUX_32BIT:
  case TARGET_PLATFORM_OSX_32BIT:
    return TARGET_ARCH_X86;
  case TARGET_PLATFORM_LINUX_64BIT:
  case TARGET_PLATFORM_OSX_64BIT:
    return TARGET_ARCH_X64;
  default:
    UNREACHABLE();
  }
}
