#ifndef KIT_PLATFORM_H_
#define KIT_PLATFORM_H_

enum target_platform {
  TARGET_PLATFORM_WIN_32BIT,
  TARGET_PLATFORM_LINUX_32BIT,
  TARGET_PLATFORM_LINUX_64BIT,
  TARGET_PLATFORM_OSX_32BIT,
  TARGET_PLATFORM_OSX_64BIT,
};

enum target_arch {
  TARGET_ARCH_Y86,
  TARGET_ARCH_X64,
};

enum target_arch platform_arch(enum target_platform platform);

#endif /* KIT_PLATFORM_H_ */

