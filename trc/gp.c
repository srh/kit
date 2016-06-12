#include "gp.h"

#include "objfile_objfile.h"

void apptext(struct objfile *f, const void *buf, size_t count) {
  objfile_section_append_raw(objfile_text(f), buf, count);
}

void pushtext(struct objfile *f, uint8_t byte) {
  apptext(f, &byte, 1);
}

enum oz ptr_oz(struct objfile *f) {
  switch (objfile_arch(f)) {
  case TARGET_ARCH_Y86:
    return OZ_32;
  case TARGET_ARCH_X64:
    return OZ_64;
  default:
    UNREACHABLE();
  }
}
