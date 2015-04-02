#ifndef KIT_POS_H_
#define KIT_POS_H_

#include <stddef.h>
#include <stdint.h>

struct pos {
  size_t global_offset;
};

size_t compute_line(const uint8_t *buf, size_t offset);
size_t compute_column(const uint8_t *buf, size_t offset);

#endif /* KIT_POS_H_ */
