#ifndef KIRA_POS_H_
#define KIRA_POS_H_

#include <stddef.h>
#include <stdint.h>

#include "identmap.h"

struct pos {
  size_t global_offset;
  size_t line;
  size_t column;
};

struct pos make_pos(size_t global_offset,
                    size_t line,
                    size_t column);

#endif /* KIRA_POS_H_ */
