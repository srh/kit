#ifndef KIRA_POS_H_
#define KIRA_POS_H_

#include <stddef.h>
#include <stdint.h>

#include "identmap.h"

struct pos {
  size_t global_offset;
  size_t line;
  size_t column;
  ident_value filename;
};

struct pos make_pos(size_t global_offset,
                    size_t line,
                    size_t column,
                    ident_value filename);

#endif /* KIRA_POS_H_ */
