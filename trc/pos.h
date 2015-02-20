#ifndef KIRA_POS_H_
#define KIRA_POS_H_

#include <stddef.h>

struct pos {
  size_t offset;
  size_t line;
  size_t column;
};

struct pos make_pos(size_t offset, size_t line, size_t column);

#endif /* KIRA_POS_H_ */
