#ifndef KIRA_PARSE_H_
#define KIRA_PARSE_H_

#include <stddef.h>
#include <stdint.h>

#include "databuf.h"

struct ast_file;
struct identmap;

struct pos {
  size_t offset;
  size_t line;
  size_t column;
};

struct pos make_pos(size_t offset, size_t line, size_t column);

struct error_info {
  struct pos pos;
  struct databuf message;
};

void error_info_destroy(struct error_info *ei);

int parse_test(void);
int parse_buf_file(struct identmap *im,
                   const uint8_t *buf, size_t length,
                   struct ast_file *file_out,
                   struct error_info *error_info_out);

#endif /* KIRA_PARSE_H_ */
