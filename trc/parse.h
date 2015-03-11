#ifndef KIRA_PARSE_H_
#define KIRA_PARSE_H_

#include <stddef.h>
#include <stdint.h>

#include "databuf.h"
#include "pos.h"

struct ast_file;
struct identmap;

struct error_dump {
  void (*dumper)(struct error_dump *ctx, struct identmap *im,
                 struct pos pos, const char *msg, size_t msglen);
};

int parse_test(void);
int parse_buf_file(struct identmap *im,
                   const uint8_t *buf, size_t length,
                   size_t global_offset,
                   ident_value filename,
                   struct ast_file *file_out,
                   struct error_dump *error_dump);

#endif /* KIRA_PARSE_H_ */
