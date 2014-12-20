#ifndef KIRA_PARSE_H_
#define KIRA_PARSE_H_

#include <stddef.h>
#include <stdint.h>

struct ast_file;
struct ident_map;

int parse_test(void);
int parse_buf_file(struct ident_map *im,
                   const uint8_t *buf, size_t length,
                   struct ast_file *file_out,
                   size_t *error_pos_out);

#endif /* KIRA_PARSE_H_ */
