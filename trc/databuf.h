#ifndef KIRA_DATABUF_H_
#define KIRA_DATABUF_H_

#include <stddef.h>
#include <stdint.h>

struct databuf {
  uint8_t *buf;
  size_t count;
  size_t limit;
};

void databuf_init(struct databuf *b);
void databuf_destroy(struct databuf *b);
void databuf_move_destroy(struct databuf *b,
                          void **buf_out, size_t *count_out);
void databuf_append(struct databuf *b, const void *p, size_t count);
void databuf_append_c_str(struct databuf *b, const char *s);
void databuf_overwrite(struct databuf *b, size_t offset,
                       const void *p, size_t count);

#endif /* KIRA_DATABUF_H_ */
