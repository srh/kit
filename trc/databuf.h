#ifndef KIT_DATABUF_H_
#define KIT_DATABUF_H_

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

void databuf_append_le_u32(struct databuf *b, uint32_t x);
void databuf_append_le_u16(struct databuf *b, uint16_t x);

#endif /* KIT_DATABUF_H_ */
