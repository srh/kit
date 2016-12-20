#ifndef KIT_IO_H_
#define KIT_IO_H_

#include <stddef.h>
#include <stdint.h>

int read_file(const char *path,
              uint8_t **data_out,
              size_t *size_out);

int write_file(const char *path,
               const void *buf,
               size_t buf_size);

#endif /* KIT_IO_H_ */
