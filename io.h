#ifndef KIRA_IO_H_
#define KIRA_IO_H_

#include <stddef.h>
#include <stdint.h>

int read_file(const char *path,
              uint8_t **data_out,
              size_t *size_out);

#endif /* KIRA_IO_H_ */
