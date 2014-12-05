#ifndef KIRA_PARSE_H_
#define KIRA_PARSE_H_

#include <stddef.h>
#include <stdint.h>

int parse_test(void);
int count_parse_buf(const uint8_t *buf, size_t length,
		    size_t *leafcount_out, size_t *error_pos_out);

#endif /* KIRA_PARSE_H_ */
