#ifndef KIRA_TYPECHECK_H_
#define KIRA_TYPECHECK_H_

#include <stddef.h>
#include <stdint.h>

#include "identmap.h"

typedef int module_loader(const uint8_t *module_name,
			  size_t module_name_count,
			  uint8_t **data_out,
			  size_t *data_count_out);

int check_module(struct ident_map *im, module_loader *loader, ident_value name);

int test_check_file(void);

int read_module_file(const uint8_t *module_name,
		     size_t module_name_count,
		     uint8_t **data_out,
		     size_t *data_size_out);

#endif /* KIRA_TYPECHECK_H_ */
