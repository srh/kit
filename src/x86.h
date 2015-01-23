#ifndef KIRA_X86_H_
#define KIRA_X86_H_

#include <stdint.h>

struct checkstate;
struct opgraph;
struct objfile;

int gen_x86_function(struct checkstate *cs, struct objfile *f,
                     struct opgraph *g, uint32_t *symbol_Value_out);

#endif /* KIRA_X86_H_ */

