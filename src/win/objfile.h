#ifndef KIRA_WIN_OBJFILE_H_
#define KIRA_WIN_OBJFILE_H_

#include <stddef.h>

int make_blank_objfile(void **buf_out, size_t *count_out);
int make_almost_blank_objfile(void **buf_out, size_t *count_out);

#endif /* KIRA_WIN_OBJFILE_H_ */
