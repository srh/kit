#ifndef KIT_ARITY_H_
#define KIT_ARITY_H_

#include <stddef.h>

struct ast_generics;

#define ARITY_NO_PARAMLIST SIZE_MAX

struct generics_arity {
  /* ARITY_NO_PARAMLIST means no param list, 0 means an empty param list. */
  size_t value;
};

struct generics_arity params_arity(struct ast_generics *a);

struct generics_arity no_param_list_arity(void);
struct generics_arity param_list_arity(size_t arity);
int arity_no_paramlist(struct generics_arity arity);

#endif /* KIT_ARITY_H_ */
