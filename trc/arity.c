#include "arity.h"
#include "ast.h"

struct generics_arity make_arity(size_t value) {
  struct generics_arity ret;
  ret.value = value;
  return ret;
}

struct generics_arity no_param_list_arity(void) {
  return make_arity(ARITY_NO_PARAMLIST);
}

struct generics_arity param_list_arity(size_t arity) {
  CHECK(arity != ARITY_NO_PARAMLIST);
  return make_arity(arity);
}

struct generics_arity params_arity(struct ast_generics *a) {
  return make_arity(a->has_type_params ? a->params.count : ARITY_NO_PARAMLIST);
}

int arity_no_paramlist(struct generics_arity arity) {
  return arity.value == ARITY_NO_PARAMLIST;
}
