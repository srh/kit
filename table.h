#ifndef KIRA_TABLE_H_
#define KIRA_TABLE_H_

#include "ast.h"

struct def_entry {
  ident_value name;
  struct ast_optional_type_params generics;
  struct ast_typeexpr type;
  struct ast_def *def;
};

struct generics_arity {
  /* SIZE_MAX means no param list, 0 means an empty param list. */
  size_t value;
};

struct deftype_entry {
  ident_value name;
  struct generics_arity arity;
  struct ast_deftype *deftype;
};

struct name_table {
  struct def_entry *defs;
  size_t defs_count;
  size_t defs_limit;

  struct deftype_entry *deftypes;
  size_t deftypes_count;
  size_t deftypes_limit;
};

void name_table_init(struct name_table *t);
void name_table_destroy(struct name_table *t);

int name_table_add_def(struct name_table *t,
		       ident_value name,
		       struct ast_optional_type_params *generics,
		       struct ast_typeexpr *type,
		       struct ast_def *def);
int name_table_add_deftype(struct name_table *t,
			   ident_value name,
			   struct generics_arity arity,
			   struct ast_deftype *deftype);

int name_table_lookup_def(struct name_table *t,
			  ident_value name,
			  struct generics_arity arity,
			  struct def_entry **out);

int name_table_lookup_deftype(struct name_table *t,
			      ident_value name,
			      struct generics_arity arity,
			      struct deftype_entry **out);

#endif /* KIRA_TABLE_H_ */
