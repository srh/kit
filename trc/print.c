#include "print.h"

#include <stdio.h>

#include "arith.h"
#include "ast.h"
#include "databuf.h"
#include "identmap.h"
#include "typecheck.h"

void sprint_ident(struct databuf *b, struct identmap *im, struct ast_ident *a) {
  const void *buf;
  size_t count;
  identmap_lookup(im, a->value, &buf, &count);
  databuf_append(b, buf, count);
}

void sprint_vardecl(struct databuf *b, struct identmap *im, struct ast_vardecl *a) {
  sprint_ident(b, im, &a->name);
  databuf_append_c_str(b, " ");
  sprint_typeexpr(b, im, &a->type);
}

void help_append_fields(struct databuf *b, struct identmap *im,
                        struct ast_vardecl *fields, size_t fields_count) {
  databuf_append_c_str(b, " { ");
  for (size_t i = 0; i < fields_count; i++) {
    sprint_vardecl(b, im, &fields[i]);
    databuf_append_c_str(b, "; ");
  }
  databuf_append_c_str(b, "}");
}

void sprint_numeric_literal(struct databuf *b, struct ast_numeric_literal *a) {
  STATIC_CHECK('9' - '0' == 9); /* Shut up, I know. */
  /* We don't print dec/hex versions, so that string equality matches
  type equality.  We will use these strings for a type identmap. */
  uint32_t x = unsafe_numeric_literal_u32(a);
  if (x == 0) {
    char ch = '0';
    databuf_append(b, &ch, 1);
  } else {
    char buf[50] = { 0 };
    size_t i = 0;
    do {
      buf[i] = '0' + (x % 10);
      x /= 10;
      i++;
    } while (x != 0);
    while (i > 0) {
      i--;
      databuf_append(b, &buf[i], 1);
    }
  }
}

/* This produces equal outputs for equal types -- is used for
cs->typetrav_symbol_infos. */
void sprint_typeexpr(struct databuf *b, struct identmap *im, struct ast_typeexpr *a) {
  switch (a->tag) {
  case AST_TYPEEXPR_NAME: {
    sprint_ident(b, im, &a->u.name);
  } break;
  case AST_TYPEEXPR_APP: {
    sprint_ident(b, im, &a->u.app.name);
    sprint_type_param_list(b, im, a->u.app.params, a->u.app.params_count);
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    databuf_append_c_str(b, "struct");
    help_append_fields(b, im, a->u.structe.fields, a->u.structe.fields_count);
  } break;
  case AST_TYPEEXPR_UNIONE: {
    databuf_append_c_str(b, "union");
    help_append_fields(b, im, a->u.unione.fields, a->u.unione.fields_count);
  } break;
  case AST_TYPEEXPR_ARRAY: {
    databuf_append_c_str(b, "[");
    sprint_numeric_literal(b, &a->u.arraytype.number);
    databuf_append_c_str(b, "]");
    sprint_typeexpr(b, im, a->u.arraytype.param);
  } break;
  case AST_TYPEEXPR_UNKNOWN: {
    databuf_append_c_str(b, "_");
  } break;
  case AST_TYPEEXPR_NUMERIC: {
    databuf_append_c_str(b, "<numeric>");
  } break;
  default:
    databuf_append_c_str(b, "?" "?" "?typeexpr");
    break;
  }
}

void sprint_type_param_list(struct databuf *b, struct identmap *im, struct ast_typeexpr *types, size_t types_count) {
  databuf_append_c_str(b, "[");
  for (size_t i = 0; i < types_count; i++) {
    if (i) {
      databuf_append_c_str(b, ", ");
    }
    sprint_typeexpr(b, im, &types[i]);
  }
  databuf_append_c_str(b, "]");
}

void DBG_typeexpr(const char *msg, struct identmap *im, struct ast_typeexpr *a) {
  struct databuf b;
  databuf_init(&b);
  sprint_typeexpr(&b, im, a);
  DBG("%s %.*s\n", msg, size_to_int(b.count), b.buf);
  databuf_destroy(&b);
}
