#include "typeexpr.h"

#include <stdio.h>

#include "arith.h"
#include "ast.h"
#include "databuf.h"
#include "identmap.h"

void sprint_ident(struct databuf *b, struct identmap *im, struct ast_ident *a) {
  void *buf;
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
  switch (a->tag) {
  case AST_NUMERIC_LITERAL_DEC: {
    for (size_t i = 0, e = a->digits_count; i < e; i++) {
      char c = '0' + a->digits[i];
      databuf_append(b, &c, 1);
    }
  } break;
  case AST_NUMERIC_LITERAL_HEX: {
    STATIC_CHECK('F' - 'A' == 5);
    databuf_append_c_str(b, "0x");
    for (size_t i = 0, e = a->digits_count; i < e; i++) {
      int8_t value = a->digits[i];
      char c = value < 10 ? '0' + value : 'A' + (value - 10);
      databuf_append(b, &c, 1);
    }
  } break;
  default:
    databuf_append_c_str(b, "?" "?" "?numeric_literal");
    break;
  }
}

void sprint_typeexpr(struct databuf *b, struct identmap *im, struct ast_typeexpr *a) {
  switch (a->tag) {
  case AST_TYPEEXPR_NAME: {
    sprint_ident(b, im, &a->u.name);
  } break;
  case AST_TYPEEXPR_APP: {
    sprint_ident(b, im, &a->u.app.name);
    databuf_append_c_str(b, "[");
    for (size_t i = 0, e = a->u.app.params_count; i < e; i++) {
      if (i != 0) {
        databuf_append_c_str(b, ", ");
      }
      sprint_typeexpr(b, im, &a->u.app.params[i]);
    }
    databuf_append_c_str(b, "]");
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

void DBG_typeexpr(const char *msg, struct identmap *im, struct ast_typeexpr *a) {
  struct databuf b;
  databuf_init(&b);
  sprint_typeexpr(&b, im, a);
  DBG("%s %.*s\n", msg, size_to_int(b.count), b.buf);
  databuf_destroy(&b);
}
