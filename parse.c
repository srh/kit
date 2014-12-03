#include "parse.h"

#include "ast.h"
#include "identmap.h"
#include "slice.h"
#include "util.h"

struct ps {
  const uint8_t *data;
  size_t length;
  size_t pos;

  struct ident_map ident_table;
};

struct ps_savestate {
  size_t pos;
};

void ps_init(struct ps *p, const uint8_t *data, size_t length) {
  p->data = data;
  p->length = length;
  p->pos = 0;

  ident_map_init(&p->ident_table);
}

void ps_destroy(struct ps *p) {
  ident_map_destroy(&p->ident_table);
  p->data = NULL;
  p->length = 0;
  p->pos = 0;
}

int32_t ps_peek(struct ps *p) {
  CHECK(p->pos <= p->length);
  if (p->pos == p->length) {
    return -1;
  }
  return p->data[p->pos];
}

void ps_step(struct ps *p) {
  CHECK(p->pos < p->length);
  p->pos++;
}

struct ps_savestate ps_save(struct ps *p) {
  struct ps_savestate ret;
  ret.pos = p->pos;
  return ret;
}

void ps_restore(struct ps *p, struct ps_savestate save) {
  CHECK(save.pos <= p->pos);
  p->pos = save.pos;
}

ident_value ps_intern_ident(struct ps *p,
			    struct ps_savestate ident_begin,
			    struct ps_savestate ident_end) {
  CHECK(ident_end.pos <= p->length);
  CHECK(ident_begin.pos <= ident_end.pos);
  return ident_map_intern(&p->ident_table,
			  p->data + ident_begin.pos,
			  ident_end.pos - ident_begin.pos);
}

int is_ws(int32_t ch) {
  STATIC_ASSERT(' ' == 32 && '\r' == 13 && '\n' == 10);
  return ch == ' ' || ch == '\r' || ch == '\n';
}

int is_one_of(const char *s, int32_t ch) {
  while (*s) {
    if (*s == ch) {
      return 1;
    }
    ++s;
  }
  return 0;
}

int is_operlike(int32_t ch) {
  return is_one_of("~!%^&*+=-/?<,>.;:", ch);
}

int is_parenlike(int32_t ch) {
  return is_one_of("()[]{}", ch);
}

int is_alpha(int32_t ch) {
  STATIC_ASSERT('z' == 'a' + 25 && 'Z' == 'A' + 25);
  return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z');
}

int is_decimal_digit(int32_t ch) {
  STATIC_ASSERT('9' == '0' + 9); /* redundant with C std, yes. */
  return '0' <= ch && ch <= '9';
}

int is_ident_firstchar(int32_t ch) {
  return is_alpha(ch) || ch == '_';
}

int is_ident_midchar(int32_t ch) {
  return is_ident_firstchar(ch) || is_decimal_digit(ch);
}

int is_ident_postchar(int32_t ch) {
  return is_ws(ch) || is_operlike(ch) || is_parenlike(ch);
}

void skip_ws(struct ps *p) {
  while (is_ws(ps_peek(p))) {
    ps_step(p);
  }
}

int skip_string(struct ps *p, const char *s) {
  for (;;) {
    if (!*s) {
      return 1;
    }
    CHECK(*s > 0);
    if (ps_peek(p) != *s) {
      return 0;
    }
    ps_step(p);
    s++;
  }
}

int try_skip_char(struct ps *p, char ch) {
  CHECK(ch > 0);
  if (ps_peek(p) == ch) {
    ps_step(p);
    return 1;
  }
  return 0;
}

int try_skip_semicolon(struct ps *p) {
  return try_skip_char(p, ';');
}

int skip_oper(struct ps *p, const char *s) {
  if (!skip_string(p, s)) {
    return 0;
  }
  if (is_operlike(ps_peek(p))) {
    return 0;
  }
  return 1;
}

int try_skip_keyword(struct ps *p, const char *kw) {
  struct ps_savestate save = ps_save(p);
  if (!skip_string(p, kw)) {
    ps_restore(p, save);
    return 0;
  }
  if (!is_ident_postchar(ps_peek(p))) {
    ps_restore(p, save);
    return 0;
  }
  return 1;
}

int parse_ident(struct ps *p, struct ast_ident *out) {
  if (!is_ident_firstchar(ps_peek(p))) {
    return 0;
  }
  struct ps_savestate save = ps_save(p);
  ps_step(p);
  while (is_ident_midchar(ps_peek(p))) {
    ps_step(p);
  }

  if (!is_ident_postchar(ps_peek(p))) {
    return 0;
  }

  out->value = ps_intern_ident(p, save, ps_save(p));
  return 0;
}

int parse_expr(struct ps *p, struct ast_expr *out) {
  (void)p, (void)out;
  /* TODO: Implement. */
  return 0;
}

int parse_typeexpr(struct ps *p, struct ast_typeexpr *out) {
  struct ast_ident ident;
  if (!parse_ident(p, &ident)) {
    return 0;
  }

  skip_ws(p);
  if (!try_skip_char(p, '[')) {
    out->tag = AST_TYPEEXPR_NAME;
    out->u.name = ident;
    return 1;
  }

  struct ast_typeexpr *params = NULL;
  size_t params_count = 0;
  size_t params_limit = 0;

  for (;;) {
    skip_ws(p);
    if (try_skip_char(p, ']')) {
      out->tag = AST_TYPEEXPR_APP;
      out->u.app.name = ident;
      out->u.app.params = params;
      out->u.app.params_count = params_count;
      return 1;
    }

    if (params_count != 0) {
      if (!try_skip_char(p, ',')) {
	goto fail;
      }
      skip_ws(p);
    }

    struct ast_typeexpr typeexpr;
    if (!parse_typeexpr(p, &typeexpr)) {
      goto fail;
    }
    SLICE_PUSH(params, params_count, params_limit, typeexpr);
  }

 fail:
  SLICE_FREE(params, params_count, ast_typeexpr_destroy);
  ast_ident_destroy(&ident);
  return 0;
}

int parse_rest_of_def(struct ps *p, struct ast_def *out) {
  struct ast_def def;

  skip_ws(p);
  if (!parse_ident(p, &def.name)) {
    return 0;
  }

  skip_ws(p);
  if (!parse_typeexpr(p, &def.type)) {
    goto fail_ident;
  }

  skip_ws(p);
  if (!skip_oper(p, "=")) {
    goto fail_ident;
  }

  skip_ws(p);
  if (!parse_expr(p, &def.rhs)) {
    goto fail_typeexpr;
  }

  if (!try_skip_semicolon(p)) {
    goto fail_rhs;
  }

  *out = def;
  return 1;

 fail_rhs:
  ast_expr_destroy(&def.rhs);
 fail_typeexpr:
  ast_typeexpr_destroy(&def.type);
 fail_ident:
  ast_ident_destroy(&def.name);
  return 0;
}

int parse_rest_of_import(struct ps *p, struct ast_import *out) {
  skip_ws(p);
  struct ast_ident ident;
  if (!parse_ident(p, &ident)) {
    return 0;
  }
  skip_ws(p);
  if (!try_skip_semicolon(p)) {
    ast_ident_destroy(&ident);
    return 0;
  }
  out->ident = ident;
  return 1;
}

int parse_toplevel(struct ps *p, struct ast_toplevel *out);

int parse_rest_of_module(struct ps *p, struct ast_module *out) {
  skip_ws(p);
  struct ast_ident name;
  if (!parse_ident(p, &name)) {
    return 0;
  }
  skip_ws(p);
  if (!try_skip_char(p, '{')) {
    ast_ident_destroy(&name);
    return 0;
  }

  struct ast_toplevel *toplevels = NULL;
  size_t toplevels_count = 0;
  size_t toplevels_limit = 0;
  for (;;) {
    skip_ws(p);

    if (try_skip_char(p, '}')) {
      out->name = name;
      out->toplevels = toplevels;
      out->toplevels_count = toplevels_count;
      return 1;
    }

    struct ast_toplevel toplevel;
    if (!parse_toplevel(p, &toplevel)) {
      SLICE_FREE(toplevels, toplevels_count, ast_toplevel_destroy);
      ast_ident_destroy(&name);
      return 0;
    }
    SLICE_PUSH(toplevels, toplevels_count, toplevels_limit, toplevel);
  }
}


int parse_toplevel(struct ps *p, struct ast_toplevel *out) {
  if (try_skip_keyword(p, "def")) {
    out->tag = AST_TOPLEVEL_DEF;
    return parse_rest_of_def(p, &out->u.def);
  } else if (try_skip_keyword(p, "import")) {
    out->tag = AST_TOPLEVEL_IMPORT;
    return parse_rest_of_import(p, &out->u.import);
  } else if (try_skip_keyword(p, "module")) {
    out->tag = AST_TOPLEVEL_MODULE;
    return parse_rest_of_module(p, &out->u.module);
  } else {
    return 0;
  }
}

int parse_file(struct ps *p, struct ast_file *out) {
  struct ast_toplevel *toplevels = NULL;
  size_t toplevels_count = 0;
  size_t toplevels_limit = 0;
  for (;;) {
    skip_ws(p);

    if (ps_peek(p) == -1) {
      ast_file_init(out, toplevels, toplevels_count);
      return 1;
    }

    struct ast_toplevel toplevel;
    if (!parse_toplevel(p, &toplevel)) {
      SLICE_FREE(toplevels, toplevels_count, ast_toplevel_destroy);
      return 0;
    }
    SLICE_PUSH(toplevels, toplevels_count, toplevels_limit, toplevel);
  }
}
