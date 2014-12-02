#include "parse.h"

#include "ast.h"
#include "slice.h"
#include "util.h"

struct ident_bounds {
  size_t begin;
  size_t end;
};

struct ps {
  const uint8_t *data;
  size_t length;
  size_t pos;

  struct ident_bounds *ident_table;
  size_t ident_table_count;
  size_t ident_table_limit;
};

struct ps_savestate {
  size_t pos;
};

void ps_init(struct ps *p, const uint8_t *data, size_t length) {
  p->data = data;
  p->length = length;
  p->pos = 0;

  p->ident_table = NULL;
  p->ident_table_count = 0;
  p->ident_table_limit = 0;
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

uint32_t ps_intern_ident(struct ps *p,
			 struct ps_savestate ident_begin,
			 struct ps_savestate ident_end) {
  /* TODO: Actually dedup idents. */
  (void)ident_end;
  CHECK(ident_begin.pos <= p->length);
  CHECK(p->ident_table_count <= UINT32_MAX);
  uint32_t ret = p->ident_table_count;

  struct ident_bounds bounds;
  bounds.begin = ident_begin.pos;
  bounds.end = ident_end.pos;
  SLICE_PUSH(p->ident_table, p->ident_table_count, p->ident_table_limit,
	     bounds);
  return ret;
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

int parse_rest_of_def(struct ps *p, struct ast_def *out) {
  (void)p, (void)out;
  /* TODO: Implement. */
  return 0;
}

int parse_rest_of_import(struct ps *p, struct ast_import *out) {
  skip_ws(p);
  if (!parse_ident(p, &out->ident)) {
    return 0;
  }
  skip_ws(p);
  if (!try_skip_semicolon(p)) {
    return 0;
  }
  return 1;
}

int parse_toplevel(struct ps *p, struct ast_toplevel *out);

int parse_rest_of_module(struct ps *p, struct ast_module *out) {
  skip_ws(p);
  if (!parse_ident(p, &out->name)) {
    return 0;
  }
  skip_ws(p);
  if (!try_skip_char(p, '{')) {
    return 0;
  }

  struct ast_toplevel *toplevels = NULL;
  size_t toplevels_count = 0;
  size_t toplevels_limit = 0;
  for (;;) {
    skip_ws(p);

    if (try_skip_char(p, '}')) {
      out->toplevels = toplevels;
      out->toplevels_count = toplevels_count;
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
