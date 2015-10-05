#include "parse.h"

#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "identmap.h"
#include "slice.h"
#include "typecheck.h"
#include "util.h"

#define PARSE_DBG(...)

struct ps {
  const uint8_t *data;
  size_t length;
  size_t pos;

  size_t line;
  size_t column;

  size_t global_offset_base;

  struct identmap im;
  ident_value ptr_ident;

  size_t leafcount;
  struct error_dump *error_dump;
};

struct ps_savestate {
  size_t pos;
  size_t line;
  size_t column;
  size_t leafcount;
};

void ps_init(struct ps *p, struct error_dump *error_dump, const uint8_t *data,
             size_t length,
             size_t global_offset_base) {
  /* This check lets ps_pos avoid a check. */
  CHECK(SIZE_MAX - length >= global_offset_base);
  p->data = data;
  p->length = length;
  p->pos = 0;

  p->line = 1;
  p->column = 0;

  p->global_offset_base = global_offset_base;

  identmap_init(&p->im);
  p->ptr_ident = identmap_intern_c_str(&p->im, PTR_TYPE_NAME);
  p->leafcount = 0;
  p->error_dump = error_dump;
}

/* Takes ownership of the ident table -- use ps_remove_identmap to get
it back. */
void ps_init_with_identmap(struct ps *p, struct identmap *im,
                           struct error_dump *error_dump,
                           const uint8_t *data, size_t length,
                           size_t global_offset_base) {
  /* This check lets ps_pos avoid a check. */
  CHECK(SIZE_MAX - length >= global_offset_base);
  p->data = data;
  p->length = length;
  p->pos = 0;

  p->line = 1;
  p->column = 0;

  p->global_offset_base = global_offset_base;

  identmap_init_move(&p->im, im);
  p->ptr_ident = identmap_intern_c_str(&p->im, PTR_TYPE_NAME);
  p->leafcount = 0;
  p->error_dump = error_dump;
}

void ps_remove_identmap(struct ps *p, struct identmap *im_out) {
  identmap_init_move(im_out, &p->im);
  identmap_init(&p->im);
}

void ps_destroy(struct ps *p) {
  p->data = NULL;
  p->length = 0;
  p->pos = 0;
  p->line = 0;
  p->column = 0;
  identmap_destroy(&p->im);
  p->leafcount = 0;
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
  int32_t ch = p->data[p->pos];
  if (ch == '\n') {
    p->line = size_add(p->line, 1);
    p->column = 0;
  } else if (ch == '\t') {
    p->column = size_add((p->column | 7), 1);
  } else {
    p->column = size_add(p->column, 1);
  }

  p->pos++;
}

struct ps_savestate ps_save(struct ps *p) {
  struct ps_savestate ret;
  ret.pos = p->pos;
  ret.line = p->line;
  ret.column = p->column;
  ret.leafcount = p->leafcount;
  return ret;
}

struct pos ps_pos(struct ps *p) {
  struct pos ret;
  /* We could safely do raw addition here because of the check in
  ps_init that p->length + p->global_offset_base won't overflow. */
  ret.global_offset = size_add(p->pos, p->global_offset_base);
  return ret;
}

void ps_restore(struct ps *p, struct ps_savestate save) {
  CHECK(save.pos <= p->pos);
  CHECK(save.leafcount <= p->leafcount);
  p->pos = save.pos;
  p->line = save.line;
  p->column = save.column;
  p->leafcount = save.leafcount;
}

void ps_count_leaf(struct ps *p) {
  CHECK(p->leafcount != SIZE_MAX);
  p->leafcount++;
}

enum tri {
  TRI_SUCCESS,
  TRI_QUICKFAIL,
  TRI_ERROR,
};

enum tri success_or_fail(int success) {
  return success ? TRI_SUCCESS : TRI_ERROR;
}

ident_value ps_intern_ident(struct ps *p,
                            size_t begin_global_offset,
                            size_t end_global_offset) {
  size_t begin_pos = size_sub(begin_global_offset, p->global_offset_base);
  size_t end_pos = size_sub(end_global_offset, p->global_offset_base);
  CHECK(end_pos <= p->length);
  CHECK(begin_pos <= end_pos);
  return identmap_intern(&p->im,
                         p->data + begin_pos,
                         end_pos - begin_pos);
}

struct precedence_pair {
  int left_precedence;
  int right_precedence;
};

const int kSemicolonPrecedence = 205;
/* There is no comma operator, so no intermingling of commas and
semicolons. */
const int kCommaPrecedence = 205;

const int kConversionRightPrecedence = 905;

const struct precedence_pair binop_precedences[] = {
  [AST_BINOP_ASSIGN] = { 306, 304 },
  [AST_BINOP_ADD] = { 504, 506 },
  [AST_BINOP_SUB] = { 504, 506 },
  [AST_BINOP_MUL] = { 604, 606 },
  [AST_BINOP_DIV] = { 604, 606 },
  [AST_BINOP_MOD] = { 605, 605 },
  [AST_BINOP_LT] = { 405, 405 },
  [AST_BINOP_LE] = { 405, 405 },
  [AST_BINOP_GT] = { 405, 405 },
  [AST_BINOP_GE] = { 405, 405 },
  [AST_BINOP_EQ] = { 405, 405 },
  [AST_BINOP_NE] = { 405, 405 },
  /* TODO: Let bitwise ops self-associate but require parens for any
  combination of them with other operators. */
  [AST_BINOP_BIT_XOR] = { 405, 405 },
  [AST_BINOP_BIT_OR] = { 405, 405 },
  [AST_BINOP_BIT_AND] = { 405, 405 },
  [AST_BINOP_BIT_LEFTSHIFT] = { 405, 405 },
  [AST_BINOP_BIT_RIGHTSHIFT] = { 405, 405 },
  [AST_BINOP_LOGICAL_OR] = { 356, 354 },
  [AST_BINOP_LOGICAL_AND] = { 376, 374 },
};

struct precedence_pair binop_precedence(enum ast_binop op) {
  CHECK(0 <= op &&
        op < sizeof(binop_precedences) / sizeof(binop_precedences[0]));
  return binop_precedences[op];
}

const int unop_precedences[] = {
  [AST_UNOP_DEREFERENCE] = 905,
  [AST_UNOP_ADDRESSOF] = 905,
  [AST_UNOP_NEGATE] = 905,
  [AST_UNOP_CONVERT] = 905,
  [AST_UNOP_LOGICAL_NOT] = 905,
  [AST_UNOP_BITWISE_NOT] = 905,
};

int unop_right_precedence(enum ast_unop op) {
  CHECK(0 <= op &&
        op < sizeof(unop_precedences) / sizeof(unop_precedences[0]));
  return unop_precedences[op];
}


enum precedence_comparison {
  PRECEDENCE_COMPARISON_CONFLICTS,
  PRECEDENCE_COMPARISON_PULLS_LEFT,
  PRECEDENCE_COMPARISON_PULLS_RIGHT,
};

enum precedence_comparison compare_precedence(int left, int right) {
  if (left + 1 < right) {
    return PRECEDENCE_COMPARISON_PULLS_RIGHT;
  }
  if (right + 1 < left) {
    return PRECEDENCE_COMPARISON_PULLS_LEFT;
  }
  return PRECEDENCE_COMPARISON_CONFLICTS;
}


int is_ws(int32_t ch) {
  STATIC_CHECK(' ' == 32 && '\r' == 13 && '\n' == 10 && '\t' == 9);
  return ch == ' ' || ch == '\r' || ch == '\n' || ch == '\t';
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
  return is_one_of("~!%^&*+=-/?<,>.:;|@", ch);
}

int is_binop_start(int32_t ch) {
  return is_one_of("!%^&*+=-/<>|:", ch);
}

int is_parenlike(int32_t ch) {
  return is_one_of("()[]{}", ch);
}

int is_alpha(int32_t ch) {
  STATIC_CHECK('z' == 'a' + 25 && 'Z' == 'A' + 25);
  return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z');
}

int is_decimal_digit(int32_t ch, int8_t *digit_value_out) {
  STATIC_CHECK('9' == '0' + 9); /* redundant with C std, yes. */
  if ('0' <= ch && ch <= '9') {
    *digit_value_out = (int8_t)(ch - '0');
    return 1;
  } else {
    return 0;
  }
}

int is_hex_digit(int32_t ch, int8_t *digit_value_out) {
  STATIC_CHECK('F' == 'A' + 5);
  STATIC_CHECK('f' == 'a' + 5);
  if (is_decimal_digit(ch, digit_value_out)) {
    return 1;
  }
  if ('a' <= ch && ch <= 'f') {
    *digit_value_out = (int8_t)((ch - 'a') + 10);
    return 1;
  } else if ('A' <= ch && ch <= 'F') {
    *digit_value_out = (int8_t)((ch - 'A') + 10);
    return 1;
  } else {
    return 0;
  }
}

int is_ident_firstchar(int32_t ch) {
  return is_alpha(ch) || ch == '_';
}

int is_ident_midchar(int32_t ch) {
  int8_t digit_value_discard;
  return is_ident_firstchar(ch) || is_decimal_digit(ch, &digit_value_discard);
}

int is_ident_postchar(int32_t ch) {
  return is_ws(ch) || is_operlike(ch) || is_parenlike(ch);
}

int is_numeric_postchar(int32_t ch) {
  return is_ident_postchar(ch);
}

int is_ok_in_comment(int32_t ch) {
  STATIC_CHECK(' ' == 32 && '~' == 126);
  return is_ws(ch) || (' ' <= ch && ch <= '~');
}

int is_typeexpr_firstchar(int32_t ch) {
  return is_ident_firstchar(ch) || ch == '[' || ch == '*';
}

/* Skips whitespace... including comments. */
int skip_ws(struct ps *p) {
 top:
  for (;;) {
    int32_t ch = ps_peek(p);
    if (is_ws(ch)) {
      ps_step(p);
    } else if (ch == '/') {
      struct ps_savestate save = ps_save(p);
      ps_step(p);
      int32_t ch2 = ps_peek(p);
      if (ch2 == '/') {
        ps_step(p);
        goto skip_past_line_comment;
      } else if (ch2 == '*') {
        ps_step(p);
        goto skip_past_star_comment;
      } else {
        ps_restore(p, save);
        return 1;
      }
    } else {
      return 1;
    }
  }

 skip_past_line_comment:
  for (;;) {
    STATIC_CHECK('\n' == 10);
    int32_t ch = ps_peek(p);
    if (ch == '\n') {
      ps_step(p);
      goto top;
    } else if (ch == -1) {
      return 1;
    } else if (is_ok_in_comment(ch)) {
      ps_step(p);
    } else {
      return 0;
    }
  }

 skip_past_star_comment:
  for (;;) {
    int32_t ch = ps_peek(p);
    if (ch == '*') {
      ps_step(p);
      int32_t ch2 = ps_peek(p);
      if (ch2 == '/') {
        ps_step(p);
        goto top;
      } else if (is_ok_in_comment(ch)) {
        ps_step(p);
      } else {
        return 0;
      }
    } else if (is_ok_in_comment(ch)) {
      ps_step(p);
    } else {
      return 0;
    }
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

int try_skip_tilder(struct ps *p, const char *s) {
  struct ps_savestate save = ps_save(p);
  if (skip_string(p, s)) {
    if (ps_peek(p) != '~') {
      ps_count_leaf(p);
      return 1;
    }
  }
  ps_restore(p, save);
  return 0;
}

int try_skip_char(struct ps *p, char ch) {
  CHECK(ch > 0);
  if (ps_peek(p) == ch) {
    ps_step(p);
    ps_count_leaf(p);
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
  if (is_operlike(ps_peek(p)) && ps_peek(p) != ';' && ps_peek(p) != ',') {
    return 0;
  }
  ps_count_leaf(p);
  return 1;
}

int try_skip_oper(struct ps *p, const char *s) {
  struct ps_savestate save = ps_save(p);
  if (skip_oper(p, s)) {
    return 1;
  } else {
    ps_restore(p, save);
    return 0;
  }
}

int try_parse_unop(struct ps *p, enum ast_unop *out, struct ast_ident *name_out) {
  struct pos start_pos = ps_pos(p);
  int32_t ch1 = ps_peek(p);
  if (is_one_of("*&-~!^", ch1)) {
    struct ps_savestate save = ps_save(p);
    ps_step(p);
    if (is_operlike(ps_peek(p))) {
      ps_restore(p, save);
      return 0;
    }
    enum ast_unop op = (ch1 == '*' ? AST_UNOP_DEREFERENCE
                        : ch1 == '&' ? AST_UNOP_ADDRESSOF
                        : ch1 == '-' ? AST_UNOP_NEGATE
                        : ch1 == '!' ? AST_UNOP_LOGICAL_NOT
                        : ch1 == '^' ? AST_UNOP_BITWISE_NOT
                        : AST_UNOP_CONVERT);
    uint8_t buf[1];
    buf[0] = (uint8_t)ch1;
    ident_value ident = identmap_intern(&p->im, buf, 1);
    ast_ident_init(name_out, ast_meta_make(start_pos, ps_pos(p)), ident);
    *out = op;
    ps_count_leaf(p);
    return 1;
  } else {
    return 0;
  }
}

int parse_binop(struct ps *p, enum ast_binop *out,
                struct ast_ident *name_out) {
  int32_t ch1 = ps_peek(p);
  if (!is_binop_start(ch1)) {
    return 0;
  }

  struct pos pos_start = ps_pos(p);
  enum ast_binop op;
  ps_step(p);
  switch (ch1) {
  case '=':
    if (ps_peek(p) == '=') {
      ps_step(p);
      op = AST_BINOP_EQ;
      goto done;
    }
    op = AST_BINOP_ASSIGN;
    goto done;
  case '+': op = AST_BINOP_ADD; goto done;
  case '-': op = AST_BINOP_SUB; goto done;
  case '*': op = AST_BINOP_MUL; goto done;
  case '/': op = AST_BINOP_DIV; goto done;
  case '%': op = AST_BINOP_MOD; goto done;
  case '<':
    if (ps_peek(p) == '=') {
      ps_step(p);
      op = AST_BINOP_LE;
      goto done;
    }
    if (ps_peek(p) == '<') {
      ps_step(p);
      op = AST_BINOP_BIT_LEFTSHIFT;
      goto done;
    }
    op = AST_BINOP_LT;
    goto done;
  case '>':
    if (ps_peek(p) == '=') {
      ps_step(p);
      op = AST_BINOP_GE;
      goto done;
    }
    if (ps_peek(p) == '>') {
      ps_step(p);
      op = AST_BINOP_BIT_RIGHTSHIFT;
      goto done;
    }
    op = AST_BINOP_GT;
    goto done;
  case '!':
    if (ps_peek(p) == '=') {
      ps_step(p);
      op = AST_BINOP_NE;
      goto done;
    }
    return 0;
  case '^': op = AST_BINOP_BIT_XOR; goto done;
  case '|':
    if (ps_peek(p) == '|') {
      ps_step(p);
      op = AST_BINOP_LOGICAL_OR;
      goto done;
    }
    op = AST_BINOP_BIT_OR;
    goto done;
  case '&':
    if (ps_peek(p) == '&') {
      ps_step(p);
      op = AST_BINOP_LOGICAL_AND;
      goto done;
    }
    op = AST_BINOP_BIT_AND;
    goto done;
  default:
    return 0;
  }

 done:
  if (is_operlike(ps_peek(p))) {
    return 0;
  } else {
    ps_count_leaf(p);
    *out = op;
    struct pos pos_end = ps_pos(p);
    ast_ident_init(name_out, ast_meta_make(pos_start, pos_end),
                   ps_intern_ident(p, pos_start.global_offset, pos_end.global_offset));
    return 1;
  }
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
  ps_count_leaf(p);
  return 1;
}

/* This doesn't really catch all keywords, for example, really "copy"
and "move" as used in defclass should be keywords?  We really have
valid typenames and valid expression literals and valid variable
names.  For example, "void" isn't here because it's a type name and
gets parsed by typechecking code, too.  But it should not be allowed
as the name of a variable.  TODO: Do more specific name-checking
instead. */
int is_ident_keyword(struct ps *p, size_t global_offset_begin, size_t global_offset_end) {
  size_t begin = size_sub(global_offset_begin, p->global_offset_base);
  size_t end = size_sub(global_offset_end, p->global_offset_base);
  CHECK(begin < end);
  CHECK(end <= p->length);
  size_t count = end - begin;
  const uint8_t *ptr = p->data + begin;
  switch (count) {
  case 3:
    return 0 == memcmp(ptr, "def", count);
  case 4:
    return 0 == memcmp(ptr, "true", count)
      || 0 == memcmp(ptr, "null", count)
      || 0 == memcmp(ptr, "else", count)
      || 0 == memcmp(ptr, "func", count);
  case 5:
    return 0 == memcmp(ptr, "while", count)
      || 0 == memcmp(ptr, "false", count)
      || 0 == memcmp(ptr, "union", count);
  case 6:
    return 0 == memcmp(ptr, "switch", count)
      || 0 == memcmp(ptr, "struct", count);
  case 7:
    return 0 == memcmp(ptr, "deftype", count)
      || 0 == memcmp(ptr, "defenum", count);
  case 8:
    return 0 == memcmp(ptr, "defclass", count);
  default:
    return 0;
  }
}

enum tri triparse_ident(struct ps *p, struct ast_ident *out) {
  PARSE_DBG("parse_ident\n");
  struct pos pos_start = ps_pos(p);
  if (try_skip_char(p, '`')) {
    enum ast_unop unop;
    struct ast_ident name;
    if (!try_parse_unop(p, &unop, &name)) {
      enum ast_binop binop;
      if (!parse_binop(p, &binop, &name)) {
        return TRI_ERROR;
      }
    }

    if (!try_skip_char(p, '`')) {
      ast_ident_destroy(&name);
      return TRI_ERROR;
    }

    if (!is_ident_postchar(ps_peek(p))) {
      return TRI_ERROR;
    }
    *out = name;
    return TRI_SUCCESS;
  }

  if (!is_ident_firstchar(ps_peek(p))) {
    return TRI_QUICKFAIL;
  }
  ps_step(p);
  while (is_ident_midchar(ps_peek(p))) {
    ps_step(p);
  }

  if (!is_ident_postchar(ps_peek(p))) {
    return TRI_ERROR;
  }

  struct pos pos_end = ps_pos(p);
  if (is_ident_keyword(p, pos_start.global_offset, pos_end.global_offset)) {
    return TRI_ERROR;
  }

  ast_ident_init(out, ast_meta_make(pos_start, pos_end),
                 ps_intern_ident(p, pos_start.global_offset, pos_end.global_offset));
  ps_count_leaf(p);
  return TRI_SUCCESS;
}

int parse_ident(struct ps *p, struct ast_ident *out) {
  return TRI_SUCCESS == triparse_ident(p, out);
}

enum allow_blanks {
  ALLOW_BLANKS_NO,
  ALLOW_BLANKS_YES,
};

int help_parse_typeexpr(struct ps *p, enum allow_blanks allow_blanks,
                        struct ast_typeexpr *out,
                        struct pos *pos_end_out);
int parse_typeexpr(struct ps *p, struct ast_typeexpr *out,
                   struct pos *pos_end_out);

int help_parse_vardecl(struct ps *p, enum allow_blanks allow_blanks, struct ast_vardecl *out) {
  struct pos pos_start = ps_pos(p);
  struct ast_ident name;
  if (!parse_ident(p, &name)) {
    goto fail;
  }
  if (!skip_ws(p)) {
    goto fail_name;
  }

  struct pos type_pos_end;
  struct ast_typeexpr type;
  if (allow_blanks == ALLOW_BLANKS_YES && !is_typeexpr_firstchar(ps_peek(p))) {
    type.tag = AST_TYPEEXPR_UNKNOWN;
    ast_unknown_init(&type.u.unknown, ast_meta_make(name.meta.pos_end, name.meta.pos_end));
    type_pos_end = ps_pos(p);
  } else {
    if (!help_parse_typeexpr(p, allow_blanks, &type, &type_pos_end)) {
      goto fail_name;
    }
  }

  ast_vardecl_init(out, ast_meta_make(pos_start, type_pos_end),
                   name, type);
  return 1;

 fail_name:
  ast_ident_destroy(&name);
 fail:
  return 0;
}

int parse_vardecl(struct ps *p, struct ast_vardecl *out) {
  return help_parse_vardecl(p, ALLOW_BLANKS_NO, out);
}

int parse_expr(struct ps *p, struct ast_expr *out, int precedence_context);

int parse_params_list(struct ps *p,
                      struct ast_vardecl **params_out,
                      size_t *params_count_out) {
  PARSE_DBG("parse_params_list\n");
  if (!try_skip_char(p, '(')) {
    return 0;
  }
  struct ast_vardecl *params = NULL;
  size_t params_count = 0;
  size_t params_limit = 0;

  for (;;) {
    if (!skip_ws(p)) {
      goto fail;
    }
    if (try_skip_char(p, ')')) {
      *params_out = params;
      *params_count_out = params_count;
      return 1;
    }

    if (params_count != 0) {
      if (!(try_skip_char(p, ',') && skip_ws(p))) {
        goto fail;
      }
    }

    struct ast_vardecl vardecl;
    if (!parse_vardecl(p, &vardecl)) {
      goto fail;
    }
    SLICE_PUSH(params, params_count, params_limit, vardecl);
  }

 fail:
  SLICE_FREE(params, params_count, ast_vardecl_destroy);
  return 0;
}

int parse_bracebody(struct ps *p, struct ast_bracebody *out);
int parse_constructor_pattern(struct ps *p, struct ast_constructor_pattern *out);

int parse_condition(struct ps *p, struct ast_condition *out) {
  if (try_skip_keyword(p, "case")) {
    struct ast_constructor_pattern pattern;
    if (!(skip_ws(p) && parse_constructor_pattern(p, &pattern))) {
      return 0;
    }
    struct ast_expr rhs;
    if (!(skip_ws(p) && try_skip_oper(p, "=") &&
          skip_ws(p) && parse_expr(p, &rhs, kSemicolonPrecedence))) {
      goto fail_pattern;
    }
    ast_condition_init_pattern(out, pattern, rhs);
    return 1;
  fail_pattern:
    ast_constructor_pattern_destroy(&pattern);
    return 0;
  }

  struct ast_expr expr;
  if (!parse_expr(p, &expr, kSemicolonPrecedence)) {
    return 0;
  }
  ast_condition_init(out, expr);
  return 1;
}

int parse_rest_of_if_statement(struct ps *p, struct pos pos_start,
                               struct ast_statement *out) {
  struct ast_condition condition;
  if (!(skip_ws(p) && parse_condition(p, &condition))) {
    goto fail;
  }

  struct ast_bracebody thenbody;
  if (!parse_bracebody(p, &thenbody)) {
    goto fail_condition;
  }

  struct pos pos_thenbody_end = ps_pos(p);

  if (!skip_ws(p)) {
    goto fail_thenbody;
  }
  if (!try_skip_keyword(p, "else")) {
    out->tag = AST_STATEMENT_IFTHEN;
    ast_ifthen_statement_init(&out->u.ifthen_statement,
                              ast_meta_make(pos_start, pos_thenbody_end),
                              condition,
                              thenbody);
    return 1;
  }

  if (!skip_ws(p)) {
    goto fail_thenbody;
  }

  struct ast_bracebody elsebody;
  struct pos if_pos = ps_pos(p);
  if (try_skip_keyword(p, "if")) {
    struct ast_statement elseif_statement;
    if (!parse_rest_of_if_statement(p, if_pos, &elseif_statement)) {
      goto fail_thenbody;
    }
    struct ast_statement *statements = malloc_mul(sizeof(*statements), 1);
    statements[0] = elseif_statement;
    ast_bracebody_init(&elsebody,
                       ast_meta_make(if_pos, ps_pos(p)), /* TODO: Inaccurate end pos. */
                       statements,
                       1);
  } else {
    if (!parse_bracebody(p, &elsebody)) {
      goto fail_thenbody;
    }
  }

  out->tag = AST_STATEMENT_IFTHENELSE;
  ast_ifthenelse_statement_init(&out->u.ifthenelse_statement,
                                ast_meta_make(pos_start, ps_pos(p)),
                                condition,
                                thenbody,
                                elsebody);
  return 1;

 fail_thenbody:
  ast_bracebody_destroy(&thenbody);
 fail_condition:
  ast_condition_destroy(&condition);
 fail:
  return 0;
}

int parse_rest_of_var_statement(struct ps *p, struct pos pos_start,
                                struct ast_var_statement *out) {
  struct ast_vardecl decl;
  if (!(skip_ws(p) && help_parse_vardecl(p, ALLOW_BLANKS_YES, &decl))) {
    goto fail;
  }

  if (!skip_ws(p)) {
    goto fail;
  }

  if (try_skip_semicolon(p)) {
    ast_var_statement_init_without_rhs(out, ast_meta_make(pos_start, ps_pos(p)),
                                       decl);
    return 1;
  }

  if (!try_skip_oper(p, "=")) {
    goto fail;
  }
  struct ast_expr rhs;
  if (!(skip_ws(p) && parse_expr(p, &rhs, kSemicolonPrecedence))) {
    goto fail_decl;
  }

  if (!try_skip_semicolon(p)) {
    goto fail_rhs;
  }

  ast_var_statement_init_with_rhs(out, ast_meta_make(pos_start, ps_pos(p)),
                                  decl, ast_exprcall_make(rhs));
  return 1;

 fail_rhs:
  ast_expr_destroy(&rhs);
 fail_decl:
  ast_vardecl_destroy(&decl);
 fail:
  return 0;
}

int parse_rest_of_while_statement(struct ps *p, struct pos pos_start,
                                  struct ast_while_statement *out) {
  struct ast_condition condition;
  if (!(skip_ws(p) && parse_condition(p, &condition))) {
    goto fail;
  }

  struct ast_bracebody body;
  if (!parse_bracebody(p, &body)) {
    goto fail_condition;
  }

  ast_while_statement_init(out, ast_meta_make(pos_start, ps_pos(p)),
                           condition, body);
  return 1;

 fail_condition:
  ast_condition_destroy(&condition);
 fail:
  return 0;
}

int parse_expr_statement(struct ps *p, struct ast_expr **out) {
  struct ast_expr expr;
  if (!parse_expr(p, &expr, kSemicolonPrecedence)) {
    return 0;
  }

  if (!try_skip_semicolon(p)) {
    ast_expr_destroy(&expr);
    return 0;
  }

  ast_expr_alloc_move(expr, out);
  return 1;
}

int parse_naked_var_or_expr_statement(struct ps *p, int force_assignment,
                                      struct ast_statement *out);

int parse_rest_of_for_statement(struct ps *p, struct pos pos_start,
                                struct ast_for_statement *out) {
  if (!skip_ws(p)) {
    goto fail;
  }

  struct pos initializer_start = ps_pos(p);
  int has_initializer;
  struct ast_statement initializer = { 0 };
  if (try_skip_semicolon(p)) {
    has_initializer = 0;
  } else {
    has_initializer = 1;
    if (try_skip_keyword(p, "var")) {
      initializer.tag = AST_STATEMENT_VAR;
      if (!parse_rest_of_var_statement(p, initializer_start, &initializer.u.var_statement)) {
        goto fail;
      }
    } else {
      if (!parse_naked_var_or_expr_statement(p, 1, &initializer)) {
        goto fail;
      }
    }
  }

  if (!skip_ws(p)) {
    goto fail_initializer;
  }

  int has_condition;
  struct ast_expr condition = { 0 };
  if (try_skip_semicolon(p)) {
    has_condition = 0;
  } else {
    has_condition = 1;
    if (!parse_expr(p, &condition, kSemicolonPrecedence)) {
      goto fail_initializer;
    }

    if (!try_skip_semicolon(p)) {
      goto fail_condition;
    }
  }

  if (!skip_ws(p)) {
    goto fail_condition;
  }

  int has_increment;
  struct ast_expr increment = { 0 };
  if (ps_peek(p) == '{') {
    has_increment = 0;
  } else {
    has_increment = 1;
    if (!parse_expr(p, &increment, kSemicolonPrecedence)) {
      goto fail_condition;
    }
  }

  struct ast_bracebody body;
  if (!parse_bracebody(p, &body)) {
    goto fail_increment;
  }

  struct ast_statement *ini = NULL;
  struct ast_expr *con = NULL;
  struct ast_expr *inc = NULL;
  if (has_initializer) {
    ast_statement_alloc_move(initializer, &ini);
  }
  if (has_condition) {
    ast_expr_alloc_move(condition, &con);
  }
  if (has_increment) {
    ast_expr_alloc_move(increment, &inc);
  }

  ast_for_statement_init(out, ast_meta_make(pos_start, ps_pos(p)),
                         has_initializer, ini,
                         has_condition, con,
                         has_increment, inc,
                         body);
  return 1;

 fail_increment:
  if (has_increment) {
    ast_expr_destroy(&increment);
  }
 fail_condition:
  if (has_condition) {
    ast_expr_destroy(&condition);
  }
 fail_initializer:
  if (has_initializer) {
    ast_statement_destroy(&initializer);
  }
 fail:
  return 0;
}

int parse_constructor_pattern(struct ps *p, struct ast_constructor_pattern *out) {
  struct pos pos_start = ps_pos(p);
  int addressof_constructor = try_skip_oper(p, "&");

  if (addressof_constructor) {
    if (!skip_ws(p)) {
      goto fail;
    }
  }

  struct ast_ident constructor_name;
  if (!parse_ident(p, &constructor_name)) {
    goto fail;
  }

  struct ast_vardecl decl;
  if (!(skip_ws(p) && try_skip_char(p, '(')
        && skip_ws(p) && help_parse_vardecl(p, ALLOW_BLANKS_YES, &decl))) {
    goto fail_constructor_name;
  }

  if (!(skip_ws(p) && try_skip_char(p, ')'))) {
    goto fail_decl;
  }

  ast_constructor_pattern_init(out, ast_meta_make(pos_start, ps_pos(p)),
                               addressof_constructor,
                               constructor_name, decl);
  return 1;

 fail_decl:
  ast_vardecl_destroy(&decl);
 fail_constructor_name:
  ast_ident_destroy(&constructor_name);
 fail:
  return 0;
}

int parse_rest_of_bracebody(struct ps *p, struct pos pos_start, struct ast_bracebody *out);
int parse_statement(struct ps *p, struct ast_statement *out);

int parse_cased_statement(struct ps *p, struct ast_cased_statement *out) {
  struct pos pos_start = ps_pos(p);
  struct ast_case_pattern pattern;
  if (try_skip_keyword(p, "default")) {
    /* TODO: The meta field is kind of different for a default ast_case_pattern. */
    ast_case_pattern_init_default(&pattern, ast_meta_make(pos_start, ps_pos(p)));
  } else {
    if (!try_skip_keyword(p, "case")) {
      goto fail;
    }

    struct ast_constructor_pattern constructor_pattern;
    if (!(skip_ws(p) && parse_constructor_pattern(p, &constructor_pattern))) {
      goto fail;
    }

    ast_case_pattern_init(&pattern, constructor_pattern);
  }

  if (!(skip_ws(p) && try_skip_char(p, ':') && skip_ws(p))) {
    goto fail_pattern;
  }

  struct ast_bracebody body;
  struct pos bracebody_start = ps_pos(p);
  if (try_skip_char(p, '{')) {
    if (!parse_rest_of_bracebody(p, bracebody_start, &body)) {
      goto fail_pattern;
    }
  } else {
    struct ast_statement *statements = NULL;
    size_t statements_count = 0;
    size_t statements_limit = 0;
    for (;;) {
      struct ps_savestate save = ps_save(p);
      if (try_skip_keyword(p, "case") || try_skip_keyword(p, "default") || try_skip_char(p, '}')) {
        /* Require at least one naked statement, so that it doesn't
        look like fall-through. */
        if (statements_count == 0) {
          goto fail_statements;
        }

        ps_restore(p, save);
        ast_bracebody_init(&body, ast_meta_make(bracebody_start, ps_pos(p)),
                           statements, statements_count);
        goto success_body;
      }
      struct ast_statement statement;
      if (!parse_statement(p, &statement)) {
        goto fail_statements;
      }
      SLICE_PUSH(statements, statements_count, statements_limit, statement);
      if (!skip_ws(p)) {
        goto fail_statements;
      }
    }
  fail_statements:
    SLICE_FREE(statements, statements_count, ast_statement_destroy);
    goto fail_pattern;
  }

 success_body:
  ast_cased_statement_init(out, ast_meta_make(pos_start, ps_pos(p)),
                           pattern, body);
  return 1;

 fail_pattern:
  ast_case_pattern_destroy(&pattern);
 fail:
  return 0;
}

int parse_rest_of_switch_statement(struct ps *p, struct pos pos_start,
                                   struct ast_switch_statement *out) {
  struct ast_expr swartch;
  if (!(skip_ws(p) && parse_expr(p, &swartch, kSemicolonPrecedence))) {
    goto fail;
  }

  if (!(skip_ws(p) && try_skip_char(p, '{'))) {
    goto fail_swartch;
  }

  struct ast_cased_statement *cases = NULL;
  size_t cases_count = 0;
  size_t cases_limit = 0;

  for (;;) {
    if (!skip_ws(p)) {
      goto fail_cases;
    }
    if (try_skip_char(p, '}')) {
      ast_switch_statement_init(out, ast_meta_make(pos_start, ps_pos(p)),
                                swartch, cases, cases_count);
      return 1;
    }
    struct ast_cased_statement cas;
    if (!parse_cased_statement(p, &cas)) {
      goto fail_cases;
    }
    SLICE_PUSH(cases, cases_count, cases_limit, cas);
  }

 fail_cases:
  SLICE_FREE(cases, cases_count, ast_cased_statement_destroy);
 fail_swartch:
  ast_expr_destroy(&swartch);
 fail:
  return 0;
}

enum tri triparse_numeric_literal(struct ps *p, struct ast_numeric_literal *out);
int parse_rest_of_index_param(struct ps *p, struct ast_expr *out);
int parse_after_atomic(struct ps *p, struct pos pos_start, struct ast_expr lhs,
                       int precedence_context, struct ast_expr *out);
enum tri help_triparse_typeexpr(struct ps *p, enum allow_blanks allow_blanks,
                                struct ast_typeexpr *out, struct pos *pos_end_out);

struct ambig_indexer {
  struct pos type_start;
  struct pos expr_end;
  struct ast_numeric_literal number;
};

void ambig_indexer_destroy(struct ambig_indexer *ambi) {
  ast_numeric_literal_destroy(&ambi->number);
}

void expressionize(struct ast_ident name,
                   struct ambig_indexer *indexers, size_t indexers_count,
                   struct ast_expr *out) {
  struct pos pos_start = name.meta.pos_start;
  struct ast_expr lhs;
  ast_expr_partial_init(&lhs, AST_EXPR_NAME, ast_expr_info_default());
  ast_name_expr_init(&lhs.u.name, name);
  for (size_t i = 0; i < indexers_count; i++) {
    struct ast_expr number_expr;
    ast_expr_partial_init(&number_expr, AST_EXPR_NUMERIC_LITERAL, ast_expr_info_default());
    number_expr.u.numeric_literal = indexers[i].number;
    struct ast_expr index_expr;
    ast_expr_partial_init(&index_expr, AST_EXPR_INDEX, ast_expr_info_default());
    ast_index_expr_init(&index_expr.u.index_expr, ast_meta_make(pos_start, indexers[i].expr_end),
                        lhs, number_expr);
    lhs = index_expr;
  }
  free(indexers);
  *out = lhs;
}

void collapse_indexers(struct ambig_indexer *indexers, size_t indexers_count,
                       struct ast_typeexpr param, struct pos param_pos_end,
                       struct ast_typeexpr *out) {
  for (size_t i = indexers_count; i > 0;) {
    i--;
    struct ast_typeexpr tmp;
    tmp.tag = AST_TYPEEXPR_ARRAY;
    ast_arraytype_init(&tmp.u.arraytype, ast_meta_make(indexers[i].type_start, param_pos_end),
                       indexers[i].number, param);
    param = tmp;
  }
  free(indexers);
  *out = param;
}

int parse_statement_after_atomic(struct ps *p, struct pos pos_start,
                                 struct ast_expr lhs, struct ast_statement *out) {
  struct ast_expr whole_expr;
  if (!parse_after_atomic(p, pos_start, lhs, kSemicolonPrecedence, &whole_expr)) {
    ast_expr_destroy(&lhs);
    return 0;
  }
  if (!try_skip_semicolon(p)) {
    ast_expr_destroy(&whole_expr);
    return 0;
  }
  out->tag = AST_STATEMENT_EXPR;
  ast_expr_alloc_move(whole_expr, &out->u.expr);
  return 1;
}

enum tri triparse_naked_var_or_expr_statement(
    struct ps *p, int force_assignment, struct ast_statement *out) {
  PARSE_DBG("%u:%u: triparse_naked_var\n", p->line, p->column);
  struct pos pos_start = ps_pos(p);
  struct ast_ident name;
  enum tri name_res = triparse_ident(p, &name);
  switch (name_res) {
  case TRI_ERROR:
    PARSE_DBG("%u:%u: triparse_naked_var name error\n", p->line, p->column);
    goto error;
  case TRI_QUICKFAIL:
    out->tag = AST_STATEMENT_EXPR;
    return success_or_fail(parse_expr_statement(p, &out->u.expr));
  case TRI_SUCCESS:
    break;
  default:
    UNREACHABLE();
  }

  if (!skip_ws(p)) {
    goto error_name;
  }

  struct ast_vardecl decl;
  {
    struct ambig_indexer *indexers = NULL;
    size_t indexers_count = 0;
    size_t indexers_limit = 0;

    struct ast_typeexpr type;
    struct pos type_pos_end;
    for (;;) {
      struct pos type_start = ps_pos(p);
      if (try_skip_char(p, '[')) {
        if (!skip_ws(p)) {
          goto error_name;
        }
        struct ambig_indexer ambi;
        enum tri numeric_res = triparse_numeric_literal(p, &ambi.number);
        switch (numeric_res) {
        case TRI_ERROR:
          goto error_name;
        case TRI_QUICKFAIL: {
          struct ast_expr arg;
          if (!parse_rest_of_index_param(p, &arg)) {
            goto error_indexers;
          }
          struct ast_expr lhs;
          expressionize(name, indexers, indexers_count, &lhs);
          struct ast_expr index_expr;
          ast_expr_partial_init(&index_expr, AST_EXPR_INDEX, ast_expr_info_default());
          ast_index_expr_init(&index_expr.u.index_expr, ast_meta_make(pos_start, ps_pos(p)),
                              lhs, arg);
          if (!parse_statement_after_atomic(p, pos_start, index_expr, out)) {
            return TRI_ERROR;
          } else {
            return TRI_SUCCESS;
          }
        } break;
        case TRI_SUCCESS:
          break;
        default:
          UNREACHABLE();
        }

        if (!(skip_ws(p) && try_skip_char(p, ']'))) {
          goto error_indexers;
        }

        ambi.type_start = type_start;
        ambi.expr_end = ps_pos(p);
        SLICE_PUSH(indexers, indexers_count, indexers_limit, ambi);
        if (!skip_ws(p)) {
          goto error_indexers;
        }
        continue;
      } else {
        struct ast_typeexpr local_type;
        struct pos local_type_pos_end;
        enum tri local_type_res = help_triparse_typeexpr(p, ALLOW_BLANKS_YES, &local_type,
                                                         &local_type_pos_end);
        switch (local_type_res) {
        case TRI_QUICKFAIL: {
          PARSE_DBG("%u:%u: triparse_naked_var whole expr\n", p->line, p->column);
          struct ast_expr lhs;
          expressionize(name, indexers, indexers_count, &lhs);
          return success_or_fail(parse_statement_after_atomic(p, pos_start, lhs, out));
        } break;
        case TRI_ERROR:
          goto error_indexers;
        case TRI_SUCCESS: {
          collapse_indexers(indexers, indexers_count, local_type, local_type_pos_end, &type);
          type_pos_end = local_type_pos_end;
          goto build_vardecl;
        } break;
        default:
          UNREACHABLE();
        }
      }
    }
  error_indexers:
    SLICE_FREE(indexers, indexers_count, ambig_indexer_destroy);
    goto error_name;
  build_vardecl:
    ast_vardecl_init(&decl, ast_meta_make(pos_start, type_pos_end), name, type);
  }

  if (!skip_ws(p)) {
    goto error_decl;
  }

  if (ps_peek(p) == ';') {
    if (force_assignment) {
      goto error_decl;
    } else {
      ps_step(p);
      ps_count_leaf(p);
      out->tag = AST_STATEMENT_VAR;
      ast_var_statement_init_without_rhs(&out->u.var_statement,
                                         ast_meta_make(pos_start, ps_pos(p)),
                                         decl);
      return TRI_SUCCESS;
    }
  }

  if (!try_skip_oper(p, "=")) {
    goto error_decl;
  }
  struct ast_expr rhs;
  if (!(skip_ws(p) && parse_expr(p, &rhs, kSemicolonPrecedence))) {
    goto error_decl;
  }

  if (!try_skip_semicolon(p)) {
    goto error_rhs;
  }

  out->tag = AST_STATEMENT_VAR;
  ast_var_statement_init_with_rhs(&out->u.var_statement, ast_meta_make(pos_start, ps_pos(p)),
                                  decl, ast_exprcall_make(rhs));
  return TRI_SUCCESS;

 error_rhs:
  ast_expr_destroy(&rhs);
 error_decl:
  ast_vardecl_destroy(&decl);
  return TRI_ERROR;
 error_name:
  ast_ident_destroy(&name);
 error:
  return TRI_ERROR;
}

int parse_naked_var_or_expr_statement(struct ps *p, int force_assignment,
                                      struct ast_statement *out) {
  return success_or_fail(triparse_naked_var_or_expr_statement(p, force_assignment, out));
}

int parse_statement(struct ps *p, struct ast_statement *out) {
  struct pos pos_start = ps_pos(p);
  if (try_skip_keyword(p, "var")) {
    out->tag = AST_STATEMENT_VAR;
    return parse_rest_of_var_statement(p, pos_start, &out->u.var_statement);
  } else if (try_skip_keyword(p, "return")) {
    if (!skip_ws(p)) {
      return 0;
    }

    if (try_skip_semicolon(p)) {
      out->tag = AST_STATEMENT_RETURN;
      ast_return_statement_init_no_expr(&out->u.return_statement,
                                        ast_meta_make(pos_start, ps_pos(p)));
      return 1;
    }

    struct ast_expr expr;
    if (!parse_expr(p, &expr, kSemicolonPrecedence)) {
      return 0;
    }

    if (!try_skip_semicolon(p)) {
      ast_expr_destroy(&expr);
      return 0;
    }

    out->tag = AST_STATEMENT_RETURN;
    ast_return_statement_init(&out->u.return_statement,
                              ast_meta_make(pos_start, ps_pos(p)),
                              expr);
    return 1;
  } else if (try_skip_keyword(p, "if")) {
    return parse_rest_of_if_statement(p, pos_start, out);
  } else if (try_skip_keyword(p, "while")) {
    out->tag = AST_STATEMENT_WHILE;
    return parse_rest_of_while_statement(p, pos_start, &out->u.while_statement);
  } else if (try_skip_keyword(p, "for")) {
    out->tag = AST_STATEMENT_FOR;
    return parse_rest_of_for_statement(p, pos_start, &out->u.for_statement);
  } else if (try_skip_keyword(p, "switch")) {
    out->tag = AST_STATEMENT_SWITCH;
    return parse_rest_of_switch_statement(p, pos_start, &out->u.switch_statement);
  } else {
    return parse_naked_var_or_expr_statement(p, 0, out);
  }
}

int parse_rest_of_bracebody(struct ps *p, struct pos pos_start, struct ast_bracebody *out) {
  struct ast_statement *statements = NULL;
  size_t statements_count = 0;
  size_t statements_limit = 0;
  for (;;) {
    if (!skip_ws(p)) {
      goto fail;
    }
    if (try_skip_char(p, '}')) {
      ast_bracebody_init(out, ast_meta_make(pos_start, ps_pos(p)),
                         statements, statements_count);
      return 1;
    }

    struct ast_statement statement;
    if (!parse_statement(p, &statement)) {
      goto fail;
    }

    SLICE_PUSH(statements, statements_count, statements_limit, statement);
  }

 fail:
  SLICE_FREE(statements, statements_count, ast_statement_destroy);
  return 0;
}

int parse_bracebody(struct ps *p, struct ast_bracebody *out) {
  struct pos pos_start = ps_pos(p);
  if (!try_skip_char(p, '{')) {
    return 0;
  }
  return parse_rest_of_bracebody(p, pos_start, out);
}

int parse_lambdaspec(struct ps *p,
                     struct ast_vardecl **params_out,
                     size_t *params_count_out,
                     struct ast_typeexpr *return_type_out,
                     struct ast_bracebody *bracebody_out) {
  struct ast_vardecl *params;
  size_t params_count;
  if (!(skip_ws(p) && parse_params_list(p, &params, &params_count))) {
    goto fail;
  }

  PARSE_DBG("parse_lambdaspec parsing return type\n");
  struct pos return_type_pos_end_discard;
  struct ast_typeexpr return_type;
  if (!(skip_ws(p) && parse_typeexpr(p, &return_type, &return_type_pos_end_discard))) {
    goto fail_params;
  }

  PARSE_DBG("parse_lambdaspec parsing body\n");
  struct ast_bracebody bracebody;
  if (!(skip_ws(p) && parse_bracebody(p, &bracebody))) {
    goto fail_return_type;
  }

  *params_out = params;
  *params_count_out = params_count;
  *return_type_out = return_type;
  *bracebody_out = bracebody;
  return 1;

 fail_return_type:
  ast_typeexpr_destroy(&return_type);
 fail_params:
  SLICE_FREE(params, params_count, ast_vardecl_destroy);
 fail:
  return 0;
}

int parse_rest_of_lambda(struct ps *p, struct pos pos_start,
                         struct ast_lambda *out) {
  PARSE_DBG("parse_rest_of_lambda\n");
  struct ast_vardecl *params;
  size_t params_count;
  struct ast_typeexpr return_type;
  struct ast_bracebody bracebody;
  if (!parse_lambdaspec(p, &params, &params_count, &return_type, &bracebody)) {
    return 0;
  }

  PARSE_DBG("parse_rest_of_lambda success\n");
  ast_lambda_init(out, ast_meta_make(pos_start, ps_pos(p)),
                  params, params_count, return_type, bracebody);
  return 1;
}

enum tri triparse_numeric_literal(struct ps *p, struct ast_numeric_literal *out) {
  struct pos pos_start = ps_pos(p);
  int32_t first_digit = ps_peek(p);
  int8_t first_digit_value;
  if (!is_decimal_digit(first_digit, &first_digit_value)) {
    goto quickfail;
  }
  ps_step(p);

  int8_t *digits = NULL;
  size_t digits_count = 0;
  size_t digits_limit = 0;

  enum ast_numeric_literal_tag tag;
  if (first_digit == '0') {
    if (ps_peek(p) == 'x') {
      /* We peek and step to avoid calling ps_leaf_count, because I
      think the tests make more sense if a whole hex literal counts as
      a leaf. */
      ps_step(p);
      tag = AST_NUMERIC_LITERAL_HEX;

      int32_t ch;
      int8_t ch_value;
      while ((ch = ps_peek(p)), is_hex_digit(ch, &ch_value)) {
        SLICE_PUSH(digits, digits_count, digits_limit, ch_value);
        ps_step(p);
      }
    } else {
      SLICE_PUSH(digits, digits_count, digits_limit, first_digit_value);
      tag = AST_NUMERIC_LITERAL_DEC;

      int8_t ch_value_discard;
      if (is_decimal_digit(ps_peek(p), &ch_value_discard)) {
        /* Octalesque constant. */
        goto fail;
      }
    }
  } else {
    SLICE_PUSH(digits, digits_count, digits_limit, first_digit_value);
    tag = AST_NUMERIC_LITERAL_DEC;

    int32_t ch;
    int8_t ch_value;
    while ((ch = ps_peek(p)), is_decimal_digit(ch, &ch_value)) {
      SLICE_PUSH(digits, digits_count, digits_limit, ch_value);
      ps_step(p);
    }
  }

  if (!is_numeric_postchar(ps_peek(p))) {
    goto fail;
  }

  ast_numeric_literal_init(out, ast_meta_make(pos_start, ps_pos(p)),
                           tag, digits, digits_count);
  ps_count_leaf(p);
  return TRI_SUCCESS;

 fail:
  free(digits);
  return TRI_ERROR;
 quickfail:
  return TRI_QUICKFAIL;
}

int parse_rest_of_arglist(struct ps *p,
                          struct ast_exprcall **args_out,
                          size_t *args_count_out) {
  struct ast_exprcall *args = NULL;
  size_t args_count = 0;
  size_t args_limit = 0;

  for (;;) {
    if (!skip_ws(p)) {
      goto fail;
    }
    if (try_skip_char(p, ')')) {
      *args_out = args;
      *args_count_out = args_count;
      return 1;
    }
    if (args_count != 0) {
      if (!try_skip_char(p, ',')) {
        goto fail;
      }
      if (!skip_ws(p)) {
        goto fail;
      }
    }

    struct ast_expr expr;
    if (!parse_expr(p, &expr, kCommaPrecedence)) {
      goto fail;
    }

    struct ast_exprcall exprcall;
    ast_exprcall_init(&exprcall, expr);

    SLICE_PUSH(args, args_count, args_limit, exprcall);
  }

 fail:
  SLICE_FREE(args, args_count, ast_exprcall_destroy);
  return 0;
}

void build_unop_expr(struct ast_meta meta,
                     struct ast_ident unop_name,
                     enum ast_unop unop,
                     struct ast_expr rhs,
                     struct ast_expr *out) {
  if (is_magic_unop(unop)) {
    ast_ident_destroy(&unop_name);
    ast_expr_partial_init(out, AST_EXPR_UNOP, ast_expr_info_default());
    ast_unop_expr_init(&out->u.unop_expr, meta, unop, rhs);
  } else {
    struct ast_expr func;
    ast_expr_partial_init(&func, AST_EXPR_NAME, ast_expr_info_default());
    ast_name_expr_init(&func.u.name, unop_name);

    struct ast_exprcall *args = malloc_mul(sizeof(*args), 1);
    ast_exprcall_init(&args[0], rhs);

    ast_expr_partial_init(out, AST_EXPR_FUNCALL, ast_expr_info_default());

    ast_funcall_init(&out->u.funcall, meta,
                     ast_exprcall_make(func), args, 1);
  }
}

void build_binop_expr(struct ast_meta meta,
                      struct ast_ident binop_name,
                      enum ast_binop binop,
                      struct ast_expr old_lhs,
                      struct ast_expr rhs,
                      struct ast_expr *out) {
  if (is_magic_binop(binop)) {
    ast_ident_destroy(&binop_name);
    ast_expr_partial_init(out, AST_EXPR_BINOP, ast_expr_info_default());
    ast_binop_expr_init(&out->u.binop_expr,
                        meta, binop, old_lhs, rhs);
  } else {
    struct ast_expr func;
    ast_expr_partial_init(&func, AST_EXPR_NAME, ast_expr_info_default());
    ast_name_expr_init(&func.u.name, binop_name);

    struct ast_exprcall *args = malloc_mul(sizeof(*args), 2);
    ast_exprcall_init(&args[0], old_lhs);
    ast_exprcall_init(&args[1], rhs);

    ast_expr_partial_init(out, AST_EXPR_FUNCALL, ast_expr_info_default());
    ast_funcall_init(&out->u.funcall, meta,
                     ast_exprcall_make(func), args, 2);
  }
}

int parse_string_char(struct ps *p, uint8_t *out) {
  int32_t ch = ps_peek(p);

  STATIC_CHECK(' ' == 32 && '~' == 126);
  if (ch < ' ' || ch > '~') {
    return 0;
  }

  ps_step(p);
  if (ch != '\\') {
    *out = (uint8_t)ch;
    ps_count_leaf(p);
    return 1;
  }

  int32_t dh = ps_peek(p);
  switch (dh) {
  case 'n':
    ps_step(p);
    *out = '\n';
    break;
  case 't':
    ps_step(p);
    *out = '\t';
    break;
  case '\'':
    ps_step(p);
    *out = '\'';
    break;
  case '\"':
    ps_step(p);
    *out = '\"';
    break;
  case '\\':
    ps_step(p);
    *out = '\\';
    break;
  case 'r':
    ps_step(p);
    *out = '\r';
    break;
  case '0':
    ps_step(p);
    *out = 0;
    break;
  case 'x': {
    ps_step(p);
    int8_t v1;
    if (!is_hex_digit(ps_peek(p), &v1)) {
      return 0;
    }
    ps_step(p);
    int8_t v2;
    if (!is_hex_digit(ps_peek(p), &v2)) {
      return 0;
    }
    ps_step(p);

    uint8_t combined = (16 * (uint8_t)v1) + (uint8_t)v2;
    *out = combined;
  } break;
  default:
    return 0;
  }
  ps_count_leaf(p);
  return 1;
}

int parse_rest_of_char_literal(struct ps *p, struct pos pos_start,
                               struct ast_char_literal *out) {
  if (ps_peek(p) == '\'') {
    return 0;
  }

  uint8_t value;
  if (!parse_string_char(p, &value)) {
    return 0;
  }

  if (!try_skip_char(p, '\'')) {
    return 0;
  }

  ast_char_literal_init(out, ast_meta_make(pos_start, ps_pos(p)), value);
  return 1;
}


int parse_rest_of_string_literal(struct ps *p, struct pos pos_start,
                                 struct ast_string_literal *out) {
  uint8_t *values = NULL;
  size_t values_count = 0;
  size_t values_limit = 0;

  struct pos pos_end;

  for (;;) {
    while (ps_peek(p) != '\"') {
      uint8_t value;
      if (!parse_string_char(p, &value)) {
        return 0;
      }
      SLICE_PUSH(values, values_count, values_limit, value);
    }

    ps_step(p);
    /* We count the terminating double-quote character (try_skip_char
    counts the opining double-quote character). */
    ps_count_leaf(p);

    pos_end = ps_pos(p);

    /* Prevents concatenated string literals from being _too_ close. */
    if (ps_peek(p) == '\"') {
      return 0;
    }

    if (!skip_ws(p)) {
      return 0;
    }

    /* You can concat string literals like in C. */
    if (!try_skip_char(p, '\"')) {
      break;
    }
  }

  ast_string_literal_init(out, ast_meta_make(pos_start, pos_end),
                          values, values_count);
  return 1;
}

int parse_rest_of_strinit(struct ps *p, struct pos pos_start,
                          struct ast_strinit *out) {
  /* TODO: This is a copy/paste of parse_rest_of_arglist, except it
  uses exprcall and ')'. */
  struct ast_expr *args = NULL;
  size_t args_count = 0;
  size_t args_limit = 0;

  for (;;) {
    if (!skip_ws(p)) {
      goto fail;
    }
    if (try_skip_char(p, '}')) {
      ast_strinit_init(out, ast_meta_make(pos_start, ps_pos(p)),
                       args, args_count);
      return 1;
    }
    if (args_count != 0) {
      if (!try_skip_char(p, ',')) {
        goto fail;
      }
      if (!skip_ws(p)) {
        goto fail;
      }
    }

    struct ast_expr expr;
    if (!parse_expr(p, &expr, kCommaPrecedence)) {
      goto fail;
    }

    SLICE_PUSH(args, args_count, args_limit, expr);
  }

 fail:
  SLICE_FREE(args, args_count, ast_expr_destroy);
  return 0;
}

int parse_rest_of_type_param_list(struct ps *p, enum allow_blanks allow_blanks, struct ast_typeexpr **params_out,
                                  size_t *params_count_out);

int parse_atomic_expr(struct ps *p, struct ast_expr *out) {
  struct pos pos_start = ps_pos(p);
  if (try_skip_keyword(p, "func")) {
    ast_expr_partial_init(out, AST_EXPR_LAMBDA, ast_expr_info_default());
    return parse_rest_of_lambda(p, pos_start, &out->u.lambda);
  }
  if (try_skip_char(p, '\'')) {
    ast_expr_partial_init(out, AST_EXPR_CHAR_LITERAL, ast_expr_info_default());
    return parse_rest_of_char_literal(p, pos_start, &out->u.char_literal);
  }
  if (try_skip_char(p, '\"')) {
    ast_expr_partial_init(out, AST_EXPR_STRING_LITERAL, ast_expr_info_default());
    return parse_rest_of_string_literal(p, pos_start, &out->u.string_literal);
  }

  int8_t digit_value_discard;
  if (is_decimal_digit(ps_peek(p), &digit_value_discard)) {
    ast_expr_partial_init(out, AST_EXPR_NUMERIC_LITERAL, ast_expr_info_default());
    return TRI_SUCCESS == triparse_numeric_literal(p, &out->u.numeric_literal);
  }
  if (try_skip_keyword(p, "true")) {
    ast_expr_partial_init(out, AST_EXPR_BOOL_LITERAL, ast_expr_info_default());
    ast_bool_literal_init(&out->u.bool_literal, ast_meta_make(pos_start, ps_pos(p)), 1);
    return 1;
  }
  if (try_skip_keyword(p, "false")) {
    ast_expr_partial_init(out, AST_EXPR_BOOL_LITERAL, ast_expr_info_default());
    ast_bool_literal_init(&out->u.bool_literal, ast_meta_make(pos_start, ps_pos(p)), 0);
    return 1;
  }
  if (try_skip_keyword(p, "void")) {
    ast_expr_partial_init(out, AST_EXPR_VOID_LITERAL, ast_expr_info_default());
    ast_void_literal_init(&out->u.void_literal,
                          ast_meta_make(pos_start, ps_pos(p)));
    return 1;
  }
  if (try_skip_keyword(p, "null")) {
    ast_expr_partial_init(out, AST_EXPR_NULL_LITERAL,
                          ast_expr_info_default());
    ast_void_literal_init(&out->u.void_literal,
                          ast_meta_make(pos_start, ps_pos(p)));
    return 1;
  }

  if (try_skip_char(p, '@')) {
    if (!try_skip_char(p, '[')) {
      return 0;
    }
    struct ast_typeexpr type;
    struct pos type_pos_end_discard;
    if (!(skip_ws(p) && help_parse_typeexpr(p, ALLOW_BLANKS_YES, &type,
                                            &type_pos_end_discard))) {
      return 0;
    }
    struct ast_expr expr;
    if (!(skip_ws(p) && try_skip_char(p, ']') && skip_ws(p)
          && parse_expr(p, &expr, kConversionRightPrecedence))) {
      ast_typeexpr_destroy(&type);
      return 0;
    }

    ast_expr_partial_init(out, AST_EXPR_TYPED, ast_expr_info_default());
    ast_typed_expr_init(&out->u.typed_expr,
                        ast_meta_make(pos_start, ast_expr_pos_end(&expr)),
                        type,
                        expr);
    return 1;
  }

  if (is_ident_firstchar(ps_peek(p))) {
    ast_expr_partial_init(out, AST_EXPR_NAME, ast_expr_info_default());
    struct ast_ident ident;
    if (!parse_ident(p, &ident)) {
      return 0;
    }

    skip_ws(p);
    if (!try_skip_char(p, '@')) {
      ast_name_expr_init(&out->u.name, ident);
      return 1;
    }

    struct ast_typeexpr *params;
    size_t params_count;
    if (!(try_skip_char(p, '[')
          && parse_rest_of_type_param_list(p, ALLOW_BLANKS_NO, &params, &params_count))) {
      ast_ident_destroy(&ident);
      return 0;
    }

    ast_name_expr_init_with_params(&out->u.name, ast_meta_make(pos_start, ps_pos(p)),
                                   ident, params, params_count);
    return 1;
  }

  if (try_skip_char(p, '(')) {
    struct ast_expr expr;
    if (!parse_expr(p, &expr, kSemicolonPrecedence)) {
      return 0;
    }
    if (!try_skip_char(p, ')')) {
      ast_expr_destroy(&expr);
      return 0;
    }
    *out = expr;
    return 1;
  }

  if (try_skip_char(p, '{')) {
    ast_expr_partial_init(out, AST_EXPR_STRINIT, ast_expr_info_default());
    return parse_rest_of_strinit(p, pos_start, &out->u.strinit);
  }

  struct ast_ident unop_name;
  enum ast_unop unop;
  if (try_parse_unop(p, &unop, &unop_name)) {
    struct ast_expr rhs;
    if (!(skip_ws(p) && parse_expr(p, &rhs, unop_right_precedence(unop)))) {
      ast_ident_destroy(&unop_name);
      return 0;
    }
    struct ast_meta meta = ast_meta_make(pos_start, ast_expr_pos_end(&rhs));
    build_unop_expr(meta, unop_name, unop, rhs, out);
    return 1;
  }

  return 0;
}

int parse_rest_of_index_param(struct ps *p, struct ast_expr *out) {
  struct ast_expr arg;
  if (!parse_expr(p, &arg, kCommaPrecedence)) {
    goto fail;
  }
  if (!try_skip_char(p, ']')) {
    goto fail_arg;
  }
  *out = arg;
  return 1;
 fail_arg:
  ast_expr_destroy(&arg);
 fail:
  return 0;
}

int parse_expr(struct ps *p, struct ast_expr *out, int precedence_context) {
  struct pos pos_start = ps_pos(p);
  struct ast_expr lhs;
  if (!parse_atomic_expr(p, &lhs)) {
    return 0;
  }
  PARSE_DBG("parse_expr parsed atomic expr\n");

  return parse_after_atomic(p, pos_start, lhs, precedence_context, out);
}

int parse_after_atomic(struct ps *p, struct pos pos_start, struct ast_expr lhs,
                       int precedence_context, struct ast_expr *out) {
  for (;;) {
    if (!skip_ws(p)) {
      goto fail;
    }
    PARSE_DBG("parse_expr looking for paren\n");
    struct pos pos_fieldname = ps_pos(p);
    if (try_skip_char(p, '(')) {
      PARSE_DBG("parse_expr saw paren\n");
      struct ast_exprcall *args;
      size_t args_count;
      if (!parse_rest_of_arglist(p, &args, &args_count)) {
        goto fail;
      }
      struct ast_expr old_lhs = lhs;
      ast_expr_partial_init(&lhs, AST_EXPR_FUNCALL, ast_expr_info_default());
      ast_funcall_init(&lhs.u.funcall,
                       ast_meta_make(pos_start, ps_pos(p)),
                       ast_exprcall_make(old_lhs),
                       args,
                       args_count);
    } else if (try_skip_char(p, '[')) {
      PARSE_DBG("parse_expr saw bracket\n");
      struct ast_expr arg;
      if (!parse_rest_of_index_param(p, &arg)) {
        goto fail;
      }
      struct ast_expr old_lhs = lhs;
      ast_expr_partial_init(&lhs, AST_EXPR_INDEX, ast_expr_info_default());
      ast_index_expr_init(&lhs.u.index_expr, ast_meta_make(pos_start, ps_pos(p)),
                          old_lhs, arg);
    } else if (try_skip_tilder(p, ".~")) {
      struct ast_fieldname fieldname;
      ast_fieldname_init_whole(&fieldname, ast_meta_make(pos_fieldname, ps_pos(p)));

      struct ast_expr old_lhs = lhs;
      ast_expr_partial_init(&lhs, AST_EXPR_LOCAL_FIELD_ACCESS,
                            ast_expr_info_default());
      ast_local_field_access_init(&lhs.u.local_field_access,
                                  ast_meta_make(pos_start, ps_pos(p)),
                                  old_lhs,
                                  fieldname);
    } else if (try_skip_oper(p, ".")) {
      struct ast_ident ident;
      if (!(skip_ws(p) && parse_ident(p, &ident))) {
        goto fail;
      }

      struct ast_fieldname fieldname;
      ast_fieldname_init(&fieldname, ast_meta_make(pos_fieldname, ps_pos(p)),
                         ident);

      struct ast_expr old_lhs = lhs;
      ast_expr_partial_init(&lhs, AST_EXPR_LOCAL_FIELD_ACCESS,
                            ast_expr_info_default());
      ast_local_field_access_init(&lhs.u.local_field_access,
                                  ast_meta_make(pos_start, ps_pos(p)),
                                  old_lhs,
                                  fieldname);
    } else if (try_skip_tilder(p, "->~")) {
      struct ast_fieldname fieldname;
      ast_fieldname_init_whole(&fieldname, ast_meta_make(pos_fieldname, ps_pos(p)));

      struct ast_expr old_lhs = lhs;
      ast_expr_partial_init(&lhs, AST_EXPR_DEREF_FIELD_ACCESS,
                            ast_expr_info_default());
      ast_deref_field_access_init(&lhs.u.deref_field_access,
                                  ast_meta_make(pos_start, ps_pos(p)),
                                  old_lhs,
                                  fieldname);
    } else if (try_skip_oper(p, "->")) {
      struct ast_ident ident;
      if (!(skip_ws(p) && parse_ident(p, &ident))) {
        goto fail;
      }

      struct ast_fieldname fieldname;
      ast_fieldname_init(&fieldname, ast_meta_make(pos_fieldname, ps_pos(p)),
                         ident);

      struct ast_expr old_lhs = lhs;
      ast_expr_partial_init(&lhs, AST_EXPR_DEREF_FIELD_ACCESS,
                            ast_expr_info_default());
      ast_deref_field_access_init(&lhs.u.deref_field_access,
                                  ast_meta_make(pos_start, ps_pos(p)),
                                  old_lhs,
                                  fieldname);
    } else if (is_binop_start(ps_peek(p))) {
      struct ps_savestate save = ps_save(p);

      enum ast_binop op;
      struct ast_ident op_name;
      if (!parse_binop(p, &op, &op_name)) {
        goto fail;
      }

      struct precedence_pair op_precedence = binop_precedence(op);

      enum precedence_comparison cmp
        = compare_precedence(precedence_context,
                             op_precedence.left_precedence);

      switch (cmp) {
      case PRECEDENCE_COMPARISON_CONFLICTS:
        ast_ident_destroy(&op_name);
        goto fail;
      case PRECEDENCE_COMPARISON_PULLS_LEFT:
        ps_restore(p, save);
        *out = lhs;
        return 1;
      case PRECEDENCE_COMPARISON_PULLS_RIGHT:
        break;
      default:
        UNREACHABLE();
      }

      struct ast_expr rhs;
      if (!(skip_ws(p)
            && parse_expr(p, &rhs, op_precedence.right_precedence))) {
        ast_ident_destroy(&op_name);
        goto fail;
      }

      struct ast_expr old_lhs = lhs;
      build_binop_expr(ast_meta_make(pos_start, ast_expr_pos_end(&rhs)),
                       op_name, op, old_lhs, rhs, &lhs);
    } else {
      PARSE_DBG("parse_expr done\n");
      *out = lhs;
      return 1;
    }
  }

 fail:
  ast_expr_destroy(&lhs);
  return 0;
}

int parse_braced_fields(struct ps *p,
                        enum allow_blanks allow_blanks,
                        struct ast_vardecl **fields_out,
                        size_t *fields_count_out) {
  if (!try_skip_char(p, '{')) {
    return 0;
  }
  struct ast_vardecl *fields = NULL;
  size_t fields_count = 0;
  size_t fields_limit = 0;
  for (;;) {
    if (!skip_ws(p)) {
      goto fail;
    }
    if (try_skip_char(p, '}')) {
      *fields_out = fields;
      *fields_count_out = fields_count;
      return 1;
    }

    struct ast_vardecl field;
    if (!help_parse_vardecl(p, allow_blanks, &field)) {
      goto fail;
    }

    if (!(skip_ws(p) && try_skip_semicolon(p))) {
      ast_vardecl_destroy(&field);
      goto fail;
    }

    SLICE_PUSH(fields, fields_count, fields_limit, field);
  }

 fail:
  SLICE_FREE(fields, fields_count, ast_vardecl_destroy);
  return 0;
}

int parse_rest_of_structe(struct ps *p, enum allow_blanks allow_blanks, struct pos pos_start,
                          struct ast_structe *out, struct pos *pos_end_out) {
  struct ast_vardecl *fields;
  size_t fields_count;
  if (!(skip_ws(p) && parse_braced_fields(p, allow_blanks, &fields, &fields_count))) {
    return 0;
  }
  struct pos pos_end = ps_pos(p);
  *pos_end_out = pos_end;
  ast_structe_init(out, ast_meta_make(pos_start, pos_end),
                   fields, fields_count);
  return 1;
}

int parse_rest_of_unione(struct ps *p,
                         enum allow_blanks allow_blanks,
                         struct pos pos_start,
                         struct ast_unione *out,
                         struct pos *pos_end_out) {
  struct ast_vardecl *fields;
  size_t fields_count;
  if (!(skip_ws(p) && parse_braced_fields(p, allow_blanks, &fields, &fields_count))) {
    return 0;
  }
  struct pos pos_end = ps_pos(p);
  *pos_end_out = pos_end;
  ast_unione_init(out, ast_meta_make(pos_start, pos_end),
                  fields, fields_count);
  return 1;
}

int continue_parsing_arraytype(struct ps *p, enum allow_blanks allow_blanks,
                               struct pos pos_start, struct ast_numeric_literal number,
                               struct ast_arraytype *out, struct pos *pos_end_out) {
  struct ast_typeexpr param;
  if (!(skip_ws(p) && try_skip_char(p, ']') && skip_ws(p) &&
        help_parse_typeexpr(p, allow_blanks, &param, pos_end_out))) {
    goto fail;
  }

  ast_arraytype_init(out, ast_meta_make(pos_start, ps_pos(p)),
                     number, param);
  return 1;
 fail:
  ast_numeric_literal_destroy(&number);
  return 0;
}

int parse_rest_of_arraytype(struct ps *p, enum allow_blanks allow_blanks,
                            struct pos pos_start,
                            struct ast_arraytype *out,
                            struct pos *pos_end_out) {
  if (!skip_ws(p)) {
    return 0;
  }
  struct ast_numeric_literal number;
  if (TRI_SUCCESS != triparse_numeric_literal(p, &number)) {
    return 0;
  }

  return continue_parsing_arraytype(p, allow_blanks, pos_start, number, out, pos_end_out);
}

int parse_rest_of_pointer(struct ps *p, struct ast_meta star_operator_meta,
                          enum allow_blanks allow_blanks,
                          struct ast_typeexpr *out,
                          struct pos *pos_end_out) {
  if (!skip_ws(p)) {
    goto fail;
  }

  struct ast_typeexpr param;
  struct pos param_pos_end;
  if (!help_parse_typeexpr(p, allow_blanks, &param, &param_pos_end)) {
    goto fail;
  }

  struct pos pos_start = star_operator_meta.pos_start;

  struct ast_ident star_name;
  ast_ident_init(&star_name, star_operator_meta, p->ptr_ident);

  struct ast_typeexpr *params = malloc_mul(sizeof(*params), 1);
  params[0] = param;

  out->tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&out->u.app, ast_meta_make(pos_start, param_pos_end),
                   star_name, params, 1);
  *pos_end_out = param_pos_end;
  return 1;

 fail:
  ast_meta_destroy(&star_operator_meta);
  return 0;
}

int parse_rest_of_type_param_list(struct ps *p,
                                  enum allow_blanks allow_blanks,
                                  struct ast_typeexpr **params_out,
                                  size_t *params_count_out) {
  struct ast_typeexpr *params = NULL;
  size_t params_count = 0;
  size_t params_limit = 0;

  for (;;) {
    if (!skip_ws(p)) {
      goto fail;
    }
    if (try_skip_char(p, ']')) {
      *params_out = params;
      *params_count_out = params_count;
      return 1;
    }

    if (params_count != 0) {
      if (!try_skip_char(p, ',')) {
        goto fail;
      }
      if (!skip_ws(p)) {
        goto fail;
      }
    }

    struct ast_typeexpr typeexpr;
    struct pos pos_end_discard;
    if (!help_parse_typeexpr(p, allow_blanks, &typeexpr, &pos_end_discard)) {
      goto fail;
    }
    SLICE_PUSH(params, params_count, params_limit, typeexpr);
  }

 fail:
  SLICE_FREE(params, params_count, ast_typeexpr_destroy);
  return 0;
}

int continue_ident_typeexpr(struct ps *p, enum allow_blanks allow_blanks,
                            struct ast_ident name, struct ast_typeexpr *out,
                            struct pos *pos_end_out) {
  struct pos after_ident = ps_pos(p);
  if (!skip_ws(p)) {
    goto fail_ident;
  }

  if (!try_skip_char(p, '[')) {
    out->tag = AST_TYPEEXPR_NAME;
    out->u.name = name;
    *pos_end_out = after_ident;
    return 1;
  }

  struct ast_typeexpr *params = NULL;
  size_t params_count = 0;
  if (!parse_rest_of_type_param_list(p, allow_blanks, &params, &params_count)) {
    goto fail_ident;
  }

  struct pos pos_end = ps_pos(p);
  *pos_end_out = pos_end;
  out->tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&out->u.app, ast_meta_make(name.meta.pos_start, pos_end),
                   name, params, params_count);
  return 1;
 fail_ident:
  ast_ident_destroy(&name);
  return 0;
}

enum tri help_triparse_typeexpr(struct ps *p, enum allow_blanks allow_blanks,
                                struct ast_typeexpr *out, struct pos *pos_end_out) {
  struct pos pos_start = ps_pos(p);
  if (try_skip_keyword(p, "struct")) {
    out->tag = AST_TYPEEXPR_STRUCTE;
    return success_or_fail(parse_rest_of_structe(p, allow_blanks, pos_start, &out->u.structe, pos_end_out));
  }

  if (try_skip_keyword(p, "union")) {
    out->tag = AST_TYPEEXPR_UNIONE;
    return success_or_fail(parse_rest_of_unione(p, allow_blanks, pos_start, &out->u.unione, pos_end_out));
  }

  if (try_skip_char(p, '[')) {
    out->tag = AST_TYPEEXPR_ARRAY;
    return success_or_fail(parse_rest_of_arraytype(p, allow_blanks, pos_start, &out->u.arraytype, pos_end_out));
  }

  if (try_skip_char(p, '*')) {
    return success_or_fail(parse_rest_of_pointer(p, ast_meta_make(pos_start, ps_pos(p)), allow_blanks, out, pos_end_out));
  }

  struct ps_savestate before_underscore = ps_save(p);
  if (try_skip_keyword(p, "_")) {
    if (allow_blanks) {
      out->tag = AST_TYPEEXPR_UNKNOWN;
      ast_unknown_init(&out->u.unknown, ast_meta_make(pos_start, ps_pos(p)));
      *pos_end_out = ps_pos(p);
      return TRI_SUCCESS;
    } else {
      ps_restore(p, before_underscore);
      return TRI_ERROR;
    }
  }

  struct ast_ident name;
  switch (triparse_ident(p, &name)) {
  case TRI_SUCCESS:
    return success_or_fail(continue_ident_typeexpr(p, allow_blanks, name, out, pos_end_out));
  case TRI_QUICKFAIL:
    return TRI_QUICKFAIL;
  case TRI_ERROR:
    return TRI_ERROR;
  default:
    UNREACHABLE();
  }
}

int help_parse_typeexpr(struct ps *p, enum allow_blanks allow_blanks, struct ast_typeexpr *out,
                        struct pos *pos_end_out) {
  return TRI_SUCCESS == help_triparse_typeexpr(p, allow_blanks, out, pos_end_out);
}

int parse_typeexpr(struct ps *p, struct ast_typeexpr *out, struct pos *pos_end_out) {
  return help_parse_typeexpr(p, ALLOW_BLANKS_NO, out, pos_end_out);
}

int parse_type_params_if_present(struct ps *p,
                                 struct ast_generics *out) {
  struct pos pos_start = ps_pos(p);
  if (!try_skip_char(p, '[')) {
    ast_generics_init_no_params(out);
    return 1;
  }

  struct ast_ident *params = NULL;
  size_t params_count = 0;
  size_t params_limit = 0;
  for (;;) {
    if (!skip_ws(p)) {
      goto fail;
    }
    if (try_skip_char(p, ']')) {
      ast_generics_init_has_params(out,
                                   ast_meta_make(pos_start, ps_pos(p)),
                                   params, params_count);
      return 1;
    }

    if (params_count != 0) {
      if (!try_skip_char(p, ',')) {
        goto fail;
      }
      if (!skip_ws(p)) {
        goto fail;
      }
    }
    struct ast_ident param;
    if (!parse_ident(p, &param)) {
      goto fail;
    }
    SLICE_PUSH(params, params_count, params_limit, param);
  }

 fail:
  SLICE_FREE(params, params_count, ast_ident_destroy);
  return 0;
}

int parse_def_generics_and_name(struct ps *p,
                                int is_export,
                                struct ast_generics *generics_out,
                                struct ast_ident *name_out) {
  struct ast_generics generics = { 0 };
  if (is_export) {
    ast_generics_init_no_params(&generics);
  } else {
    if (!(skip_ws(p) && parse_type_params_if_present(p, &generics))) {
      goto fail;
    }
  }

  struct ast_ident name;
  if (!(skip_ws(p) && parse_ident(p, &name))) {
    goto fail_generics;
  }

  *generics_out = generics;
  *name_out = name;
  return 1;

 fail_generics:
  ast_generics_destroy(&generics);
 fail:
  return 0;
}

int parse_rest_of_def(struct ps *p, struct pos pos_start,
                      int is_export, struct ast_def *out) {
  struct ast_generics generics;
  struct ast_ident name;
  if (!parse_def_generics_and_name(p, is_export, &generics, &name)) {
    goto fail;
  }

  if (!skip_ws(p)) {
    goto fail_ident;
  }

  int has_typeexpr;
  struct ast_typeexpr typeexpr = { 0 };
  if (try_skip_oper(p, "=")) {
    has_typeexpr = 0;
  } else {
    struct pos pos_end_discard;
    if (!parse_typeexpr(p, &typeexpr, &pos_end_discard)) {
      goto fail_ident;
    }
    has_typeexpr = 1;
    if (!(skip_ws(p) && skip_oper(p, "="))) {
      goto fail_typeexpr;
    }
  }

  struct ast_expr rhs;
  if (!(skip_ws(p) && parse_expr(p, &rhs, kSemicolonPrecedence))) {
    goto fail_typeexpr;
  }

  if (!try_skip_semicolon(p)) {
    goto fail_rhs;
  }

  struct ast_meta meta = ast_meta_make(pos_start, ps_pos(p));
  if (has_typeexpr) {
    ast_def_init(out, meta, is_export, generics, name, typeexpr, rhs);
  } else {
    ast_def_init_no_type(out, meta, is_export, generics, name, rhs);
  }
  return 1;

 fail_rhs:
  ast_expr_destroy(&rhs);
 fail_typeexpr:
  if (has_typeexpr) {
    ast_typeexpr_destroy(&typeexpr);
  }
 fail_ident:
  ast_ident_destroy(&name);
  ast_generics_destroy(&generics);
 fail:
  return 0;
}

int parse_rest_of_func(struct ps *p, struct pos pos_start,
                       int is_export, struct ast_def *out) {
  struct ast_generics generics;
  struct ast_ident name;
  if (!parse_def_generics_and_name(p, is_export, &generics, &name)) {
    goto fail;
  }

  struct pos lambda_start = ps_pos(p);

  struct ast_vardecl *params;
  size_t params_count;
  struct ast_typeexpr return_type;
  struct ast_bracebody bracebody;
  if (!parse_lambdaspec(p, &params, &params_count, &return_type, &bracebody)) {
    goto fail_name;
  }

  struct pos pos_end = ps_pos(p);

  struct ast_expr rhs;
  ast_expr_partial_init(&rhs, AST_EXPR_LAMBDA, ast_expr_info_default());
  ast_lambda_init(&rhs.u.lambda, ast_meta_make(lambda_start, pos_end),
                  params, params_count, return_type, bracebody);

  struct ast_meta meta = ast_meta_make(pos_start, ps_pos(p));
  ast_def_init_no_type(out, meta, is_export, generics, name, rhs);
  return 1;

 fail_name:
  ast_ident_destroy(&name);
  ast_generics_destroy(&generics);
 fail:
  return 0;
}

int parse_rest_of_extern_def(struct ps *p, struct pos pos_start,
                             struct ast_extern_def *out) {
  struct ast_ident name;
  if (!(skip_ws(p) && parse_ident(p, &name))) {
    goto fail;
  }

  struct ast_typeexpr type;
  struct pos type_pos_end_discard;
  if (!(skip_ws(p) && parse_typeexpr(p, &type, &type_pos_end_discard))) {
    goto fail_ident;
  }

  if (!(skip_ws(p) && try_skip_semicolon(p))) {
    goto fail_typeexpr;
  }

  ast_extern_def_init(out, ast_meta_make(pos_start, ps_pos(p)),
                      name, type);
  return 1;

 fail_typeexpr:
  ast_typeexpr_destroy(&type);
 fail_ident:
  ast_ident_destroy(&name);
 fail:
  return 0;
}


int parse_rest_of_import(struct ps *p, struct pos pos_start,
                         struct ast_import *out) {
  PARSE_DBG("parse_rest_of_import\n");
  struct ast_ident name;
  if (!(skip_ws(p) && parse_ident(p, &name))) {
    goto fail;
  }
  PARSE_DBG("parse_rest_of_import about to skip semicolon\n");
  if (!(skip_ws(p) && try_skip_semicolon(p))) {
    goto fail_ident;
  }
  ast_import_init(out, ast_meta_make(pos_start, ps_pos(p)), name);
  return 1;

 fail_ident:
  ast_ident_destroy(&name);
 fail:
  return 0;
}

int parse_toplevel(struct ps *p, struct ast_toplevel *out);

int parse_rest_of_deftype(struct ps *p, struct pos pos_start,
                          int is_class,
                          struct ast_deftype *out) {
  PARSE_DBG("parse_rest_of_deftype");
  struct ast_generics generics;
  if (!(skip_ws(p) && parse_type_params_if_present(p, &generics))) {
    goto fail;
  }
  if (!skip_ws(p)) {
    goto fail_generics;
  }
  enum ast_deftype_disposition disposition;
  if (!is_class) {
    disposition = AST_DEFTYPE_NOT_CLASS;
  } else if (try_skip_keyword(p, "copy")) {
    disposition = AST_DEFTYPE_CLASS_DEFAULT_COPY_MOVE_DESTROY;
  } else if (try_skip_keyword(p, "move")) {
    disposition = AST_DEFTYPE_CLASS_DEFAULT_MOVE;
  } else {
    disposition = AST_DEFTYPE_CLASS_NO_DEFAULTS;
  }
  struct ast_ident name;
  if (!(skip_ws(p) && parse_ident(p, &name))) {
    goto fail_generics;
  }
  struct ast_typeexpr type;
  struct pos type_pos_end_discard;
  if (!(skip_ws(p) && parse_typeexpr(p, &type, &type_pos_end_discard))) {
    goto fail_ident;
  }
  if (!(skip_ws(p) && try_skip_semicolon(p))) {
    goto fail_typeexpr;
  }
  ast_deftype_init(out, ast_meta_make(pos_start, ps_pos(p)),
                   disposition, generics, name, type);
  return 1;

 fail_typeexpr:
  ast_typeexpr_destroy(&type);
 fail_ident:
  ast_ident_destroy(&name);
 fail_generics:
  ast_generics_destroy(&generics);
 fail:
  return 0;
}

int parse_rest_of_defenum(struct ps *p, struct pos pos_start,
                          struct ast_deftype *out) {
  PARSE_DBG("parse_rest_of_defenum");
  struct ast_generics generics;
  if (!(skip_ws(p) && parse_type_params_if_present(p, &generics))) {
    goto fail;
  }
  struct ast_ident name;
  if (!(skip_ws(p) && parse_ident(p, &name))) {
    goto fail_generics;
  }
  struct ast_vardecl *enumfields;
  size_t enumfields_count;
  if (!(skip_ws(p) && parse_braced_fields(p, ALLOW_BLANKS_NO, &enumfields, &enumfields_count))) {
    goto fail_name;
  }
  if (!(skip_ws(p) && try_skip_semicolon(p))) {
    goto fail_enumfields;
  }
  ast_deftype_init_enum(out, ast_meta_make(pos_start, ps_pos(p)),
                        generics, name, enumfields, enumfields_count);
  return 1;

 fail_enumfields:
  SLICE_FREE(enumfields, enumfields_count, ast_vardecl_destroy);
 fail_name:
  ast_ident_destroy(&name);
 fail_generics:
  ast_generics_destroy(&generics);
 fail:
  return 0;
}

int parse_toplevels(struct ps *p, int32_t until_ch,
                    struct ast_toplevel **toplevels_out,
                    size_t *toplevels_count_out) {
  struct ast_toplevel *toplevels = NULL;
  size_t toplevels_count = 0;
  size_t toplevels_limit = 0;
  for (;;) {
    if (!skip_ws(p)) {
      goto fail;
    }
    if (ps_peek(p) == until_ch) {
      *toplevels_out = toplevels;
      *toplevels_count_out = toplevels_count;
      return 1;
    }

    struct ast_toplevel toplevel;
    if (!parse_toplevel(p, &toplevel)) {
      goto fail;
    }
    SLICE_PUSH(toplevels, toplevels_count, toplevels_limit, toplevel);
  }
 fail:
  SLICE_FREE(toplevels, toplevels_count, ast_toplevel_destroy);
  return 0;
}

int parse_rest_of_access(struct ps *p, struct pos pos_start, struct ast_access *out) {
  /* This is weird: We require exactly one space between the keyword
  "access" and the type being accessed.  This way, you can grep
  "access typename" */
  /* TODO: This is stupid, and s2 doesn't behave this way. */
  if (ps_peek(p) != ' ') {
    goto fail;
  }
  ps_step(p);

  struct ast_ident name;
  if (!parse_ident(p, &name)) {
    goto fail;
  }

  if (!skip_ws(p)) {
    goto fail_name;
  }

  struct generics_arity arity;
  if (!try_skip_char(p, '[')) {
    arity = no_param_list_arity();
  } else {
    uint32_t counter = 0;
    for (;;) {
      if (!skip_ws(p)) {
        goto fail_name;
      }
      if (try_skip_char(p, ']')) {
        break;
      }
      if (counter != 0) {
        if (!(try_skip_char(p, ',') && skip_ws(p))) {
          goto fail_name;
        }
      }
      if (!try_skip_char(p, '_')) {
        goto fail_name;
      }
      counter = uint32_add(counter, 1);
    }
    arity = param_list_arity(counter);
    if (!skip_ws(p)) {
      goto fail_name;
    }
  }

  struct ast_toplevel *toplevels;
  size_t toplevels_count;
  if (!(try_skip_char(p, '{')
        && parse_toplevels(p, '}', &toplevels, &toplevels_count))) {
    goto fail_name;
  }

  if (!try_skip_char(p, '}')) {
    goto fail_toplevels;
  }

  ast_access_init(out, ast_meta_make(pos_start, ps_pos(p)),
                  name, arity, toplevels, toplevels_count);
  return 1;

 fail_toplevels:
  SLICE_FREE(toplevels, toplevels_count, ast_toplevel_destroy);
 fail_name:
  ast_ident_destroy(&name);
 fail:
  return 0;
}

int parse_toplevel(struct ps *p, struct ast_toplevel *out) {
  PARSE_DBG("parse_toplevel\n");
  struct pos pos_start = ps_pos(p);
  if (try_skip_keyword(p, "func")) {
    out->tag = AST_TOPLEVEL_DEF;
    return parse_rest_of_func(p, pos_start, 0, &out->u.def);
  } else if (try_skip_keyword(p, "def")) {
    out->tag = AST_TOPLEVEL_DEF;
    return parse_rest_of_def(p, pos_start, 0, &out->u.def);
  } else if (try_skip_keyword(p, "inline")) {
    if (!(skip_ws(p) && try_skip_keyword(p, "func"))) {
      return 0;
    }
    out->tag = AST_TOPLEVEL_DEF;
    return parse_rest_of_func(p, pos_start, 0, &out->u.def);
  } else if (try_skip_keyword(p, "export")) {
    if (!skip_ws(p)) {
      return 0;
    }
    if (try_skip_keyword(p, "func")) {
      out->tag = AST_TOPLEVEL_DEF;
      return parse_rest_of_func(p, pos_start, 1, &out->u.def);
    }
    if (!try_skip_keyword(p, "def")) {
      return 0;
    }
    out->tag = AST_TOPLEVEL_DEF;
    return parse_rest_of_def(p, pos_start, 1, &out->u.def);
  } else if (try_skip_keyword(p, "extern")) {
    out->tag = AST_TOPLEVEL_EXTERN_DEF;
    return parse_rest_of_extern_def(p, pos_start, &out->u.extern_def);
  } else if (try_skip_keyword(p, "import")) {
    out->tag = AST_TOPLEVEL_IMPORT;
    return parse_rest_of_import(p, pos_start, &out->u.import);
  } else if (try_skip_keyword(p, "deftype")) {
    out->tag = AST_TOPLEVEL_DEFTYPE;
    return parse_rest_of_deftype(p, pos_start, 0, &out->u.deftype);
  } else if (try_skip_keyword(p, "defclass")) {
    out->tag = AST_TOPLEVEL_DEFTYPE;
    return parse_rest_of_deftype(p, pos_start, 1, &out->u.deftype);
  } else if (try_skip_keyword(p, "defenum")) {
    out->tag = AST_TOPLEVEL_DEFTYPE;
    return parse_rest_of_defenum(p, pos_start, &out->u.deftype);
  } else if (try_skip_keyword(p, "access")) {
    out->tag = AST_TOPLEVEL_ACCESS;
    return parse_rest_of_access(p, pos_start, &out->u.access);
  } else {
    return 0;
  }
}

int parse_file(struct ps *p, struct ast_file *out) {
  struct ast_toplevel *toplevels = NULL;
  size_t toplevels_count = 0;
  if (!parse_toplevels(p, -1, &toplevels, &toplevels_count)) {
    return 0;
  }
  ast_file_init(out, toplevels, toplevels_count);
  return 1;
}

int parse_buf_file(struct identmap *im,
                   const uint8_t *buf, size_t length,
                   size_t global_offset,
                   struct ast_file *file_out,
                   struct error_dump *error_dump) {
  struct ps p;
  ps_init_with_identmap(&p, im, error_dump, buf, length, global_offset);

  struct ast_file a;
  int ret = parse_file(&p, &a);
  if (ret) {
    *file_out = a;
  } else {
    const char *msg = "Parse abandoned.";
    (*error_dump->dumper)(error_dump, &p.im, p.line, p.column, msg, strlen(msg));
  }
  ps_remove_identmap(&p, im);
  ps_destroy(&p);
  return ret;
}

void silent_error(struct error_dump *ctx, struct identmap *im,
                  size_t line, size_t column, const char *msg, size_t msglen) {
  (void)ctx, (void)im, (void)line, (void)column, (void)msg, (void)msglen;
}

int count_parse_buf(const uint8_t *buf, size_t length,
                    size_t *leafcount_out, struct pos *error_pos_out) {
  struct error_dump dump;
  dump.dumper = &silent_error;

  struct ps p;
  ps_init(&p, &dump, buf, length, 0);

  struct ast_file file;
  PARSE_DBG("parse_file...\n");
  int ret = parse_file(&p, &file);
  if (ret) {
    *leafcount_out = p.leafcount;
    ast_file_destroy(&file);
  } else {
    *error_pos_out = ps_pos(&p);
  }
  ps_destroy(&p);
  return ret;
}

int count_parse(const char *str,
                size_t *leafcount_out, struct pos *error_pos_out) {
  size_t length = strlen(str);
  const uint8_t *data = (const uint8_t *)str;
  return count_parse_buf(data, length, leafcount_out, error_pos_out);
}

int run_count_test(const char *name, const char *str, size_t expected) {
  DBG("run_count_test %s...\n", name);
  size_t count;
  struct pos pos;
  int res = count_parse(str, &count, &pos);
  if (!res) {
    DBG("run_count_test %s FAIL: parse failed at offset %"PRIz"\n",
        name, pos.global_offset);
    return 0;
  }
  if (count != expected) {
    DBG("run_count_test %s FAIL: wrong count: expected %"PRIz", got %"PRIz"\n",
        str, expected, count);
    return 0;
  }
  return 1;
}

int parse_test_nothing(void) {
  return run_count_test("nothing", "", 0);
}

int parse_test_whitespace(void) {
  return run_count_test("whitespace", "  \n\t  ", 0);
}

int parse_test_imports(void) {
  return run_count_test("imports",
                        "import a; import aa; import bcd; import blah_quux;",
                        12);
}

int parse_test_defs(void) {
  int pass = 1;
  pass &= run_count_test("def01", "def a int = 0;", 6);
  pass &= run_count_test("def02", "def b int = 1;", 6);
  pass &= run_count_test("def03", "def a int =0   ;  ", 6);
  pass &= run_count_test("def04", "def abc_def int = 12345;", 6);
  pass &= run_count_test("def05", "def foo fn[int, int] = 1;", 11);
  pass &= run_count_test("def06",
                         "def foo fn[int, int] = "
                         "func(x int, y int) int { 3; };",
                         23);
  pass &= run_count_test("def07", "def foo fn[int, int] = \n"
                         "\tfunc(x int, y int) int { foo(bar); };\n",
                         26);
  pass &= run_count_test("def10",
                         "def foo bar = 2 + 3;",
                         8);
  pass &= run_count_test("def11",
                         "def foo bar = 2 + *3 - 4;",
                         11);
  pass &= run_count_test("def12",
                         "def foo bar = (2 ^ 3) - 4 && x -> quux;",
                         16);
  pass &= run_count_test("def13",
                         "def[] foo fn[int] = func() int {\n"
                         "   var x int = 3;\n"
                         "   return x;\n"
                         "};\n",
                         25);
  pass &= run_count_test("def14",
                         "def[a,b] foo/*heh*/fn[int] = func() int {"
                         "//blah blah blah\n"
                         "   var x int = -3;\n"
                         "   return x;\n"
                         "};\n",
                         29);
  pass &= run_count_test("def15",
                         "def foo i32 = 1 == 1 || 2 == 1;\n",
                         12);
  pass &= run_count_test("def16",
                         "def foo i32 = 1 + - 1;\n",
                         9);
  pass &= run_count_test("def17",
                         "def foo fn[int] = func() int {\n"
                         "  var x int;\n"
                         "};\n",
                         18);
  pass &= run_count_test("def18",
                         "def foo bar = func() void {\n"
                         "  (x+3)[y(z)] = 3;\n"
                         "  return x[y];\n"
                         "};\n",
                         31);
  pass &= run_count_test("def19",
                         "def foo bar = func() void {\n"
                         "  for var i i32 = 3; i < 3; i = i + 1 {\n"
                         "  }\n"
                         "};\n",
                         29);
  pass &= run_count_test("def20",
                         "def foo bar = func() void {\n"
                         "  for var i i32 = 3; i < 3; i = i + 1 {\n"
                         "    x = 2;\n"
                         "  }\n"
                         "};\n",
                         33);
  pass &= run_count_test("def20",
                         "def foo bar = baz@[a, b](1, 2, 3);\n",
                         19);
  pass &= run_count_test("def21",
                         "def foo u8 = 'a';\n",
                         8);
  pass &= run_count_test("def22",
                         "def foo u8 = '\\n';\n",
                         8);
  pass &= run_count_test("def23",
                         "def foo u8 = '\\x2A';\n",
                         8);
  pass &= run_count_test("def24-a",
                         "def foo [11]u8 = \"\\x2Abcdef\";\n",
                         16);
  pass &= run_count_test("def24-b",
                         "def foo [21]u8 = \"\\x2Abcdef\"\n"
                         "  \"abcdefghi\\\"\";\n",
                         28);
  pass &= run_count_test("def25-a",
                         "def a b = func() c {\n"
                         "  switch d {\n"
                         "    case e(f g): { }\n"
                         "    case h(i j): { k; }\n"
                         "  }\n"
                         "};\n",
                         35);
  pass &= run_count_test("def25-b",
                         "def a b = func() c {\n"
                         "  switch d {\n"
                         "    case e(f g): { }\n"
                         "    case h(i j): k;\n"
                         "  }\n"
                         "};\n",
                         33);
  pass &= run_count_test("def26",
                         "func a() c {\n"
                         "  switch d {\n"
                         "    case e(f g): { }\n"
                         "    case h(i j): { k; }\n"
                         "  }\n"
                         "}\n"
                         "func b() c {\n"
                         "  switch d {\n"
                         "    case e(f g): { }\n"
                         "    case h(i j): { k; }\n"
                         "  }\n"
                         "}\n",
                         62);
  pass &= run_count_test("def26",
                         "func a() c {\n"
                         "  switch d {\n"
                         "    case e(f g):\n"
                         "    case h(i j): { k; }\n"
                         "  }\n"
                         "}\n"
                         "func b() c {\n"
                         "  switch d {\n"
                         "    case e(f g): { }\n"
                         "    case h(i j): k; l + j;\n"
                         "  }\n"
                         "}\n",
                         62);
  pass &= run_count_test("def27",
                         "func `~`(x u32) size { }\n",
                         11);
  pass &= run_count_test("def28",
                         "def foo fn[int] = func() int {\n"
                         "  x int;\n"
                         "};\n",
                         17);
  pass &= run_count_test("def29",
                         "def foo fn[int] = func() int {\n"
                         "  x int = 3;\n"
                         "};\n",
                         19);
  pass &= run_count_test("def30",
                         "def foo bar = func() void {\n"
                         "  for i i32 = 3; i < 3; i = i + 1 {\n"
                         "  }\n"
                         "};\n",
                         28);
  pass &= run_count_test("def31",
                         "def foo bar = func() void {\n"
                         "  for i i32 = 0x3a; i < 3; i = i + 1 {\n"
                         "  }\n"
                         "};\n",
                         28);
  pass &= run_count_test("def32",
                         "func foo() void {\n"
                         "  if x { } else if y { } else { }\n"
                         "}\n",
                         19);
  pass &= run_count_test("def33",
                         "func foo() void {\n"
                         "  x [3]i32;\n"
                         "  x [3][4]i32;\n"
                         "  x[1][y] = z;\n"
                         "  x struct { };\n"
                         "}\n",
                         37);
  pass &= run_count_test("def34",
                         "func foo() void {\n"
                         "  return x->~.~->~;\n"
                         "}\n",
                         13);
  pass &= run_count_test("def35",
                         "func foo() void {\n"
                         "  if case Foo(x) = y {\n"
                         "  }\n"
                         "}\n",
                         17);
  pass &= run_count_test("def36",
                         "func foo() void {\n"
                         "  if case Foo(x) = y {\n"
                         "  } else { }\n"
                         "}\n",
                         20);
  return pass;
}

int parse_test_deftypes(void) {
  int pass = 1;
  pass &= run_count_test("deftype1",
                         " deftype foo bar;",
                         4);
  pass &= run_count_test("deftype2",
                         "deftype foo fn[int, int] ; ",
                         9);
  pass &= run_count_test("deftype3",
                         "deftype foo struct { x y; z int; t fn[beh]; };\n"
                         "deftype [ c, d ]  bar union{a b;c d[e,f];};",
                         40);
  pass &= run_count_test("deftype4",
                         "deftype[] foo bar;\n",
                         6);
  pass &= run_count_test("deftype5",
                         "deftype foo struct { x bar [quux]; };\n",
                         12);
  pass &= run_count_test("deftype6",
                         "def x int = 3;"
                         "deftype[T] foo struct { count u32; p ptr[T]; };\n",
                         24);
  pass &= run_count_test("deftype7",
                         "deftype foo [7]bar;\n"
                         "deftype[T] foo struct { count u32; p [3]T; };\n",
                         25);
  pass &= run_count_test("deftype8",
                         "defclass move foo [7]bar;\n"
                         "deftype[T] foo struct { count u32; p [3]T; };\n",
                         26);
  pass &= run_count_test("deftype9",
                         "defclass move foo [7]bar;\n"
                         "defclass[T] copy foo struct { count u32; p [3]T; };\n",
                         27);
  return pass;
}

int parse_test_externs(void) {
  int pass = 1;
  pass &= run_count_test("externs1",
                         "extern putchar fn[i32, i32];\n"
                         "def blah fn[i32, i32] = 3;\n",
                         20);
  return pass;
}

int parse_test_exports(void) {
  int pass = 1;
  pass &= run_count_test("externs1",
                         "export def blah fn[i32, i32] = 3;\n",
                         12);
  return pass;
}

int parse_test_access(void) {
  int pass = 1;
  pass &= run_count_test("access1",
                         "def x i32 = 4;\n"
                         "access string { def foo i32 = 3; }\n",
                         16);
  pass &= run_count_test("access1",
                         "def x i32 = 4;\n"
                         "access vec[_] { def foo i32 = 3; }\n",
                         19);
  return pass;
}

int parse_test(void) {
  int pass = 1;
  pass &= parse_test_nothing();
  pass &= parse_test_whitespace();
  pass &= parse_test_imports();
  pass &= parse_test_defs();
  pass &= parse_test_deftypes();
  pass &= parse_test_externs();
  pass &= parse_test_exports();
  pass &= parse_test_access();
  return pass;
}

