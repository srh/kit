#include "parse.h"

#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "identmap.h"
#include "slice.h"
#include "util.h"

#define PARSE_DBG(...)

struct ps {
  const uint8_t *data;
  size_t length;
  size_t pos;

  size_t line;
  size_t column;

  struct ident_map ident_table;
  size_t leafcount;
};

struct ps_savestate {
  size_t pos;
  size_t line;
  size_t column;
  size_t leafcount;
};

void ps_init(struct ps *p, const uint8_t *data, size_t length) {
  p->data = data;
  p->length = length;
  p->pos = 0;

  p->line = 1;
  p->column = 0;

  ident_map_init(&p->ident_table);
  p->leafcount = 0;
}

void ps_destroy(struct ps *p) {
  ident_map_destroy(&p->ident_table);
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

size_t ps_pos(struct ps *p) {
  return p->pos;
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

ident_value ps_intern_ident(struct ps *p,
			    struct ps_savestate ident_begin,
			    struct ps_savestate ident_end) {
  CHECK(ident_end.pos <= p->length);
  CHECK(ident_begin.pos <= ident_end.pos);
  return ident_map_intern(&p->ident_table,
			  p->data + ident_begin.pos,
			  ident_end.pos - ident_begin.pos);
}

void malloc_move_ast_expr(struct ast_expr movee, struct ast_expr **out) {
  struct ast_expr *p = malloc(sizeof(*p));
  CHECK(p);
  *p = movee;
  *out = p;
}

struct precedence_pair {
  int left_precedence;
  int right_precedence;
};

const int kSemicolonPrecedence = 205;
/* There is no comma operator, so no intermingling of commas and semicolons. */
const int kCommaPrecedence = 205;

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

const int unop_right_precedences[] = {
  [AST_UNOP_DEREFERENCE] = { 905 },
};

const int unop_right_precedence(enum ast_unop op) {
  CHECK(0 <= op &&
	op < sizeof(unop_right_precedences) / sizeof(unop_right_precedences[0]));
  return unop_right_precedences[op];
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
  STATIC_ASSERT(' ' == 32 && '\r' == 13 && '\n' == 10 && '\t' == 9);
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
  return is_one_of("~!%^&*+=-/?<,>.;:", ch);
}

int is_binop_start(int32_t ch) {
  return is_one_of("!%^&*+=-/<>", ch);
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

int is_numeric_postchar(int32_t ch) {
  return is_ident_postchar(ch);
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
  if (is_operlike(ps_peek(p))) {
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

int try_parse_unop(struct ps *p, enum ast_unop *out) {
  int32_t ch1 = ps_peek(p);
  if (ch1 == '*') {
    struct ps_savestate save = ps_save(p);
    ps_step(p);
    if (is_operlike(ps_peek(p))) {
      ps_restore(p, save);
      return 0;
    }
    *out = AST_UNOP_DEREFERENCE;
    ps_count_leaf(p);
    return 1;
  } else {
    return 0;
  }
}

int parse_binop(struct ps *p, enum ast_binop *out) {
  int32_t ch1 = ps_peek(p);
  if (!is_binop_start(ch1)) {
    return 0;
  }

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

int parse_ident(struct ps *p, struct ast_ident *out) {
  PARSE_DBG("parse_ident\n");
  size_t pos_start = ps_pos(p);
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

  ast_ident_init(out, ast_meta_make(pos_start, ps_pos(p)),
		 ps_intern_ident(p, save, ps_save(p)));
  ps_count_leaf(p);
  return 1;
}

int parse_typeexpr(struct ps *p, struct ast_typeexpr *out);

int parse_vardecl(struct ps *p, struct ast_vardecl *out) {
  size_t pos_start = ps_pos(p);
  struct ast_ident name;
  if (!parse_ident(p, &name)) {
    return 0;
  }
  skip_ws(p);
  struct ast_typeexpr type;
  if (!parse_typeexpr(p, &type)) {
    ast_ident_destroy(&name);
    return 0;
  }

  ast_vardecl_init(out, ast_meta_make(pos_start, ps_pos(p)),
		   name, type);
  return 1;
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
    skip_ws(p);
    if (try_skip_char(p, ')')) {
      *params_out = params;
      *params_count_out = params_count;
      return 1;
    }

    if (params_count != 0) {
      if (!try_skip_char(p, ',')) {
	goto fail;
      }
      skip_ws(p);
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

int parse_rest_of_if_statement(struct ps *p, size_t pos_start,
			       struct ast_statement *out) {
  skip_ws(p);
  struct ast_expr condition;
  if (!parse_expr(p, &condition, kSemicolonPrecedence)) {
    return 0;
  }

  struct ast_bracebody thenbody;
  if (!parse_bracebody(p, &thenbody)) {
    goto fail_condition;
  }

  size_t pos_thenbody_end = ps_pos(p);

  skip_ws(p);
  if (!try_skip_keyword(p, "else")) {
    struct ast_expr *heap_condition;
    malloc_move_ast_expr(condition, &heap_condition);
    out->tag = AST_STATEMENT_IFTHEN;
    ast_ifthen_statement_init(&out->u.ifthen_statement,
			      ast_meta_make(pos_start, pos_thenbody_end),
			      heap_condition,
			      thenbody);
    return 1;
  }

  skip_ws(p);
  struct ast_bracebody elsebody;
  if (!parse_bracebody(p, &elsebody)) {
    goto fail_thenbody;
  }

  struct ast_expr *heap_condition;
  malloc_move_ast_expr(condition, &heap_condition);
  out->tag = AST_STATEMENT_IFTHENELSE;
  ast_ifthenelse_statement_init(&out->u.ifthenelse_statement,
				ast_meta_make(pos_start, ps_pos(p)),
				heap_condition,
				thenbody,
				elsebody);
  return 1;

 fail_thenbody:
  ast_bracebody_destroy(&thenbody);
 fail_condition:
  ast_expr_destroy(&condition);
  return 0;
}

int parse_rest_of_var_statement(struct ps *p, size_t pos_start,
				struct ast_var_statement *out) {
    skip_ws(p);
    struct ast_ident name;
    if (!parse_ident(p, &name)) {
      return 0;
    }

    skip_ws(p);
    struct ast_typeexpr type;
    if (!parse_typeexpr(p, &type)) {
      goto fail_ident;
    }

    skip_ws(p);
    if (!skip_oper(p, "=")) {
      goto fail_typeexpr;
    }

    skip_ws(p);
    struct ast_expr rhs;
    if (!parse_expr(p, &rhs, kSemicolonPrecedence)) {
      goto fail_typeexpr;
    }

    if (!try_skip_semicolon(p)) {
      goto fail_rhs;
    }

    struct ast_expr *heap_rhs;
    malloc_move_ast_expr(rhs, &heap_rhs);
    ast_var_statement_init(out, ast_meta_make(pos_start, ps_pos(p)),
			   name, type, heap_rhs);
    return 1;

 fail_rhs:
    ast_expr_destroy(&rhs);
 fail_typeexpr:
    ast_typeexpr_destroy(&type);
 fail_ident:
    ast_ident_destroy(&name);
    return 0;
}

int parse_statement(struct ps *p, struct ast_statement *out) {
  size_t pos_start = ps_pos(p);
  if (try_skip_keyword(p, "var")) {
    out->tag = AST_STATEMENT_VAR;
    return parse_rest_of_var_statement(p, pos_start, &out->u.var_statement);
  } else if (try_skip_keyword(p, "goto")) {
    skip_ws(p);
    struct ast_ident target;
    if (!parse_ident(p, &target)) {
      return 0;
    }
    skip_ws(p);
    if (!try_skip_semicolon(p)) {
      ast_ident_destroy(&target);
      return 0;
    }
    out->tag = AST_STATEMENT_GOTO;
    ast_goto_statement_init(&out->u.goto_statement,
			    ast_meta_make(pos_start, ps_pos(p)),
			    target);
    return 1;
  } else if (try_skip_keyword(p, "label")) {
    skip_ws(p);
    struct ast_ident label;
    if (!parse_ident(p, &label)) {
      return 0;
    }
    skip_ws(p);
    if (!try_skip_semicolon(p)) {
      ast_ident_destroy(&label);
      return 0;
    }
    out->tag = AST_STATEMENT_LABEL;
    ast_label_statement_init(&out->u.label_statement,
			     ast_meta_make(pos_start, ps_pos(p)),
			     label);
    return 1;
  } else if (try_skip_keyword(p, "return")) {
    skip_ws(p);
    struct ast_expr expr;
    if (!parse_expr(p, &expr, kSemicolonPrecedence)) {
      return 0;
    }
    if (!try_skip_semicolon(p)) {
      ast_expr_destroy(&expr);
      return 0;
    }

    struct ast_expr *heap_expr;
    malloc_move_ast_expr(expr, &heap_expr);
    out->tag = AST_STATEMENT_RETURN_EXPR;
    out->u.return_expr = heap_expr;
    return 1;
  } else if (try_skip_keyword(p, "if")) {
    return parse_rest_of_if_statement(p, pos_start, out);
  } else {
    struct ast_expr expr;
    if (!parse_expr(p, &expr, kSemicolonPrecedence)) {
      return 0;
    }

    if (!try_skip_semicolon(p)) {
      ast_expr_destroy(&expr);
      return 0;
    }

    struct ast_expr *heap_expr;
    malloc_move_ast_expr(expr, &heap_expr);
    out->tag = AST_STATEMENT_EXPR;
    out->u.expr = heap_expr;
    return 1;
  }
}

int parse_bracebody(struct ps *p, struct ast_bracebody *out) {
  size_t pos_start = ps_pos(p);
  if (!try_skip_char(p, '{')) {
    return 0;
  }

  struct ast_statement *statements = NULL;
  size_t statements_count = 0;
  size_t statements_limit = 0;
  for (;;) {
    skip_ws(p);
    if (try_skip_char(p, '}')) {
      ast_bracebody_init(out, ast_meta_make(pos_start, ps_pos(p)),
			 statements, statements_count);
      return 1;
    }

    struct ast_statement statement;
    if (!parse_statement(p, &statement)) {
      SLICE_FREE(statements, statements_count, ast_statement_destroy);
      return 0;
    }

    SLICE_PUSH(statements, statements_count, statements_limit, statement);
  }
}

int parse_rest_of_lambda(struct ps *p, size_t pos_start,
			 struct ast_lambda *out) {
  PARSE_DBG("parse_rest_of_lambda\n");
  skip_ws(p);
  struct ast_vardecl *params;
  size_t params_count;
  if (!parse_params_list(p, &params, &params_count)) {
    return 0;
  }

  skip_ws(p);
  PARSE_DBG("parse_rest_of_lambda parsing return type\n");
  struct ast_typeexpr return_type;
  if (!parse_typeexpr(p, &return_type)) {
    goto fail_params;
  }

  skip_ws(p);
  PARSE_DBG("parse_rest_of_lambda parsing body\n");
  struct ast_bracebody bracebody;
  if (!parse_bracebody(p, &bracebody)) {
    goto fail_return_type;
  }

  PARSE_DBG("parse_rest_of_lambda success\n");
  ast_lambda_init(out, ast_meta_make(pos_start, ps_pos(p)),
		  params, params_count, return_type, bracebody);
  return 1;

 fail_return_type:
  ast_typeexpr_destroy(&return_type);
 fail_params:
  SLICE_FREE(params, params_count, ast_vardecl_destroy);
  return 0;
}

int parse_numeric_literal(struct ps *p, struct ast_numeric_literal *out) {
  int8_t *digits = NULL;
  size_t digits_count = 0;
  size_t digits_limit = 0;

  size_t pos_start = ps_pos(p);
  int32_t first_digit = ps_peek(p);
  if (!is_decimal_digit(first_digit)) {
    return 0;
  }
  ps_step(p);

  int8_t first_digit_value = (int8_t)(first_digit - '0');
  SLICE_PUSH(digits, digits_count, digits_limit, first_digit_value);
  int32_t ch;
  while ((ch = ps_peek(p)), is_decimal_digit(ch)) {
    int8_t ch_value = (int8_t)(ch - '0');
    SLICE_PUSH(digits, digits_count, digits_limit, ch_value);
    ps_step(p);
  }

  if (!is_numeric_postchar(ch)) {
    free(digits);
    return 0;
  }

  ast_numeric_literal_init(out, ast_meta_make(pos_start, ps_pos(p)),
			   digits, digits_count);
  ps_count_leaf(p);
  return 1;
}

int parse_rest_of_arglist(struct ps *p,
			  struct ast_expr **args_out,
			  size_t *args_count_out) {
  struct ast_expr *args = NULL;
  size_t args_count = 0;
  size_t args_limit = 0;

  for (;;) {
    skip_ws(p);
    if (try_skip_char(p, ')')) {
      *args_out = args;
      *args_count_out = args_count;
      return 1;
    }
    if (args_count != 0) {
      if (!try_skip_char(p, ',')) {
	goto fail;
      }
      skip_ws(p);
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

int parse_atomic_expr(struct ps *p, struct ast_expr *out) {
  size_t pos_start = ps_pos(p);
  if (try_skip_keyword(p, "fn")) {
    out->tag = AST_EXPR_LAMBDA;
    return parse_rest_of_lambda(p, pos_start, &out->u.lambda);
  }

  if (is_decimal_digit(ps_peek(p))) {
    out->tag = AST_EXPR_NUMERIC_LITERAL;
    return parse_numeric_literal(p, &out->u.numeric_literal);
  }

  if (is_ident_firstchar(ps_peek(p))) {
    out->tag = AST_EXPR_NAME;
    return parse_ident(p, &out->u.name);
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

  enum ast_unop unop;
  if (try_parse_unop(p, &unop)) {
    struct ast_expr rhs;
    if (!parse_expr(p, &rhs, unop_right_precedence(unop))) {
      return 0;
    }

    struct ast_expr *heap_rhs;
    malloc_move_ast_expr(rhs, &heap_rhs);
    out->tag = AST_EXPR_UNOP;
    ast_unop_expr_init(&out->u.unop_expr, ast_meta_make(pos_start, ps_pos(p)),
		       unop, heap_rhs);
    return 1;
  }

  return 0;
}

int parse_expr(struct ps *p, struct ast_expr *out, int precedence_context) {
  size_t pos_start = ps_pos(p);
  struct ast_expr lhs;
  if (!parse_atomic_expr(p, &lhs)) {
    return 0;
  }
  PARSE_DBG("parse_expr parsed atomic expr\n");

  for (;;) {
    skip_ws(p);
    PARSE_DBG("parse_expr looking for paren\n");
    if (try_skip_char(p, '(')) {
      PARSE_DBG("parse_expr saw paren\n");
      struct ast_expr *args;
      size_t args_count;
      if (!parse_rest_of_arglist(p, &args, &args_count)) {
	goto fail;
      }
      struct ast_expr *heap_lhs;
      malloc_move_ast_expr(lhs, &heap_lhs);
      lhs.tag = AST_EXPR_FUNCALL;
      ast_funcall_init(&lhs.u.funcall,
		       ast_meta_make(pos_start, ps_pos(p)),
		       heap_lhs,
		       args,
		       args_count);
    } else if (try_skip_oper(p, ".")) {
      skip_ws(p);
      struct ast_ident field_name;
      if (!parse_ident(p, &field_name)) {
	goto fail;
      }

      struct ast_expr *heap_lhs;
      malloc_move_ast_expr(lhs, &heap_lhs);
      lhs.tag = AST_EXPR_LOCAL_FIELD_ACCESS;
      ast_local_field_access_init(&lhs.u.local_field_access,
				  ast_meta_make(pos_start, ps_pos(p)),
				  field_name);
    } else if (try_skip_oper(p, "->")) {
      skip_ws(p);
      struct ast_ident field_name;
      if (!parse_ident(p, &field_name)) {
	goto fail;
      }

      struct ast_expr *heap_lhs;
      malloc_move_ast_expr(lhs, &heap_lhs);
      lhs.tag = AST_EXPR_DEREF_FIELD_ACCESS;
      ast_deref_field_access_init(&lhs.u.deref_field_access,
				  ast_meta_make(pos_start, ps_pos(p)),
				  field_name);
    } else if (is_binop_start(ps_peek(p))) {
      struct ps_savestate save = ps_save(p);

      enum ast_binop op;
      if (!parse_binop(p, &op)) {
	goto fail;
      }

      struct precedence_pair op_precedence = binop_precedence(op);

      enum precedence_comparison cmp
	= compare_precedence(precedence_context,
			     op_precedence.left_precedence);

      switch (cmp) {
      case PRECEDENCE_COMPARISON_CONFLICTS:
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

      skip_ws(p);
      struct ast_expr rhs;
      if (!parse_expr(p, &rhs, op_precedence.right_precedence)) {
	goto fail;
      }

      struct ast_expr *heap_lhs;
      malloc_move_ast_expr(lhs, &heap_lhs);
      struct ast_expr *heap_rhs;
      malloc_move_ast_expr(rhs, &heap_rhs);

      lhs.tag = AST_EXPR_BINOP;
      /* TODO: This (and other calls to ast_meta_make after a
	 parse_expr) can include trailing whitespace in the ast_meta
	 interval. */
      ast_binop_expr_init(&lhs.u.binop_expr,
			  ast_meta_make(pos_start, ps_pos(p)),
			  op, heap_lhs, heap_rhs);
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
			struct ast_vardecl **fields_out,
			size_t *fields_count_out) {
  if (!try_skip_char(p, '{')) {
    return 0;
  }
  struct ast_vardecl *fields = NULL;
  size_t fields_count = 0;
  size_t fields_limit = 0;
  for (;;) {
    skip_ws(p);
    if (try_skip_char(p, '}')) {
      *fields_out = fields;
      *fields_count_out = fields_count;
      return 1;
    }

    struct ast_vardecl field;
    if (!parse_vardecl(p, &field)) {
      goto fail;
    }

    skip_ws(p);
    if (!try_skip_semicolon(p)) {
      goto fail;
    }

    SLICE_PUSH(fields, fields_count, fields_limit, field);
  }

 fail:
  SLICE_FREE(fields, fields_count, ast_vardecl_destroy);
  return 0;
}

int parse_rest_of_structe(struct ps *p, size_t pos_start, struct ast_structe *out) {
  skip_ws(p);
  struct ast_vardecl *fields;
  size_t fields_count;
  if (!parse_braced_fields(p, &fields, &fields_count)) {
    return 0;
  }
  ast_structe_init(out, ast_meta_make(pos_start, ps_pos(p)),
		   fields, fields_count);
  return 1;
}

int parse_rest_of_unione(struct ps *p, size_t pos_start, struct ast_unione *out) {
  skip_ws(p);
  struct ast_vardecl *fields;
  size_t fields_count;
  if (!parse_braced_fields(p, &fields, &fields_count)) {
    return 0;
  }
  ast_unione_init(out, ast_meta_make(pos_start, ps_pos(p)),
		  fields, fields_count);
  return 1;
}

int parse_typeexpr(struct ps *p, struct ast_typeexpr *out) {
  size_t pos_start = ps_pos(p);
  if (try_skip_keyword(p, "struct")) {
    out->tag = AST_TYPEEXPR_STRUCTE;
    return parse_rest_of_structe(p, pos_start, &out->u.structe);
  }

  if (try_skip_keyword(p, "union")) {
    out->tag = AST_TYPEEXPR_UNIONE;
    return parse_rest_of_unione(p, pos_start, &out->u.unione);
  }

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
      ast_typeapp_init(&out->u.app, ast_meta_make(pos_start, ps_pos(p)),
		       ident, params, params_count);
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

int parse_type_params_if_present(struct ps *p,
				 struct ast_optional_type_params *out) {
  size_t pos_start = ps_pos(p);
  if (!try_skip_char(p, '[')) {
    ast_optional_type_params_init_no_params(out);
    return 1;
  }

  struct ast_ident *params = NULL;
  size_t params_count = 0;
  size_t params_limit = 0;
  for (;;) {
    skip_ws(p);
    if (try_skip_char(p, ']')) {
      ast_optional_type_params_init_has_params(out,
					       ast_meta_make(pos_start,
							     ps_pos(p)),
					       params, params_count);
      return 1;
    }

    if (params_count != 0) {
      if (!try_skip_char(p, ',')) {
	goto fail;
      }
      skip_ws(p);
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

int parse_rest_of_def(struct ps *p, size_t pos_start, struct ast_def *out) {
  skip_ws(p);
  struct ast_optional_type_params generics;
  if (!parse_type_params_if_present(p, &generics)) {
    goto fail;
  }

  skip_ws(p);
  struct ast_ident name;
  if (!parse_ident(p, &name)) {
    goto fail_generics;
  }

  skip_ws(p);
  struct ast_typeexpr type;
  if (!parse_typeexpr(p, &type)) {
    goto fail_ident;
  }

  skip_ws(p);
  if (!skip_oper(p, "=")) {
    goto fail_typeexpr;
  }

  skip_ws(p);
  struct ast_expr rhs;
  if (!parse_expr(p, &rhs, kSemicolonPrecedence)) {
    goto fail_typeexpr;
  }

  if (!try_skip_semicolon(p)) {
    goto fail_rhs;
  }

  ast_def_init(out, ast_meta_make(pos_start, ps_pos(p)),
	       generics, name, type, rhs);
  return 1;

 fail_rhs:
  ast_expr_destroy(&rhs);
 fail_typeexpr:
  ast_typeexpr_destroy(&type);
 fail_ident:
  ast_ident_destroy(&name);
 fail_generics:
  ast_optional_type_params_destroy(&generics);
 fail:
  return 0;
}

int parse_rest_of_import(struct ps *p, size_t pos_start, struct ast_import *out) {
  PARSE_DBG("parse_rest_of_import\n");
  skip_ws(p);
  struct ast_ident name;
  if (!parse_ident(p, &name)) {
    goto fail;
  }
  skip_ws(p);
  PARSE_DBG("parse_rest_of_import about to skip semicolon\n");
  if (!try_skip_semicolon(p)) {
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

int parse_rest_of_module(struct ps *p, size_t pos_start,
			 struct ast_module *out) {
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
      ast_module_init(out, ast_meta_make(pos_start, ps_pos(p)),
		      name, toplevels, toplevels_count);
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

int parse_rest_of_deftype(struct ps *p, size_t pos_start,
			  struct ast_deftype *out) {
  PARSE_DBG("parse_rest_of_deftype");
  skip_ws(p);
  struct ast_optional_type_params generics;
  if (!parse_type_params_if_present(p, &generics)) {
    goto fail;
  }
  skip_ws(p);
  struct ast_ident name;
  if (!parse_ident(p, &name)) {
    goto fail_generics;
  }
  skip_ws(p);
  struct ast_typeexpr type;
  if (!parse_typeexpr(p, &type)) {
    goto fail_ident;
  }
  skip_ws(p);
  if (!try_skip_semicolon(p)) {
    goto fail_typeexpr;
  }
  ast_deftype_init(out, ast_meta_make(pos_start, ps_pos(p)),
		   generics, name, type);
  return 1;

 fail_typeexpr:
  ast_typeexpr_destroy(&type);
 fail_ident:
  ast_ident_destroy(&name);
 fail_generics:
  ast_optional_type_params_destroy(&generics);
 fail:
  return 0;
}

int parse_toplevel(struct ps *p, struct ast_toplevel *out) {
  PARSE_DBG("parse_toplevel\n");
  size_t pos_start = ps_pos(p);
  if (try_skip_keyword(p, "def")) {
    out->tag = AST_TOPLEVEL_DEF;
    return parse_rest_of_def(p, pos_start, &out->u.def);
  } else if (try_skip_keyword(p, "import")) {
    out->tag = AST_TOPLEVEL_IMPORT;
    return parse_rest_of_import(p, pos_start, &out->u.import);
  } else if (try_skip_keyword(p, "module")) {
    out->tag = AST_TOPLEVEL_MODULE;
    return parse_rest_of_module(p, pos_start, &out->u.module);
  } else if (try_skip_keyword(p, "deftype")) {
    out->tag = AST_TOPLEVEL_DEFTYPE;
    return parse_rest_of_deftype(p, pos_start, &out->u.deftype);
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

int count_parse_buf(const uint8_t *buf, size_t length,
		    size_t *leafcount_out, size_t *error_pos_out) {
  struct ps p;
  ps_init(&p, buf, length);

  struct ast_file file;
  PARSE_DBG("parse_file...\n");
  int ret = parse_file(&p, &file);
  if (ret) {
    *leafcount_out = p.leafcount;
    ast_file_destroy(&file);
  } else {
    *error_pos_out = p.pos;
  }
  ps_destroy(&p);
  return ret;
}

int count_parse(const char *str, size_t *leafcount_out, size_t *error_pos_out) {
  size_t length = strlen(str);
  const uint8_t *data = (const uint8_t *)str;
  return count_parse_buf(data, length, leafcount_out, error_pos_out);
}

int run_count_test(const char *name, const char *str, size_t expected) {
  DBG("run_count_test %s...\n", name);
  size_t count;
  size_t pos;
  int res = count_parse(str, &count, &pos);
  if (!res) {
    DBG("run_count_test %s FAIL: parse failed at pos %"PRIz"\n", name, pos);
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

int parse_test_modules(void) {
  return run_count_test("modules",
			"module a { module b {import c; }import d; module egret{} } "
			"module zed {\n\t}  ",
			22);
}

int parse_test_defs(void) {
  int pass = 1;
  pass &= run_count_test("def01", "def a int = 0;", 6);
  pass &= run_count_test("def02", "def b int = 1;", 6);
  pass &= run_count_test("def03", "def a int =0   ;  ", 6);
  pass &= run_count_test("def04", "def abc_def int = 12345;", 6);
  pass &= run_count_test("def05", "def foo func[int, int] = 1;", 11);
  pass &= run_count_test("def06", "def foo func[int, int] = fn(x int, y int) int { 3; };", 23);
  pass &= run_count_test("def07", "def foo func[int, int] = \n"
			 "\tfn(x int, y int) int { foo(bar); };\n",
			 26);
  pass &= run_count_test("def08", "def foo func[int, int] = \n"
			 "\tfn(x int, y int) int { foo(bar); goto blah; label feh; };\n",
			 32);
  pass &= run_count_test("def09",
			 "def foo func[int, int] = \n" /* 9 */
			 "\tfn() int { foo(*bar.blah); if (n) { " /* 15 */
			 "goto blah; } label feh; \n" /* 7 */
			 "if n { goto blah; } else { meh; } };\n" /* 14 */,
			 48);
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
			 "def foo func[int] = fn() int {\n"
			 "   var x int = 3;\n"
			 "   return x;\n"
			 "};\n",
			 23);
  pass &= run_count_test("def14",
			 "def[a,b] foo func[int] = fn() int {\n"
			 "   var x int = 3;\n"
			 "   return x;\n"
			 "};\n",
			 28);
  return pass;
}

int parse_test_deftypes(void) {
  int pass = 1;
  pass &= run_count_test("deftype1",
			 " deftype foo bar;",
			 4);
  pass &= run_count_test("deftype2",
			 "deftype foo func[int, int] ; ",
			 9);
  pass &= run_count_test("deftype3",
			 "deftype foo struct { x y; z int; t func[beh]; };\n"
			 "deftype [ c, d ]  bar union{a b;c d[e,f];};",
			 40);
  return pass;
}

int parse_test(void) {
  int pass = 1;
  pass &= parse_test_nothing();
  pass &= parse_test_whitespace();
  pass &= parse_test_imports();
  pass &= parse_test_modules();
  pass &= parse_test_defs();
  pass &= parse_test_deftypes();
  return pass;
}

