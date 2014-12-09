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
  CHECK(0 <= op && op < sizeof(binop_precedences) / sizeof(binop_precedences[0]));
  return binop_precedences[op];
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

  PARSE_DBG("parse_ident about to ps_intern_ident\n");
  out->value = ps_intern_ident(p, save, ps_save(p));
  PARSE_DBG("parse_ident interned ident\n");
  ps_count_leaf(p);
  return 1;
}

int parse_typeexpr(struct ps *p, struct ast_typeexpr *out);

int parse_vardecl(struct ps *p, struct ast_vardecl *out) {
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

  out->name = name;
  out->type = type;
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

int parse_rest_of_if_statement(struct ps *p, struct ast_statement *out) {
  skip_ws(p);
  struct ast_expr condition;
  if (!parse_expr(p, &condition, kSemicolonPrecedence)) {
    return 0;
  }

  struct ast_bracebody thenbody;
  if (!parse_bracebody(p, &thenbody)) {
    goto fail_condition;
  }

  skip_ws(p);
  if (!try_skip_keyword(p, "else")) {
    struct ast_expr *heap_condition;
    malloc_move_ast_expr(condition, &heap_condition);
    out->tag = AST_STATEMENT_IFTHEN;
    out->u.ifthen_statement.condition = heap_condition;
    out->u.ifthen_statement.thenbody = thenbody;
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
  out->u.ifthenelse_statement.condition = heap_condition;
  out->u.ifthenelse_statement.thenbody = thenbody;
  out->u.ifthenelse_statement.elsebody = elsebody;
  return 1;

 fail_thenbody:
  ast_bracebody_destroy(&thenbody);
 fail_condition:
  ast_expr_destroy(&condition);
  return 0;
}

int parse_statement(struct ps *p, struct ast_statement *out) {
  if (try_skip_keyword(p, "goto")) {
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
    out->u.goto_statement.target = target;
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
    out->u.label_statement.label = label;
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
    return parse_rest_of_if_statement(p, out);
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
  if (!try_skip_char(p, '{')) {
    return 0;
  }

  struct ast_statement *statements = NULL;
  size_t statements_count = 0;
  size_t statements_limit = 0;
  for (;;) {
    skip_ws(p);
    if (try_skip_char(p, '}')) {
      out->statements = statements;
      out->statements_count = statements_count;
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

int parse_rest_of_lambda(struct ps *p, struct ast_lambda *out) {
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
  out->params = params;
  out->params_count = params_count;
  out->return_type = return_type;
  out->bracebody = bracebody;
  return 1;

 fail_return_type:
  ast_typeexpr_destroy(&return_type);
 fail_params:
  SLICE_FREE(params, params_count, ast_vardecl_destroy);
  return 0;
}

int parse_rest_of_numeric_literal(struct ps *p, int32_t first_digit,
				  struct ast_numeric_literal *out) {
  int8_t *digits = NULL;
  size_t digits_count = 0;
  size_t digits_limit = 0;
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

  out->digits = digits;
  out->digits_count = digits_count;
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
  if (try_skip_keyword(p, "fn")) {
    out->tag = AST_EXPR_LAMBDA;
    return parse_rest_of_lambda(p, &out->u.lambda);
  } else if (is_decimal_digit(ps_peek(p))) {
    int32_t ch = ps_peek(p);
    ps_step(p);
    out->tag = AST_EXPR_NUMERIC_LITERAL;
    return parse_rest_of_numeric_literal(p, ch, &out->u.numeric_literal);
  } else if (is_ident_firstchar(ps_peek(p))) {
    out->tag = AST_EXPR_NAME;
    return parse_ident(p, &out->u.name);
  } else if (try_skip_char(p, '(')) {
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
  } else {
    return 0;
  }
}

int parse_expr(struct ps *p, struct ast_expr *out, int precedence_context) {
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
      lhs.u.funcall.func = heap_lhs;
      lhs.u.funcall.args = args;
      lhs.u.funcall.args_count = args_count;
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
      lhs.u.binop_expr.operator = op;
      lhs.u.binop_expr.lhs = heap_lhs;
      lhs.u.binop_expr.rhs = heap_rhs;
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
    goto fail_typeexpr;
  }

  skip_ws(p);
  if (!parse_expr(p, &def.rhs, kSemicolonPrecedence)) {
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
  PARSE_DBG("parse_rest_of_import\n");
  skip_ws(p);
  struct ast_ident ident;
  if (!parse_ident(p, &ident)) {
    return 0;
  }
  skip_ws(p);
  PARSE_DBG("parse_rest_of_import about to skip semicolon\n");
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
  PARSE_DBG("parse_toplevel\n");
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
			 "\tfn() int { foo(bar); if (n) { " /* 15 */
			 "goto blah; } label feh; \n" /* 7 */
			 "if n { goto blah; } else { meh; } };\n" /* 14 */,
			 45);
  pass &= run_count_test("def10",
			 "def foo bar = 2 + 3;",
			 8);
  pass &= run_count_test("def11",
			 "def foo bar = 2 + 3 - 4;",
			 10);
  pass &= run_count_test("def12",
			 "def foo bar = (2 ^ 3) - 4 && x;",
			 14);
  return pass;
}

int parse_test(void) {
  int pass = 1;
  pass &= parse_test_nothing();
  pass &= parse_test_whitespace();
  pass &= parse_test_imports();
  pass &= parse_test_modules();
  pass &= parse_test_defs();
  return pass;
}

