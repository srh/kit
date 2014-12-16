#include "typecheck.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* TODO: Windows-specific. */
#include <direct.h>

#include "ast.h"
#include "identmap.h"
#include "io.h"
#include "parse.h"
#include "slice.h"
#include "table.h"
#include "util.h"

struct import {
  ident_value import_name;
  struct ast_file *file;
};

void import_destroy(struct import *imp) {
  ast_file_destroy(imp->file);
  free(imp->file);
}

struct checkstate {
  struct ident_map *im;

  struct import *imports;
  size_t imports_count;
  size_t imports_limit;

  struct name_table nt;
};

void checkstate_init(struct checkstate *cs, struct ident_map *im) {
  cs->im = im;
  cs->imports = NULL;
  cs->imports_count = 0;
  cs->imports_limit = 0;
  name_table_init(&cs->nt);
}

void checkstate_destroy(struct checkstate *cs) {
  name_table_destroy(&cs->nt);
  SLICE_FREE(cs->imports, cs->imports_count, import_destroy);
  cs->imports_limit = 0;
  cs->im = NULL;
}

int resolve_import_filename_and_parse(struct ident_map *im,
				      ident_value name,
				      struct ast_file *file_out) {
  int ret = 0;
  char *filename;
  {
    const void *buf;
    size_t count;
    ident_map_lookup(im, name, &buf, &count);

    filename = malloc(size_add(count, 4));
    CHECK(filename);
    memcpy(filename, buf, count);
    memcpy(filename + count, ".ki", 4);
  }

  uint8_t *data;
  size_t data_size;
  if (!read_file(filename, &data, &data_size)) {
    ERR_DBG("Could not read file.\n");
    goto fail_filename;
  }

  size_t error_pos;
  if (!parse_buf_file(im, data, data_size, file_out, &error_pos)) {
    ERR_DBG("Could not parse import.\n");
    goto fail_data;
  }

  ret = 1;
 fail_data:
  free(data);
 fail_filename:
  free(filename);
  return ret;
}

int build_table_import(struct checkstate *cs, struct ast_import *a) {
  for (size_t i = 0, e = cs->imports_count; i < e; i++) {
    if (cs->imports[i].import_name == a->name.value) {
      return 1;
    }
  }

  struct ast_file file;
  if (!resolve_import_filename_and_parse(cs->im, a->name.value, &file)) {
    return 0;
  }
  struct ast_file *heap_file = malloc(sizeof(*heap_file));
  CHECK(heap_file);
  *heap_file = file;
  struct import imp;
  imp.import_name = a->name.value;
  imp.file = heap_file;
  SLICE_PUSH(cs->imports, cs->imports_count, cs->imports_limit, imp);
  return 1;
}

int build_table_def(struct checkstate *cs, struct ast_def *a) {
  (void)cs, (void)a;
  /* TODO: Implement. */
  DBG("Warning: build_table_def: not implemented.\n");
  return 1;
}

int build_table_deftype(struct checkstate *cs, struct ast_deftype *a) {
  (void)cs, (void)a;
  /* TODO: Implement. */
  DBG("Warning: build_table_deftype: not implemented.\n");
  return 1;
}

int build_table_toplevel(struct checkstate *cs, struct ast_toplevel *a) {
  switch (a->tag) {
  case AST_TOPLEVEL_IMPORT:
    return build_table_import(cs, &a->u.import);
  case AST_TOPLEVEL_DEF:
    return build_table_def(cs, &a->u.def);
  case AST_TOPLEVEL_DEFTYPE:
    return build_table_deftype(cs, &a->u.deftype);
  default:
    UNREACHABLE();
  }
}

int check_file(struct ident_map *im, struct ast_file *file) {
  struct checkstate cs;
  checkstate_init(&cs, im);

  for (size_t i = 0, e = file->toplevels_count; i < e; i++) {
    build_table_toplevel(&cs, &file->toplevels[i]);
  }

  /* TODO: Implement actual checking. */
  checkstate_destroy(&cs);
  return 1;
}

int test_check_file(void) {
  int ret = 0;
  if (0 != _chdir("test")) {
    DBG("Could not chdir into 'test'.\n");
    goto cleanup_nothing;
  }

  uint8_t *data;
  size_t size;
  if (!read_file("foo.ki", &data, &size)) {
    DBG("Could not read file\n");
    goto cleanup_chdir;
  }

  struct ident_map im;
  ident_map_init(&im);
  struct ast_file file;
  size_t error_pos;
  if (!parse_buf_file(&im, data, size, &file, &error_pos)) {
    DBG("Could not parse file.\n");
    goto cleanup_ident_map;
  }

  if (!check_file(&im, &file)) {
    DBG("check_file failed.\n");
    goto cleanup_file;
  }

  ret = 1;
 cleanup_file:
  ast_file_destroy(&file);
 cleanup_ident_map:
  ident_map_destroy(&im);
  free(data);
 cleanup_chdir:
  if (0 != _chdir("..")) {
    DBG("Could not chdir out of 'test'.\n");
    ret = 0;
  }
 cleanup_nothing:
  return ret;
}
