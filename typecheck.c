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

int chase_imports(struct checkstate *cs, ident_value name) {
  int ret = 0;
  ident_value *names = NULL;
  size_t names_count = 0;
  size_t names_limit = 0;

  SLICE_PUSH(names, names_count, names_limit, name);

  while (names_count) {
    name = names[--names_count];

    for (size_t i = 0, e = cs->imports_count; i < e; i++) {
      if (cs->imports[i].import_name == name) {
	goto continue_outer;
      }
    }

    struct ast_file file;
    if (!resolve_import_filename_and_parse(cs->im, name, &file)) {
      goto cleanup;
    }

    struct ast_file *heap_file = malloc(sizeof(*heap_file));
    CHECK(heap_file);
    *heap_file = file;
    struct import imp;
    imp.import_name = name;
    imp.file = heap_file;
    SLICE_PUSH(cs->imports, cs->imports_count, cs->imports_limit, imp);

    for (size_t i = 0, e = heap_file->toplevels_count; i < e; i++) {
      struct ast_toplevel *toplevel = &heap_file->toplevels[i];
      if (toplevel->tag == AST_TOPLEVEL_IMPORT) {
	SLICE_PUSH(names, names_count, names_limit, toplevel->u.import.name.value);
      }
    }

  continue_outer:
    continue;
  }

  ret = 1;
 cleanup:
  free(names);
  return ret;
}

int check_module(struct ident_map *im, ident_value name) {
  int ret = 0;
  struct checkstate cs;
  checkstate_init(&cs, im);

  if (!chase_imports(&cs, name)) {
    goto cleanup;
  }

  /* TODO: Implement actual checking, of the module we're going to
     compile. */
  ret = 1;

 cleanup:
  checkstate_destroy(&cs);
  return ret;
}

int test_check_file(void) {
  int ret = 0;
  if (0 != _chdir("test")) {
    DBG("Could not chdir into 'test'.\n");
    goto cleanup_nothing;
  }

  struct ident_map im;
  ident_map_init(&im);
  ident_value foo = ident_map_intern(&im, "foo", strlen("foo"));

  if (!check_module(&im, foo)) {
    DBG("Could not check_module foo\n");
    goto cleanup_ident_map;
  }

  ret = 1;
 cleanup_ident_map:
  ident_map_destroy(&im);
  if (0 != _chdir("..")) {
    DBG("Could not chdir out of 'test'.\n");
    ret = 0;
  }
 cleanup_nothing:
  return ret;
}
