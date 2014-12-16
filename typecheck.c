#include "typecheck.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
  module_loader *loader;
  struct ident_map *im;

  struct import *imports;
  size_t imports_count;
  size_t imports_limit;

  struct name_table nt;
};

void checkstate_init(struct checkstate *cs,
		     module_loader *loader,
		     struct ident_map *im) {
  cs->loader = loader;
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

int resolve_import_filename_and_parse(struct checkstate *cs,
				      ident_value name,
				      struct ast_file *file_out) {
  int ret = 0;

  const void *module_name;
  size_t module_name_count;
  ident_map_lookup(cs->im, name, &module_name, &module_name_count);

  uint8_t *data;
  size_t data_size;
  if (!(*cs->loader)(module_name, module_name_count, &data, &data_size)) {
    ERR_DBG("Could not read file.\n");
    goto fail;
  }

  size_t error_pos;
  if (!parse_buf_file(cs->im, data, data_size, file_out, &error_pos)) {
    ERR_DBG("Could not parse import.\n");
    goto fail_data;
  }

  ret = 1;
 fail_data:
  free(data);
 fail:
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
    if (!resolve_import_filename_and_parse(cs, name, &file)) {
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

int check_module(struct ident_map *im, module_loader *loader, ident_value name) {
  int ret = 0;
  struct checkstate cs;
  checkstate_init(&cs, loader, im);

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

int read_module_file(const uint8_t *module_name,
		     size_t module_name_count,
		     uint8_t **data_out,
		     size_t *data_size_out) {
  int ret = 0;
  char *filename;
  {
    filename = malloc(size_add(module_name_count, 4));
    CHECK(filename);
    memcpy(filename, module_name, module_name_count);
    memcpy(filename + module_name_count, ".ki", 4);
  }

  if (!read_file(filename, data_out, data_size_out)) {
    ERR_DBG("Could not read file.\n");
  } else {
    ret = 1;
  }

  free(filename);
  return ret;
}

struct test_module {
  const char *name;
  const char *data;
};

int load_test_module(struct test_module *a, size_t a_count,
		     const uint8_t *name, size_t name_count,
		     uint8_t **data_out, size_t *data_count_out) {
  for (size_t i = 0; i < a_count; i++) {
    if (strlen(a[i].name) == name_count
	&& 0 == memcmp(a[i].name, name, name_count)) {
      STATIC_CHECK(sizeof(uint8_t) == 1);
      size_t data_count = strlen(a[i].data);
      uint8_t *data = malloc(data_count);
      CHECK(data);
      memcpy(data, a[i].data, data_count);
      *data_out = data;
      *data_count_out = data_count;
      return 1;
    }
  }
  return 0;
}

int check_file_test_1(const uint8_t *name, size_t name_count,
		      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
			       "import bar;\n"
			       "\n"
			       "def x int = 3;" },
			     { "bar",
			       "import foo;\n"
			       "\n"
			       "def y double = 5;\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
			  name, name_count, data_out, data_count_out);
}

int test_check_file(void) {
  int ret = 0;
  struct ident_map im;
  ident_map_init(&im);
  ident_value foo = ident_map_intern(&im, "foo", strlen("foo"));

  if (!check_module(&im, &check_file_test_1, foo)) {
    DBG("Could not check_module foo\n");
    goto cleanup_ident_map;
  }

  ret = 1;
 cleanup_ident_map:
  ident_map_destroy(&im);
  return ret;
}
