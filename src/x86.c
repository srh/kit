#include "x86.h"

#include <stdlib.h>

#include "arith.h"
#include "ast.h"
#include "opgraph.h"
#include "slice.h"
#include "table.h"
#include "typecheck.h"
#include "win/objfile.h"

#define DWORD_SIZE 4

/* X86 WINDOWS */
void kira_sizealignof(struct name_table *nt, struct ast_typeexpr *type,
                      uint32_t *sizeof_out, uint32_t *alignof_out) {
  switch (type->tag) {
  case AST_TYPEEXPR_NAME: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(nt, type->u.name.value,
                                   no_param_list_arity(),
                                   &ent)) {
      CRASH("Type name should be found, it was not.\n");
    }
    CHECK(ent->arity.value == ARITY_NO_PARAMLIST);
    if (ent->is_primitive) {
      *sizeof_out = ent->primitive_sizeof;
      *alignof_out = ent->primitive_alignof;
    } else {
      struct ast_deftype *deftype = ent->deftype;
      CHECK(!deftype->generics.has_type_params);
      kira_sizealignof(nt, &deftype->type, sizeof_out, alignof_out);
    }
  } break;
  case AST_TYPEEXPR_APP: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(nt, type->u.app.name.value,
                                   param_list_arity(type->u.app.params_count),
                                   &ent)) {
      CRASH("Type app name should be found, it was not.\n");
    }
    CHECK(ent->arity.value == type->u.app.params_count);
    if (ent->is_primitive) {
      *sizeof_out = ent->primitive_sizeof;
      *alignof_out = ent->primitive_alignof;
    } else {
      struct ast_deftype *deftype = ent->deftype;
      CHECK(deftype->generics.has_type_params
            && deftype->generics.params_count == type->u.app.params_count);
      struct ast_typeexpr substituted;
      do_replace_generics(&deftype->generics,
                          type->u.app.params,
                          &deftype->type,
                          &substituted);
      kira_sizealignof(nt, &substituted, sizeof_out, alignof_out);
      ast_typeexpr_destroy(&substituted);
    }
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    uint32_t count = 0;
    uint32_t max_alignment = 1;
    for (size_t i = 0, e = type->u.structe.fields_count; i < e; i++) {
      uint32_t size;
      uint32_t alignment;
      kira_sizealignof(nt, &type->u.structe.fields[i].type,
                       &size, &alignment);
      count = uint32_ceil_aligned(count, alignment);
      if (max_alignment < alignment) {
        max_alignment = alignment;
      }
      count = uint32_add(count, size);
    }
    count = uint32_ceil_aligned(count, max_alignment);
    *sizeof_out = count;
    *alignof_out = max_alignment;
  } break;
  case AST_TYPEEXPR_UNIONE: {
    uint32_t max_size = 0;
    uint32_t max_alignment = 1;
    for (size_t i = 0, e = type->u.unione.fields_count; i < e; i++) {
      uint32_t size;
      uint32_t alignment;
      kira_sizealignof(nt, &type->u.unione.fields[i].type,
                       &size, &alignment);
      if (max_size < size) {
        size = max_size;
      }
      if (max_alignment < alignment) {
        max_alignment = alignment;
      }
    }
    uint32_t final_size = uint32_ceil_aligned(max_size, max_alignment);
    *sizeof_out = final_size;
    *alignof_out = max_alignment;
  } break;
  case AST_TYPEEXPR_UNKNOWN:
  default:
    UNREACHABLE();
  }
}

uint32_t kira_sizeof(struct name_table *nt, struct ast_typeexpr *type) {
  uint32_t size;
  uint32_t alignment;
  kira_sizealignof(nt, type, &size, &alignment);
  return size;
}

enum loc_tag {
  LOC_EBP,
  LOC_REGNUM,
};

struct loc {
  enum loc_tag tag;
  union {
    int32_t ebp_offset;
    int regnum;
  } u;
};

struct loc loc_ebp_offset(int32_t offset) {
  struct loc ret;
  ret.tag = LOC_EBP;
  ret.u.ebp_offset = offset;
  return ret;
}

struct loc_interval {
  struct opnum begin;
  struct opnum end;
  struct loc loc;
};

struct loc_interval loc_interval(struct opnum begin,
                                 struct opnum end,
                                 struct loc loc) {
  struct loc_interval ret;
  ret.begin = begin;
  ret.end = end;
  ret.loc = loc;
  return ret;
}

struct x86_varnode {
  struct loc_interval *intervals;
  size_t intervals_count;
  size_t intervals_limit;
};

void x86_varnode_init(struct x86_varnode *xvn) {
  xvn->intervals = NULL;
  xvn->intervals_count = 0;
  xvn->intervals_limit = 0;
}

void x86_varnode_destroy(struct x86_varnode *xvn) {
  free(xvn->intervals);
  x86_varnode_init(xvn);
}

struct x86_frame {
  /* TODO: Check WINDOWS X86 esp alignment rules. */

  /* How much space to reserve (below ebp) for static info. */
  size_t static_size;
  /* How much space to reserve for misc info. */
  size_t misc_size;

  struct x86_varnode *varinfo;
  size_t varinfo_count;
};

void x86_frame_init(struct opgraph *g, struct x86_frame *a) {
  a->static_size = 0;
  a->misc_size = 0;
  struct x86_varnode *xvns = malloc_mul(sizeof(*xvns), g->vars_count);
  for (size_t i = 0, e = g->vars_count; i < e; i++) {
    x86_varnode_init(&xvns[i]);
  }
  a->varinfo = xvns;
  a->varinfo_count = g->vars_count;
}

void x86_frame_destroy(struct x86_frame *a) {
  a->static_size = 0;
  a->misc_size = 0;
  SLICE_FREE(a->varinfo, a->varinfo_count, x86_varnode_destroy);
}

void x86_annotate_calling_convention_locs(struct checkstate *cs,
                                          struct opgraph *g,
                                          struct x86_frame *h) {
  uint32_t return_size = kira_sizeof(&cs->nt, &opgraph_varnode(g, g->fg.return_var)->type);

  /* The ebp_offset starts at 2 * DWORD_SIZE -- we have the saved ebp,
     and return pointer, in our way.  Then, if the return type has
     size greater than 8, we have a DWORD-sized pointer to where the
     return value should be written.  Then comes the first
     argument. */
  int32_t ebp_offset = (2 + (return_size > 8)) * DWORD_SIZE;

  for (size_t i = 0, e = g->fg.arg_vars_count; i < e; i++) {
    struct varnum argvar = g->fg.arg_vars[i];
    struct varnode *node = opgraph_varnode(g, argvar);

    uint32_t var_size = kira_sizeof(&cs->nt, &node->type);

    struct x86_varnode *xvn = &h->varinfo[argvar.value];

    struct loc_interval li = loc_interval(opgraph_entry_point(g),
                                          opgraph_future_0(g),
                                          loc_ebp_offset(ebp_offset));
    SLICE_PUSH(xvn->intervals, xvn->intervals_count, xvn->intervals_limit, li);

    ebp_offset = int32_add(ebp_offset, uint32_to_int32(var_size));
  }
}

int gen_x86_function(struct checkstate *cs, struct objfile *f,
                     struct opgraph *g) {
  struct x86_frame h;
  x86_frame_init(g, &h);
  x86_annotate_calling_convention_locs(cs, g, &h);



  (void)cs, (void)g;  /* TODO */
  /* X86 WINDOWS */
  objfile_fillercode_align_double_quadword(f);

  x86_frame_destroy(&h);
  /* TODO: Implement. */
  return 1;
}

