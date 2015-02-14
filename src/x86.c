#include "x86.h"

#include <stdlib.h>

#include "arith.h"
#include "ast.h"
#include "slice.h"
#include "table.h"
#include "typecheck.h"
#include "win/objfile.h"


void help_sizealignof(struct name_table *nt, struct ast_typeexpr *type,
                      ident_value fieldstop, uint32_t *offsetof_out,
                      uint32_t *sizeof_out, uint32_t *alignof_out);
void help_rhs_sizealignof(struct name_table *nt, struct ast_rhs *rhs,
                          ident_value fieldstop, uint32_t *offsetof_out,
                          uint32_t *sizeof_out, uint32_t *alignof_out);

void unionfields_sizealignof(struct name_table *nt,
                             struct ast_vardecl *fields,
                             size_t fields_count,
                             ident_value fieldstop,
                             uint32_t *offsetof_out,
                             uint32_t *sizeof_out,
                             uint32_t *alignof_out) {
  uint32_t max_size = 0;
  uint32_t max_alignment = 1;
  for (size_t i = 0; i < fields_count; i++) {
    uint32_t invalid_offsetof_param;
    uint32_t size;
    uint32_t alignment;
    help_sizealignof(nt, &fields[i].type,
                     IDENT_VALUE_INVALID,
                     &invalid_offsetof_param, &size, &alignment);

    if (fields[i].name.value == fieldstop) {
      CHECK(fieldstop != IDENT_VALUE_INVALID);
      *offsetof_out = 0;
      *sizeof_out = size;
      *alignof_out = 0;
      return;
    }

    if (max_size < size) {
      max_size = size;
    }
    if (max_alignment < alignment) {
      max_alignment = alignment;
    }
  }
  CHECK(fieldstop == IDENT_VALUE_INVALID);
  uint32_t final_size = uint32_ceil_aligned(max_size, max_alignment);
  *offsetof_out = 0;
  *sizeof_out = final_size;
  *alignof_out = max_alignment;
  return;
}

/* X86 WINDOWS */
/* This is a bi-use function -- if fieldstop is IDENT_VALUE_INVALID,
then *sizeof_out and *alignof_out are initialized with the size and
alignment of the type.  Otherwise, *offsetof_out and *sizeof_out are
initialized with the offset and size of the type's field named
fieldstop.  If the field name is not found, crashes. */
void help_sizealignof(struct name_table *nt, struct ast_typeexpr *type,
                      ident_value fieldstop, uint32_t *offsetof_out,
                      uint32_t *sizeof_out, uint32_t *alignof_out) {
  switch (type->tag) {
  case AST_TYPEEXPR_NAME: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(nt, type->u.name.value,
                                   no_param_list_arity(),
                                   &ent)) {
      CRASH("Type name should be found, it was not.");
    }
    CHECK(ent->arity.value == ARITY_NO_PARAMLIST);
    if (ent->is_primitive) {
      CHECK(fieldstop == IDENT_VALUE_INVALID);
      *offsetof_out = 0;
      *sizeof_out = ent->primitive_sizeof;
      *alignof_out = ent->primitive_alignof;
      return;
    } else {
      struct ast_deftype *deftype = ent->deftype;
      CHECK(!deftype->generics.has_type_params);
      help_rhs_sizealignof(nt, &deftype->rhs, fieldstop,
                           offsetof_out, sizeof_out, alignof_out);
      return;
    }
  } break;
  case AST_TYPEEXPR_APP: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(nt, type->u.app.name.value,
                                   param_list_arity(type->u.app.params_count),
                                   &ent)) {
      CRASH("Type app name should be found, it was not.");
    }
    CHECK(ent->arity.value == type->u.app.params_count);
    if (ent->is_primitive) {
      CHECK(fieldstop == IDENT_VALUE_INVALID);
      *offsetof_out = 0;
      *sizeof_out = ent->primitive_sizeof;
      *alignof_out = ent->primitive_alignof;
      return;
    } else {
      struct ast_deftype *deftype = ent->deftype;
      CHECK(deftype->generics.has_type_params
            && deftype->generics.params_count == type->u.app.params_count);
      struct ast_rhs substituted;
      do_replace_rhs_generics(&deftype->generics,
                              type->u.app.params,
                              type->u.app.params_count,
                              &deftype->rhs,
                              &substituted);
      help_rhs_sizealignof(nt, &substituted, fieldstop,
                           offsetof_out, sizeof_out, alignof_out);
      ast_rhs_destroy(&substituted);
      return;
    }
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    uint32_t count = 0;
    uint32_t max_alignment = 1;
    for (size_t i = 0, e = type->u.structe.fields_count; i < e; i++) {
      uint32_t invalid_offsetof_param;
      uint32_t size;
      uint32_t alignment;
      help_sizealignof(nt, &type->u.structe.fields[i].type,
                       IDENT_VALUE_INVALID,
                       &invalid_offsetof_param, &size, &alignment);
      count = uint32_ceil_aligned(count, alignment);

      if (type->u.structe.fields[i].name.value == fieldstop) {
        CHECK(fieldstop != IDENT_VALUE_INVALID);
        *offsetof_out = count;
        *sizeof_out = size;
        *alignof_out = 0;
        return;
      }

      if (max_alignment < alignment) {
        max_alignment = alignment;
      }
      count = uint32_add(count, size);
    }
    CHECK(fieldstop == IDENT_VALUE_INVALID);
    count = uint32_ceil_aligned(count, max_alignment);
    *offsetof_out = 0;
    *sizeof_out = count;
    *alignof_out = max_alignment;
    return;
  } break;
  case AST_TYPEEXPR_UNIONE: {
    unionfields_sizealignof(nt,
                            type->u.unione.fields,
                            type->u.unione.fields_count,
                            fieldstop,
                            offsetof_out,
                            sizeof_out,
                            alignof_out);
    return;
  } break;
  case AST_TYPEEXPR_ARRAY: {
    CHECK(fieldstop == IDENT_VALUE_INVALID);

    uint32_t offsetof_discard;
    uint32_t elem_size;
    uint32_t elem_alignment;
    help_sizealignof(nt, type->u.arraytype.param, IDENT_VALUE_INVALID,
                     &offsetof_discard, &elem_size, &elem_alignment);
    *offsetof_out = 0;
    *sizeof_out = uint32_mul(elem_size, type->u.arraytype.count);
    *alignof_out = 0;
    return;
  } break;
  case AST_TYPEEXPR_UNKNOWN:
  default:
    UNREACHABLE();
  }
}

void help_rhs_sizealignof(struct name_table *nt, struct ast_rhs *rhs,
                          ident_value fieldstop, uint32_t *offsetof_out,
                          uint32_t *sizeof_out, uint32_t *alignof_out) {
  switch (rhs->tag) {
  case AST_RHS_TYPE:
    help_sizealignof(nt, &rhs->u.type, fieldstop, offsetof_out,
                     sizeof_out, alignof_out);
    return;
  case AST_RHS_ENUMSPEC: {
    CHECK(fieldstop == IDENT_VALUE_INVALID);
    uint32_t body_offset_discard;
    uint32_t body_size;
    uint32_t body_align;
    unionfields_sizealignof(nt,
                            rhs->u.enumspec.enumfields,
                            rhs->u.enumspec.enumfields_count,
                            fieldstop,
                            &body_offset_discard,
                            &body_size,
                            &body_align);

    /* Other code expects enum nums to be dword-sized, the body offset
    likewise. */
    CHECK(body_align <= DWORD_SIZE);
    *offsetof_out = 0;
    *sizeof_out = uint32_add(body_size, DWORD_SIZE);
    *alignof_out = DWORD_SIZE;
    return;
  } break;
  default:
    UNREACHABLE();
  }
}

void kira_field_sizeoffset(struct name_table *nt, struct ast_typeexpr *type,
                           ident_value field_name, uint32_t *sizeof_out,
                           uint32_t *offsetof_out) {
  CHECK(field_name != IDENT_VALUE_INVALID);
  uint32_t offset;
  uint32_t size;
  uint32_t invalid_alignment_param;
  help_sizealignof(nt, type, field_name,
                   &offset, &size, &invalid_alignment_param);
  *sizeof_out = size;
  *offsetof_out = offset;
}

void kira_sizealignof(struct name_table *nt, struct ast_typeexpr *type,
                      uint32_t *sizeof_out, uint32_t *alignof_out) {
  uint32_t invalid_offsetof_param;
  help_sizealignof(nt, type, IDENT_VALUE_INVALID,
                   &invalid_offsetof_param, sizeof_out, alignof_out);
}

uint32_t kira_sizeof(struct name_table *nt, struct ast_typeexpr *type) {
  uint32_t size;
  uint32_t alignment;
  kira_sizealignof(nt, type, &size, &alignment);
  return size;
}

uint32_t kira_alignof(struct name_table *nt, struct ast_typeexpr *type) {
  uint32_t size;
  uint32_t alignment;
  kira_sizealignof(nt, type, &size, &alignment);
  return alignment;
}

