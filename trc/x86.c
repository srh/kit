#include "x86.h"

#include <stdlib.h>

#include "arith.h"
#include "ast.h"
#include "slice.h"
#include "table.h"
#include "typecheck.h"
#include "objfile_objfile.h"


void help_sizealignof(struct name_table *nt, struct ast_typeexpr *type,
                      ident_value fieldstop, uint32_t *offsetof_out,
                      struct type_attrs *attrs_out);
void help_rhs_sizealignof(struct name_table *nt, struct ast_deftype_rhs *rhs,
                          ident_value fieldstop, uint32_t *offsetof_out,
                          struct type_attrs *attrs_out);

void unionfields_sizealignof(struct name_table *nt,
                             struct ast_vardecl *fields,
                             size_t fields_count,
                             ident_value fieldstop,
                             uint32_t *offsetof_out,
                             struct type_attrs *attrs_out) {
  uint32_t max_size = 0;
  uint32_t max_alignment = 1;
  for (size_t i = 0; i < fields_count; i++) {
    uint32_t invalid_offsetof_param;
    struct type_attrs attrs;
    help_sizealignof(nt, &fields[i].type,
                     IDENT_VALUE_INVALID,
                     &invalid_offsetof_param, &attrs);

    if (fields[i].name.value == fieldstop) {
      CHECK(fieldstop != IDENT_VALUE_INVALID);
      *offsetof_out = 0;
      attrs_out->size = attrs.size;
      /* Garbage fields. */
      attrs_out->align = 0;
      attrs_out->is_primitive = 0;
      return;
    }

    if (max_size < attrs.size) {
      max_size = attrs.size;
    }
    if (max_alignment < attrs.align) {
      max_alignment = attrs.align;
    }
  }
  CHECK(fieldstop == IDENT_VALUE_INVALID);
  uint32_t final_size = uint32_ceil_aligned(max_size, max_alignment);
  *offsetof_out = 0;
  attrs_out->size = final_size;
  attrs_out->align = max_alignment;
  /* Not primitive, it's a union type. */
  attrs_out->is_primitive = 0;
  return;
}

/* X86 WINDOWS */
/* This is a bi-use function -- if fieldstop is IDENT_VALUE_INVALID,
then *attrs_out is initialized with the size and alignment (and other
attributes) of the type.  Otherwise, *offsetof_out and attrs_out->size
are initialized with the offset and size of the type's field named
fieldstop.  If the field name is not found, crashes. */
void help_sizealignof(struct name_table *nt, struct ast_typeexpr *type,
                      ident_value fieldstop, uint32_t *offsetof_out,
                      struct type_attrs *attrs_out) {
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
      attrs_out->size = ent->primitive_sizeof;
      attrs_out->align = ent->primitive_alignof;
      attrs_out->is_primitive = 1;
      return;
    } else {
      struct ast_deftype *deftype = ent->deftype;
      CHECK(!deftype->generics.has_type_params);
      help_rhs_sizealignof(nt, &deftype->rhs, fieldstop,
                           offsetof_out, attrs_out);
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
      attrs_out->size = ent->primitive_sizeof;
      attrs_out->align = ent->primitive_alignof;
      attrs_out->is_primitive = 1;
      return;
    } else {
      struct ast_deftype *deftype = ent->deftype;
      CHECK(deftype->generics.has_type_params
            && deftype->generics.params_count == type->u.app.params_count);
      struct ast_deftype_rhs substituted;
      do_replace_rhs_generics(&deftype->generics,
                              type->u.app.params,
                              type->u.app.params_count,
                              &deftype->rhs,
                              &substituted);
      help_rhs_sizealignof(nt, &substituted, fieldstop,
                           offsetof_out, attrs_out);
      ast_deftype_rhs_destroy(&substituted);
      return;
    }
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    uint32_t count = 0;
    uint32_t max_alignment = 1;
    for (size_t i = 0, e = type->u.structe.fields_count; i < e; i++) {
      uint32_t invalid_offsetof_param;
      struct type_attrs attrs;
      help_sizealignof(nt, &type->u.structe.fields[i].type,
                       IDENT_VALUE_INVALID,
                       &invalid_offsetof_param, &attrs);
      count = uint32_ceil_aligned(count, attrs.align);

      if (type->u.structe.fields[i].name.value == fieldstop) {
        CHECK(fieldstop != IDENT_VALUE_INVALID);
        *offsetof_out = count;
        attrs_out->size = attrs.size;
        /* Garbage fields. */
        attrs_out->align = 0;
        attrs_out->is_primitive = 0;
        return;
      }

      if (max_alignment < attrs.align) {
        max_alignment = attrs.align;
      }
      count = uint32_add(count, attrs.size);
    }
    CHECK(fieldstop == IDENT_VALUE_INVALID);
    count = uint32_ceil_aligned(count, max_alignment);
    *offsetof_out = 0;
    attrs_out->size = count;
    attrs_out->align = max_alignment;
    /* Not primitive: it's a struct! */
    attrs_out->is_primitive = 0;
    return;
  } break;
  case AST_TYPEEXPR_UNIONE: {
    unionfields_sizealignof(nt,
                            type->u.unione.fields,
                            type->u.unione.fields_count,
                            fieldstop,
                            offsetof_out,
                            attrs_out);
    return;
  } break;
  case AST_TYPEEXPR_ARRAY: {
    CHECK(fieldstop == IDENT_VALUE_INVALID);

    uint32_t offsetof_discard;
    struct type_attrs elem_attrs;
    help_sizealignof(nt, type->u.arraytype.param, IDENT_VALUE_INVALID,
                     &offsetof_discard, &elem_attrs);
    *offsetof_out = 0;
    attrs_out->size = uint32_mul(elem_attrs.size,
                                 unsafe_numeric_literal_u32(&type->u.arraytype.number));
    attrs_out->align = elem_attrs.align;
    /* Arrays are not primitive. */
    attrs_out->is_primitive = 0;
    return;
  } break;
  case AST_TYPEEXPR_UNKNOWN:
    UNREACHABLE();
  case AST_TYPEEXPR_NUMERIC:
    UNREACHABLE();
  default:
    UNREACHABLE();
  }
}

void help_rhs_sizealignof(struct name_table *nt, struct ast_deftype_rhs *rhs,
                          ident_value fieldstop, uint32_t *offsetof_out,
                          struct type_attrs *attrs_out) {
  switch (rhs->tag) {
  case AST_DEFTYPE_RHS_TYPE:
    help_sizealignof(nt, &rhs->u.type, fieldstop, offsetof_out, attrs_out);
    return;
  case AST_DEFTYPE_RHS_ENUMSPEC: {
    CHECK(fieldstop == IDENT_VALUE_INVALID);
    uint32_t body_offset_discard;
    struct type_attrs body_attrs;
    unionfields_sizealignof(nt,
                            rhs->u.enumspec.enumfields,
                            rhs->u.enumspec.enumfields_count,
                            fieldstop,
                            &body_offset_discard,
                            &body_attrs);

    /* Other code expects enum nums to be dword-sized, the body offset
    likewise. */
    CHECK(body_attrs.align <= DWORD_SIZE);
    *offsetof_out = 0;
    attrs_out->size = uint32_add(body_attrs.size, DWORD_SIZE);
    attrs_out->align = DWORD_SIZE;
    attrs_out->is_primitive = 0;
    return;
  } break;
  default:
    UNREACHABLE();
  }
}

void x86_field_sizeoffset(struct name_table *nt, struct ast_typeexpr *type,
                          ident_value field_name, uint32_t *sizeof_out,
                          uint32_t *offsetof_out) {
  CHECK(field_name != IDENT_VALUE_INVALID);
  uint32_t offset;
  struct type_attrs attrs;
  help_sizealignof(nt, type, field_name, &offset, &attrs);
  *sizeof_out = attrs.size;
  *offsetof_out = offset;
}

struct type_attrs x86_attrsof(struct name_table *nt, struct ast_typeexpr *type) {
  struct type_attrs attrs;
  uint32_t invalid_offsetof_param;
  help_sizealignof(nt, type, IDENT_VALUE_INVALID,
                   &invalid_offsetof_param, &attrs);
  return attrs;
}

uint32_t x86_sizeof(struct name_table *nt, struct ast_typeexpr *type) {
  struct type_attrs attrs = x86_attrsof(nt, type);
  return attrs.size;
}

uint32_t x86_alignof(struct name_table *nt, struct ast_typeexpr *type) {
  struct type_attrs attrs = x86_attrsof(nt, type);
  return attrs.align;
}
