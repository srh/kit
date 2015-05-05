#include "objfile/win.h"

#include <string.h>

#include "arith.h"
#include "objfile/structs.h"
#include "util.h"

PACK_PUSH
struct COFF_Relocation {
  /* Offset from beginning of section, assuming its section header
  VirtualAddress is zero. */
  uint32_t VirtualAddress;
  /* Index into the symbol table. */
  uint32_t SymbolTableIndex;
  /* What kind of relocation should be performed?
  IMAGE_REL_I386_ABSOLUTE 0x0000 The relocation is ignored.
  IMAGE_REL_I386_DIR16 0x0001 Not supported.
  IMAGE_REL_I386_REL16 0x0002 Not supported.
  IMAGE_REL_I386_DIR32 0x0006 The target's 32-bit VA.
  IMAGE_REL_I386_DIR32NB 0x0007 The target's 32-bit RVA.
  IMAGE_REL_I386_SEG12 0x0009 Not supported.
  IMAGE_REL_I386_SECTION 0x000A The 16-bit section index of the section
  that contains the target.  For debugging information.
  IMAGE_REL_I386_SECREL 0x000B The 32-bit offset of the target from the
  beginning of its section.  For debugging info.  Also for static thread
  local storage.
  IMAGE_REL_I386_TOKEN 0x000C The CLR token.  (wut.)
  IMAGE_REL_I386_SECREL7 0x000D The 7-bit offset from the base of the
  section that contains the target.
  IMAGE_REL_I386_REL32 0x0014 The 32-bit relative displacement from the
  target.  This supports the x86 relative branch and call instructions.
  */
  /* Looking at cl output, I see a bunch of use of 6h and 14h. */
  uint16_t Type;
} PACK_ATTRIBUTE;
PACK_POP

static const uint8_t IMAGE_SYM_CLASS_EXTERNAL = 2;
static const uint8_t IMAGE_SYM_CLASS_STATIC = 3;

PACK_PUSH
struct COFF_symbol_standard_record {
  union name_eight Name;
  uint32_t Value;
  /* Uses a 1-based index into the section table.  Special values:
  IMAGE_SYM_UNDEFINED (0).  Section not yet defined, e.g. for an
  external symbol.
  IMAGE_SYM_ABSOLUTE (0xFFFF).  The symbol has an absolute value,
  not an address relative to some section.
  IMAGE_SYM_DEBUG (0xFFFE).  Some debuggery. */
  uint16_t SectionNumber;
  /* MS tools set this field to 0x20 (function) or 0x0 (not a
  function).  I.e. kFunctionSymType or kNullSymType. */
  uint16_t Type;
  /* MS tools generally only use IMAGE_SYM_CLASS_EXTERNAL (2),
  IMAGE_SYM_CLASS_STATIC (3), IMAGE_SYM_CLASS_FUNCTION (101), and
  IMAGE_SYM_CLASS_FILE (103) which is followed by aux records that
  name the file.  There's also a 'weak external' storage class, which
  is mentioned.  Note: I don't see FILE used in a .obj file.  Also
  FUNCTION is used for .bf and .ef records, which I don't see
  either.  We only use EXTERNAL and STATIC. */
  uint8_t StorageClass;
  /* How many aux records follow this record. */
  uint8_t NumberOfAuxSymbols;
} PACK_ATTRIBUTE;
PACK_POP

PACK_PUSH
struct COFF_symbol_aux_sectiondef {
  uint32_t Length;
  uint16_t NumberOfRelocations;
  uint16_t NumberOfLineNumbers;
  /* COMDAT-only, set to zero. */
  /* I don't know if this is really COMDAT-only, I see it being used... */
  uint32_t CheckSum;
  /* COMDAT-only, set to zero. */
  uint16_t Number;
  /* COMDAT selection number, set to zero. */
  uint8_t Selection;
  uint8_t Unused[3];
} PACK_ATTRIBUTE;
PACK_POP

static const uint16_t kNullSymType = 0;
static const uint16_t kFunctionSymType = 0x20;

enum { IMAGE_SYM_UNDEFINED = 0 };


const uint32_t kNumSectionSymbolsPerSection = 2;

void win_append_section_symbols(
    struct databuf *d,
    struct objfile_section *s,
    uint16_t SectionNumber) {
  {
    struct COFF_symbol_standard_record standard;
    STATIC_CHECK(sizeof(standard.Name.ShortName) == 8);
    memcpy(standard.Name.ShortName, s, 8);
    standard.Value = 0;
    standard.SectionNumber = SectionNumber;
    standard.Type = 0;
    standard.StorageClass = IMAGE_SYM_CLASS_STATIC;
    standard.NumberOfAuxSymbols = 1;
    databuf_append(d, &standard, sizeof(standard));
  }
  {
    struct COFF_symbol_aux_sectiondef aux_sectiondef;
    aux_sectiondef.Length = size_to_uint32(objfile_section_raw_size(s));
    aux_sectiondef.NumberOfRelocations
      = objfile_section_small_relocations_count(s);
    aux_sectiondef.NumberOfLineNumbers = 0;
    aux_sectiondef.CheckSum = 0;
    aux_sectiondef.Number = 0;
    aux_sectiondef.Selection = 0;
    STATIC_CHECK(sizeof(aux_sectiondef.Unused) == 3);
    memset(aux_sectiondef.Unused, 0, 3);
    databuf_append(d, &aux_sectiondef, sizeof(aux_sectiondef));
  }
}

uint32_t win_symbols_to_write(struct objfile *f) {
  return uint32_add(uint32_mul(kNumSectionSymbolsPerSection, kNumberOfSections),
                    size_to_uint32(f->symbol_table_count));
}

uint32_t win_symbols_filesize(struct objfile *f) {
  return uint32_mul(win_symbols_to_write(f),
                    sizeof(struct COFF_symbol_standard_record));
}

void win_append_symbols(struct databuf *d,
                        struct objfile_symbol_record *symbol_table,
                        size_t symbol_table_count) {
  for (size_t i = 0; i < symbol_table_count; i++) {
    struct COFF_symbol_standard_record standard;
    standard.Name = symbol_table[i].Name;
    standard.Value = symbol_table[i].Value;
    STATIC_CHECK((int)OBJFILE_SYMBOL_SECTION_UNDEFINED == (int)IMAGE_SYM_UNDEFINED);
    standard.SectionNumber = symbol_table[i].section;
    standard.Type = symbol_table[i].is_function == IS_FUNCTION_YES ?
      kFunctionSymType : kNullSymType;
    standard.StorageClass = symbol_table[i].is_static == IS_STATIC_YES ?
      IMAGE_SYM_CLASS_STATIC : IMAGE_SYM_CLASS_EXTERNAL;
    standard.NumberOfAuxSymbols = 0;
    databuf_append(d, &standard, sizeof(standard));
  }
}

void win_append_all_section_symbols(struct databuf *d,
                                    struct objfile *f) {
  win_append_section_symbols(d, &f->data, 1);
  win_append_section_symbols(d, &f->rdata, 2);
  win_append_section_symbols(d, &f->text, 3);
}

void win_compute_section_dimensions(struct objfile_section *s,
                                    uint32_t start_of_raw,
                                    uint32_t *PointerToRelocations_out,
                                    uint32_t *pointer_to_end_out) {
  CHECK(start_of_raw % WIN_SECTION_ALIGNMENT == 0);
  uint32_t end_of_raw = uint32_add(start_of_raw, s->raw.count);
  uint32_t start_of_relocs = uint32_ceil_aligned(end_of_raw, 2);
  uint32_t end_of_relocs
    = uint32_add(start_of_relocs, uint32_mul(s->relocs_count,
                                             sizeof(struct COFF_Relocation)));
  *PointerToRelocations_out = start_of_relocs;
  *pointer_to_end_out = end_of_relocs;
}

#define IMAGE_REL_I386_DIR32 0x0006
#define IMAGE_REL_I386_DIR32NB 0x0007
#define IMAGE_REL_I386_REL32 0x0014

void win_append_relocs(struct databuf *d, struct objfile_relocation *relocs,
                       size_t relocs_count) {
  static const uint16_t win_Type[] = {
    [OBJFILE_RELOCATION_TYPE_DIR32] = IMAGE_REL_I386_DIR32,
    [OBJFILE_RELOCATION_TYPE_DIR32NB] = IMAGE_REL_I386_DIR32NB,
    [OBJFILE_RELOCATION_TYPE_REL32] = IMAGE_REL_I386_REL32,
  };

  for (size_t i = 0; i < relocs_count; i++) {
    struct COFF_Relocation coff_reloc;
    coff_reloc.VirtualAddress = relocs[i].virtual_address;
    coff_reloc.SymbolTableIndex = relocs[i].symbol_table_index;
    coff_reloc.Type = win_Type[relocs[i].type];
    databuf_append(d, &coff_reloc, sizeof(coff_reloc));
  }
}

void win_write_section_header(
    struct databuf *d, struct objfile_section *s,
    uint32_t start_of_raw, uint32_t Characteristics) {
  uint32_t PointerToRelocations;
  uint32_t pointer_to_end;
  win_compute_section_dimensions(s, start_of_raw,
                                 &PointerToRelocations, &pointer_to_end);

  struct Section_Header h;
  STATIC_CHECK(sizeof(h) == Section_Header_EXPECTED_SIZE);
  STATIC_CHECK(sizeof(h.Name) == 8 && sizeof(h.Name[0]) == 1);
  STATIC_CHECK(sizeof(s->Name) == 8);
  memcpy(h.Name, s->Name, 8);
  /* Should be set to zero for object files. */
  h.VirtualSize = 0;
  /* For simplicity, should be set to zero for object files. */
  h.VirtualAddress = 0;
  h.SizeOfRawData = objfile_section_raw_size(s);
  h.PointerToRawData = start_of_raw;
  h.PointerToRelocations = PointerToRelocations;
  /* We output no COFF line numbers. */
  h.PointerToLineNumbers = 0;
  h.NumberOfRelocations = objfile_section_small_relocations_count(s);
  h.NumberOfLineNumbers = 0;
  h.Characteristics = Characteristics;

  databuf_append(d, &h, sizeof(h));
}

