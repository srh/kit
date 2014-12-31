#include "win/objfile.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "slice.h"
#include "util.h"

struct databuf {
  uint8_t *buf;
  size_t count;
  size_t limit;
};

void databuf_init(struct databuf *b) {
  b->buf = NULL;
  b->count = 0;
  b->limit = 0;
}

void databuf_destroy(struct databuf *b) {
  free(b->buf);
  databuf_init(b);
}

void databuf_move_destroy(struct databuf *b, void **buf_out, size_t *count_out) {
  void *buf = realloc(b->buf, b->count);
  CHECK(buf || b->count == 0);
  *buf_out = buf;
  *count_out = b->count;
  databuf_init(b);
}

void databuf_grow(struct databuf *b, size_t accomodated_count) {
  size_t limit = b->limit;
  while (limit < accomodated_count) {
    limit = limit ? size_mul(limit, 2) : 64;
  }
  b->buf = realloc(b->buf, limit);
  CHECK(b->buf || limit == 0);
  b->limit = limit;
}

void databuf_append(struct databuf *b, const void *p, size_t count) {
  if (count == 0) {
    /* Avoid possible memcpy NULL-pointer 0-count situations, which
       would be undefined behavior. */
    return;
  }
  if (b->limit - b->count < count) {
    databuf_grow(b, size_add(b->count, count));
  }
  memcpy(b->buf + b->count, p, count);
  b->count += count;
}

void app8(struct databuf *d, uint8_t x) {
  databuf_append(d, &x, 1);
}

void app16(struct databuf *d, uint16_t x) {
  STATIC_CHECK(LITTLE_ENDIAN);
  databuf_append(d, &x, 2);
}

void app32(struct databuf *d, uint32_t x) {
  STATIC_CHECK(LITTLE_ENDIAN);
  databuf_append(d, &x, 4);
}

#define IMAGE_FILE_MACHINE_I386 0x14c
#define kFakeTimeDateStamp 12345

PACK_PUSH
struct COFF_Header {
  uint16_t Machine;
  uint16_t NumberOfSections;
  uint32_t TimeDateStamp;
  uint32_t PointerToSymbolTable;
  uint32_t NumberOfSymbols;
  uint16_t SizeOfOptionalHeader;
  uint16_t Characteristics;
} PACK_ATTRIBUTE;
PACK_POP

#define COFF_Header_EXPECTED_SIZE 20

PACK_PUSH
struct Section_Header {
  char Name[8];
  uint32_t VirtualSize;
  uint32_t VirtualAddress;
  uint32_t SizeOfRawData;
  uint32_t PointerToRawData;
  uint32_t PointerToRelocations;
  uint32_t PointerToLineNumbers;
  uint16_t NumberOfRelocations;
  uint16_t NumberOfLineNumbers;
  uint32_t Characteristics;
} PACK_ATTRIBUTE;
PACK_POP

#define Section_Header_EXPECTED_SIZE 40

uint16_t real_file_characteristics(void) {
  /*
    0x0001 IMAGE_FILE_RELOCS_STRIPPED: image only.
    0x0002 IMAGE_FILE_EXECUTABLE_IMAGE: image only.
    0x0004 IMAGE_FILE_LINE_NUMS_STRIPPED: deprecated, should be zero.
    0x0008 IMAGE_FILE_LOCAL_SYMS_STRIPPED: deprecated, should be zero.
    0x0010 IMAGE_FILE_AGGRESSIVE_WS_TRIM: deprecated, must be zero.
    0x0020 IMAGE_FILE_LARGE_ADDRESS_AWARE: Presumably zero, unaware, for 32-bit progs.
    0x0040 reserved
    0x0080 IMAGE_FILE_BYTES_REVERSED_LO: deprecated, should be zero.
    0x0100 IMAGE_FILE_32BIT_MACHINE: Machine is 32-bit.
           (As opposed to 16-bit?  Or 64-bit?  cl is outputting 0 for 32-bit obj files.)
    0x0200 IMAGE_FILE_DEBUG_STRIPPED:
           Debugging information is removed from the image file.
           (Irrelevant for obj files?  cl is outputting 0.)
    0x0400 IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP:
           for if the image is on removable media... I say, set to zero.
    0x0800 IMAGE_FILE_NET_RUN_FROM_SWAP:
           for if the image is on network-mounted media... I say, set to zero.
    0x1000 IMAGE_FILE_SYSTEM: a system file, not a user program
    0x2000 IMAGE_FILE_DLL: The image file is a DLL.
    0x4000 IMAGE_FILE_UP_SYSTEM_ONLY: Only run file on a uniprocessor machine.
    0x8000 IMAGE_FILE_BYTES_REVERSED_HI: deprecated, should be zero.
  */

  /* cl outputs 0 for a general .obj file too. */
  return 0;
}

uint32_t executable_section_characteristics(void) {
  /*
    bit  0. Reserved.
    bit  1. Reserved.
    bit  2. Reserved.
    bit  3. IMAGE_SCN_TYPE_NO_PAD. Obsolete, replaced by IMAGE_SCN_ALIGN_1BYTES.
    bit  4. Reserved.
    bit  5. IMAGE_SCN_CNT_CODE. The section contains executable code.
    bit  6. IMAGE_SCN_CNT_INITIALIZED_DATA. The section contains initialized data.
    bit  7. IMAGE_SCN_CNT_UNINITIALIZED_DATA. The section contains uninitialized data.
    bit  8. IMAGE_SCN_LNK_OTHER. Reserved.
    bit  9. IMAGE_SCN_LINK_INFO. The section contains comments or other
            information. The .drectve section has this type. Object files only.
    bit 10. Reserved.
    bit 11. IMAGE_SCN_LNK_REMOVE. The section will not become part of the image.
            Object files only.
    bit 12. IMAGE_SCN_LNK_COMDAT. The section contains COMDAT data. Object files only.
    bit 13. Undocumented.
    bit 14. Undocumented.
    bit 15. IMAGE_SCN_GPREL. The section contains data referenced through the global pointer (GP). TODO: Wtf is this?
    bit 16. IMAGE_SCN_MEM_PURGEABLE (reserved)? Or undocumented?
    bit 17. IMAGE_SCN_MEM_PURGEABLE (reserved)? Or undocumented? IMAGE_SCN_MEM_16BIT for ARM, section contains Thumb code.
    bit 18. IMAGE_SCN_MEM_LOCKED. Reserved.
    bit 19. IMAGE_SCN_MEM_PRELOAD. Reserved.
    bits 20:23. IMAGE_SCN_ALIGN_ ## n ## BYTES. Align data on a 2^(k-1) boundary.
                Valid only for object files.
    bit 24. IMAGE_SCN_LNK_NRELOC_OVFL. The section contains extended relocations. (We don't support that yet.)
    bit 25. IMAGE_SCN_MEM_DISCARDABLE. The section can be discarded as needed. (I guess ours can't be.)
    bit 26. IMAGE_SCN_MEM_NOT_CACHED. The section cannot be cached. (Who knows.)
    bit 27. IMAGE_SCN_MEM_NOT_PAGED. The section is not pageable. (Ours are.)
    bit 28. IMAGE_SCN_MEM_SHARED. The section can be shared in memory. (We know nothing.)
    bit 29. IMAGE_SCN_MEM_EXECUTE. The section can be executed as code.
    bit 30. IMAGE_SCN_MEM_READ. The section can be read.
    bit 31. IMAGE_SCN_MEM_WRITE. The section can be written to.
  */
  const uint32_t IMAGE_SCN_CNT_CODE = (1 << 5);
  const uint32_t IMAGE_SCN_ALIGN_16_BYTES = (5ul << 20);
  const uint32_t IMAGE_SCN_MEM_EXECUTE = (1ul << 29);
  const uint32_t IMAGE_SCN_MEM_READ = (1ul << 30);
  /* This value is also that produced by cl for its .text sections. */
  return IMAGE_SCN_CNT_CODE | IMAGE_SCN_ALIGN_16_BYTES | IMAGE_SCN_MEM_EXECUTE
    | IMAGE_SCN_MEM_READ;
}

PACK_PUSH
struct objfile_symbol_standard_record {
  union {
    uint8_t ShortName[8];
    struct {
      uint32_t Zeroes;
      uint32_t Offset;
    } LongName;
  } Name;
  uint32_t Value;
  /* Uses a 1-based index into the section table.  Special values:
     IMAGE_SYM_UNDEFINED (0).  Section not yet defined, e.g. for an
     external symbol.
     IMAGE_SYM_ABSOLUTE (0xFFFF).  The symbol has an absolute value,
     not an address relative to some section.
     IMAGE_SYM_DEBUG (0xFFFE).  Some debuggery. */
  uint16_t SectionNumber;
  /* MS tools set this field to 0x20 (function) or 0x0 (not a function). */
  uint16_t Type;
  /* MS tools generally only use IMAGE_SYM_CLASS_EXTERNAL (2),
     IMAGE_SYM_CLASS_STATIC (3), IMAGE_SYM_CLASS_FUNCTION (101), and
     IMAGE_SYM_CLASS_FILE (103) which is followed by aux records that
     name the file.  There's also a 'weak external' storage class,
     which is mentioned. */
  uint8_t StorageClass;
  /* How many aux records follow this record. */
  uint8_t NumberOfAuxSymbols;
} PACK_ATTRIBUTE;
PACK_POP

PACK_PUSH
struct objfile_symbol_aux_function {
  /* Index of the corresponding .bf symbol record. */
  uint32_t TagIndex;
  /* The size of the executable code for the function itself.  Doesn't
     overflow the section's SizeOfRawData. */
  uint32_t TotalSize;
  /* Zero if no COFF line number entry exists. */
  uint32_t PointerToLineNumber;
  /* Index of the symbol record for the next function.  Last function
     has zero. */
  uint32_t PointerToNextFunction;
  /* Set this to zero, for hygiene. */
  uint16_t Unused;
} PACK_ATTRIBUTE;
PACK_POP

PACK_PUSH
struct objfile_symbol_aux_bfef {
  uint32_t Unused1;
  /* The actual ordinal line number within the source file,
     corresponding to the .bf or .ef record. */
  uint16_t Linenumber;
  uint16_t Unused2;
  uint32_t Unused3;
  /* .bf only.  The symbol-table index of the next .bf symbol record.
     Zero for the last function in the symbol table. */
  uint32_t PointerToNextFunction;
  uint16_t Unused;
} PACK_ATTRIBUTE;
PACK_POP

PACK_PUSH
union objfile_symbol_record {
  struct objfile_symbol_standard_record standard;
  struct objfile_symbol_aux_function aux_function;
  struct objfile_symbol_aux_bfef aux_bfef;
} PACK_ATTRIBUTE;
PACK_POP

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
     IMAGE_REL_I386_SECTION 0x000A The 16-bit section index of the section that contains the target.  For debugging information.
     IMAGE_REL_I386_SECREL 0x000B The 32-bit offset of the target from the beginning of its section.  For debugging info.  Also for static thread local storage.
     IMAGE_REL_I386_TOKEN 0x000C The CLR token.  (wut.)
     IMAGE_REL_I386_SECREL7 0x000D The 7-bit offset from the base of the section that contains the target.
     IMAGE_REL_I386_REL32 0x0014 The 32-bit relative displacement from the target.  This supports the x86 relative branch and call instructions.
   */
  uint16_t Type;
} PACK_ATTRIBUTE;
PACK_POP

struct objfile_section {
  struct databuf raw;

  struct COFF_Relocation *relocs;
  size_t relocs_count;
  size_t relocs_limit;
};

void objfile_section_init(struct objfile_section *s) {
  databuf_init(&s->raw);

  s->relocs = NULL;
  s->relocs_count = 0;
  s->relocs_limit = 0;
}

void objfile_section_destroy(struct objfile_section *s) {
  databuf_destroy(&s->raw);
  free(s->relocs);
  s->relocs = NULL;
  s->relocs_count = 0;
  s->relocs_limit = 0;
}

size_t objfile_section_raw_size(struct objfile_section *s) {
  return s->raw.count;
}

uint16_t objfile_section_small_relocations_count(struct objfile_section *s) {
  /* TODO: Support an extended relocations count. */
  CHECK(s->relocs_count <= UINT16_MAX);
  return (uint16_t)s->relocs_count;
}

struct objfile_data {
  struct objfile_section text;

  union objfile_symbol_record *symbol_table;
  size_t symbol_table_count;
  size_t symbol_table_limit;

  /* strings does not include the 4-byte size field that would exist
     at the beginning of the strings table (which immediately follows
     the symbol table on disk).  Its value will be strings_count + 4
     (because it accounts for its own size usage).  Following the size
     field is a concatenation of null-terminated strings. */
  uint8_t *strings;
  size_t strings_count;
  size_t strings_limit;
};

void objfile_data_init(struct objfile_data *f) {
  objfile_section_init(&f->text);

  f->symbol_table = NULL;
  f->symbol_table_count = 0;
  f->symbol_table_limit = 0;

  f->strings = NULL;
  f->strings_count = 0;
  f->strings_limit = 0;
}

void objfile_data_destroy(struct objfile_data *f) {
  objfile_section_destroy(&f->text);

  free(f->symbol_table);
  f->symbol_table = NULL;
  f->symbol_table_count = 0;
  f->symbol_table_limit = 0;

  free(f->strings);
  f->strings = NULL;
  f->strings_count = 0;
  f->strings_limit = 0;
}

void objfile_flatten(struct objfile_data *f, struct databuf **out) {
  STATIC_CHECK(LITTLE_ENDIAN);
  struct databuf *d = malloc(sizeof(*d));
  CHECK(d);
  databuf_init(d);

  /* Right now we just have a .text section. */
  const uint16_t number_of_sections = 1;
  const uint32_t end_of_section_headers
    = uint32_add(sizeof(struct COFF_Header),
                 uint32_mul(number_of_sections,
                            sizeof(struct Section_Header)));

  const uint32_t end_of_symbols
    = uint32_add(end_of_section_headers,
                 uint32_mul(size_to_uint32(f->symbol_table_count), sizeof(union objfile_symbol_record)));
  const uint32_t strings_size = uint32_add(size_to_uint32(f->strings_count), 4);
  STATIC_CHECK(sizeof(strings_size) == 4);
  const uint32_t end_of_strings = uint32_add(end_of_symbols, strings_size);
  const uint32_t ceil_end_of_strings = uint32_ceil_aligned(end_of_strings, 16);
  const uint32_t end_of_text_raw = uint32_add(ceil_end_of_strings, f->text.raw.count);
  const uint32_t ceil_end_of_text_raw = uint32_ceil_aligned(end_of_text_raw, 2);
  const uint32_t end_of_text_relocs = uint32_add(ceil_end_of_text_raw,
                                                 uint32_mul(f->text.relocs_count,
                                                            sizeof(struct COFF_Relocation)));


  {
    struct COFF_Header h;
    STATIC_CHECK(sizeof(h) == COFF_Header_EXPECTED_SIZE);
    h.Machine = IMAGE_FILE_MACHINE_I386;
    h.NumberOfSections = number_of_sections;
    h.TimeDateStamp = kFakeTimeDateStamp;
    h.PointerToSymbolTable = end_of_section_headers;
    h.NumberOfSymbols = f->symbol_table_count;
    /* Should be zero for an object file. */
    h.SizeOfOptionalHeader = 0;
    h.Characteristics = real_file_characteristics();

    databuf_append(d, &h, sizeof(h));
  }

  /* Right now, we've got the text section. */
  {
    struct Section_Header h;
    STATIC_CHECK(sizeof(h) == Section_Header_EXPECTED_SIZE);
    STATIC_CHECK(sizeof(h.Name) == 8 && sizeof(h.Name[0]) == 1);
    static const char dot_text_string[8] = ".text";
    memcpy(h.Name, dot_text_string, 8);
    /* Should be set to zero for object files. */
    h.VirtualSize = 0;
    /* For simplicity, should be set to zero for object files. */
    h.VirtualAddress = 0;
    h.SizeOfRawData = objfile_section_raw_size(&f->text);
    h.PointerToRawData = ceil_end_of_strings;
    h.PointerToRelocations = ceil_end_of_text_raw;
    /* We output no COFF line numbers. */
    h.PointerToLineNumbers = 0;
    h.NumberOfRelocations = objfile_section_small_relocations_count(&f->text);
    h.NumberOfLineNumbers = 0;
    h.Characteristics = executable_section_characteristics();

    databuf_append(d, &h, sizeof(h));
  }
  CHECK(d->count == end_of_section_headers);

  databuf_append(d, f->symbol_table, size_mul(f->symbol_table_count, sizeof(union objfile_symbol_record)));
  CHECK(d->count == end_of_symbols);
  STATIC_CHECK(sizeof(strings_size) == 4);
  databuf_append(d, &strings_size, sizeof(strings_size));
  databuf_append(d, f->strings, f->strings_count);
  CHECK(d->count == end_of_strings);

  if (d->count % 16 != 0) {
    uint8_t ch[16] = { 0 };
    databuf_append(d, ch, 16 - (d->count % 16));
  }

  CHECK(d->count == ceil_end_of_strings);

  databuf_append(d, f->text.raw.buf, f->text.raw.count);
  CHECK(d->count == end_of_text_raw);
  if (d->count % 2 != 0) {
    uint8_t ch = 0;
    databuf_append(d, &ch, 1);
  }
  CHECK(d->count == ceil_end_of_text_raw);

  databuf_append(d, f->text.relocs, size_mul(f->text.relocs_count, sizeof(struct COFF_Relocation)));
  CHECK(d->count == end_of_text_relocs);

  *out = d;
}

void objfile_section_append_raw(struct objfile_section *s, const void *buf, size_t n) {
  databuf_append(&s->raw, buf, n);
}

#define IMAGE_REL_I386_DIR32 0x0006
#define IMAGE_REL_I386_DIR32NB 0x0007
#define IMAGE_REL_I386_REL32 0x0014

void objfile_section_append_32bit_reloc(struct objfile_section *s,
                                        uint32_t SymbolTableIndex,
                                        uint16_t Type) {
  struct COFF_Relocation reloc;
  reloc.VirtualAddress = s->raw.count;
  reloc.SymbolTableIndex = SymbolTableIndex;
  reloc.Type = Type;
  SLICE_PUSH(s->relocs, s->relocs_count, s->relocs_limit, reloc);
  uint32_t zero = 0;
  objfile_section_append_raw(s, &zero, sizeof(zero));
}

void objfile_section_append_dir32(struct objfile_section *s, uint32_t SymbolTableIndex) {
  objfile_section_append_32bit_reloc(s, SymbolTableIndex, IMAGE_REL_I386_DIR32);
}

void objfile_section_append_dir32nb(struct objfile_section *s, uint32_t SymbolTableIndex) {
  objfile_section_append_32bit_reloc(s, SymbolTableIndex, IMAGE_REL_I386_DIR32NB);
}

void objfile_section_append_rel32(struct objfile_section *s, uint32_t SymbolTableIndex) {
  objfile_section_append_32bit_reloc(s, SymbolTableIndex, IMAGE_REL_I386_REL32);
}

int make_almost_blank_objfile(void **buf_out, size_t *count_out) {
  STATIC_CHECK(LITTLE_ENDIAN);
  struct objfile_data f;
  objfile_data_init(&f);
  objfile_section_append_raw(&f.text, "abcdefghij", 10);

  struct databuf *d;
  objfile_flatten(&f, &d);
  databuf_move_destroy(d, buf_out, count_out);
  free(d);

  objfile_data_destroy(&f);
  return 1;
}
