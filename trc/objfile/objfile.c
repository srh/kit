#include "objfile/objfile.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "databuf.h"
#include "objfile/structs.h"
#include "objfile/win.h"
#include "slice.h"
#include "util.h"

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

uint16_t real_file_characteristics(void) {
  /*
  0x0001 IMAGE_FILE_RELOCS_STRIPPED: image only.
  0x0002 IMAGE_FILE_EXECUTABLE_IMAGE: image only.
  0x0004 IMAGE_FILE_LINE_NUMS_STRIPPED: deprecated, should be zero.
  0x0008 IMAGE_FILE_LOCAL_SYMS_STRIPPED: deprecated, should be zero.
  0x0010 IMAGE_FILE_AGGRESSIVE_WS_TRIM: deprecated, must be zero.
  0x0020 IMAGE_FILE_LARGE_ADDRESS_AWARE:
             Presumably zero, unaware, for 32-bit progs.
  0x0040 reserved
  0x0080 IMAGE_FILE_BYTES_REVERSED_LO: deprecated, should be zero.
  0x0100 IMAGE_FILE_32BIT_MACHINE:
             Machine is 32-bit. (As opposed to 16-bit?  Or 64-bit?
             cl is outputting 0 for 32-bit obj files.)
  0x0200 IMAGE_FILE_DEBUG_STRIPPED:
             Debugging information is removed from the image file.
             (Irrelevant for obj files?  cl is outputting 0.)
  0x0400 IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP:
             for if the image is on removable media... I say, set to zero.
  0x0800 IMAGE_FILE_NET_RUN_FROM_SWAP:
             for if the image is on network-mounted media... I say,
             set to zero.
  0x1000 IMAGE_FILE_SYSTEM: a system file, not a user program
  0x2000 IMAGE_FILE_DLL: The image file is a DLL.
  0x4000 IMAGE_FILE_UP_SYSTEM_ONLY: Only run file on a uniprocessor machine.
  0x8000 IMAGE_FILE_BYTES_REVERSED_HI: deprecated, should be zero.
  */

  /* cl outputs 0 for a general .obj file too. */
  return 0;
}

static const uint32_t IMAGE_SCN_CNT_CODE = (1 << 5);
static const uint32_t IMAGE_SCN_CNT_INITIALIZED_DATA = (1 << 6);
static const uint32_t IMAGE_SCN_ALIGN_4_BYTES = (3ul << 20);
static const uint32_t IMAGE_SCN_ALIGN_8_BYTES = (4ul << 20);
static const uint32_t IMAGE_SCN_ALIGN_16_BYTES = (5ul << 20);
static const uint32_t IMAGE_SCN_MEM_EXECUTE = (1ul << 29);
static const uint32_t IMAGE_SCN_MEM_READ = (1ul << 30);
static const uint32_t IMAGE_SCN_MEM_WRITE = (1ul << 31);

uint32_t text_section_characteristics(void) {
  /*
  bit  0. Reserved.
  bit  1. Reserved.
  bit  2. Reserved.
  bit  3. IMAGE_SCN_TYPE_NO_PAD.
          Obsolete, replaced by IMAGE_SCN_ALIGN_1BYTES.
  bit  4. Reserved.
  bit  5. IMAGE_SCN_CNT_CODE. The section contains executable code.
  bit  6. IMAGE_SCN_CNT_INITIALIZED_DATA.
          The section contains initialized data.
  bit  7. IMAGE_SCN_CNT_UNINITIALIZED_DATA.
          The section contains uninitialized data.
  bit  8. IMAGE_SCN_LNK_OTHER. Reserved.
  bit  9. IMAGE_SCN_LINK_INFO. The section contains comments or other
          information. The .drectve section has this type. Object files only.
  bit 10. Reserved.
  bit 11. IMAGE_SCN_LNK_REMOVE. The section will not become part of
          the image. Object files only.
  bit 12. IMAGE_SCN_LNK_COMDAT. The section contains COMDAT
          data. Object files only.
  bit 13. Undocumented.
  bit 14. Undocumented.
  bit 15. IMAGE_SCN_GPREL.
      The section contains data referenced through the global pointer (GP).
  bit 16. IMAGE_SCN_MEM_PURGEABLE (reserved)? Or undocumented?
  bit 17. IMAGE_SCN_MEM_PURGEABLE (reserved)? Or undocumented?
          IMAGE_SCN_MEM_16BIT for ARM, section contains Thumb code.
  bit 18. IMAGE_SCN_MEM_LOCKED. Reserved.
  bit 19. IMAGE_SCN_MEM_PRELOAD. Reserved.
  bits 20:23. IMAGE_SCN_ALIGN_ ## n ## BYTES. Align data on a 2^(k-1)
              boundary. Valid only for object files.
  bit 24. IMAGE_SCN_LNK_NRELOC_OVFL. The section contains extended
          relocations. (We don't support that yet.)
  bit 25. IMAGE_SCN_MEM_DISCARDABLE. The section can be discarded
          as needed. (I guess ours can't be.)
  bit 26. IMAGE_SCN_MEM_NOT_CACHED. The section cannot be cached.
          (Who knows.)
  bit 27. IMAGE_SCN_MEM_NOT_PAGED. The section is not pageable. (Ours are.)
  bit 28. IMAGE_SCN_MEM_SHARED. The section can be shared in memory. (Ok...)
  bit 29. IMAGE_SCN_MEM_EXECUTE. The section can be executed as code.
  bit 30. IMAGE_SCN_MEM_READ. The section can be read.
  bit 31. IMAGE_SCN_MEM_WRITE. The section can be written to.
  */
  /* This value is also that produced by cl for its .text sections. */
  return IMAGE_SCN_CNT_CODE | IMAGE_SCN_ALIGN_16_BYTES | IMAGE_SCN_MEM_EXECUTE
    | IMAGE_SCN_MEM_READ;
}

uint32_t section_alignment_characteristic(size_t max_requested_alignment) {
 switch (max_requested_alignment) {
 case 4: return IMAGE_SCN_ALIGN_4_BYTES;
 case 8: return IMAGE_SCN_ALIGN_8_BYTES;
 case 16: return IMAGE_SCN_ALIGN_16_BYTES;
 default:
   CRASH("max_requested_alignment has a weird value.");
 }
}

uint32_t rdata_section_characteristics(size_t max_requested_alignment) {
  return IMAGE_SCN_CNT_INITIALIZED_DATA
    | section_alignment_characteristic(max_requested_alignment)
    | IMAGE_SCN_MEM_READ;
}

uint32_t data_section_characteristics(size_t max_requested_alignment) {
  /* cl uses 8 bytes if there's a double, by the way. */
  return IMAGE_SCN_CNT_INITIALIZED_DATA
    | section_alignment_characteristic(max_requested_alignment)
    | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
}

static const uint8_t IMAGE_SYM_CLASS_EXTERNAL = 2;
static const uint8_t IMAGE_SYM_CLASS_STATIC = 3;
static const uint8_t IMAGE_SYM_CLASS_FUNCTION = 101;
static const uint8_t IMAGE_SYM_CLASS_FILE = 103;

PACK_PUSH
union name_eight {
  uint8_t ShortName[8];
  struct {
    uint32_t Zeroes;
    uint32_t Offset;
  } LongName;
} PACK_ATTRIBUTE;
PACK_POP

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
  either. */
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

PACK_PUSH
union COFF_symbol_record {
  struct COFF_symbol_standard_record standard;
  struct COFF_symbol_aux_sectiondef aux_sectiondef;
} PACK_ATTRIBUTE;
PACK_POP

static const uint16_t kNullSymType = 0;
static const uint16_t kFunctionSymType = 0x20;

void objfile_section_init(struct objfile_section *s, const char Name[8]) {
  memcpy(s->Name, Name, 8);

  databuf_init(&s->raw);

  /* Just start it off with 4, that's good. */
  s->max_requested_alignment = 4;

  s->relocs = NULL;
  s->relocs_count = 0;
  s->relocs_limit = 0;
}

void objfile_section_destroy(struct objfile_section *s) {
  STATIC_CHECK(sizeof(s->Name) == 8);
  memset(s->Name, 0, sizeof(s->Name));
  databuf_destroy(&s->raw);
  s->max_requested_alignment = 0;
  free(s->relocs);
  s->relocs = NULL;
  s->relocs_count = 0;
  s->relocs_limit = 0;
}

uint16_t section_to_SectionNumber(enum section section) {
  CHECK(section >= SECTION_DATA && section <= SECTION_TEXT);
  return section;
}

struct objfile {
  struct objfile_section data;
  struct objfile_section rdata;
  struct objfile_section text;

  union COFF_symbol_record *symbol_table;
  size_t symbol_table_count;
  size_t symbol_table_limit;

  /* strings does not include the 4-byte size field that would exist
  at the beginning of the strings table (which immediately follows the
  symbol table on disk).  Its value will be strings.count + 4 (because
  it accounts for its own size usage).  Following the size field is a
  concatenation of null-terminated strings. */
  struct databuf strings;
};

void objfile_init(struct objfile *f) {
  static const char DataName[8] = ".data";
  objfile_section_init(&f->data, DataName);
  static const char ReadDataName[8] = ".rdata";
  objfile_section_init(&f->rdata, ReadDataName);
  static const char TextName[8] = ".text";
  objfile_section_init(&f->text, TextName);

  f->symbol_table = NULL;
  f->symbol_table_count = 0;
  f->symbol_table_limit = 0;

  databuf_init(&f->strings);
}

void objfile_destroy(struct objfile *f) {
  objfile_section_destroy(&f->data);
  objfile_section_destroy(&f->rdata);
  objfile_section_destroy(&f->text);

  free(f->symbol_table);
  f->symbol_table = NULL;
  f->symbol_table_count = 0;
  f->symbol_table_limit = 0;

  databuf_destroy(&f->strings);
}

void objfile_alloc(struct objfile **p_out) {
  CHECK(*p_out == NULL);
  struct objfile *p = malloc(sizeof(*p));
  CHECK(p);
  objfile_init(p);
  *p_out = p;
}

void objfile_free(struct objfile **p_ref) {
  CHECK(*p_ref);
  objfile_destroy(*p_ref);
  free(*p_ref);
  *p_ref = NULL;
}

void append_fillercode_to_align(struct databuf *d, size_t alignment) {
  CHECK(alignment > 0);
  size_t n = d->count % alignment;
  if (n != 0) {
    /* X86 */
    static const uint8_t ch[16] = { 0xCC, 0xCC, 0xCC, 0xCC,
                                    0xCC, 0xCC, 0xCC, 0xCC,
                                    0xCC, 0xCC, 0xCC, 0xCC,
                                    0xCC, 0xCC, 0xCC, 0xCC };

    size_t m = size_sub(alignment, n);

    while (m > 16) {
      databuf_append(d, ch, 16);
      m = size_sub(m, 16);
    }
    databuf_append(d, ch, m);
  }
}

void append_zeros_to_align(struct databuf *d, size_t alignment) {
  CHECK(alignment > 0);
  size_t n = d->count % alignment;
  if (n != 0) {
    static const uint8_t ch[16] = { 0 };

    size_t m = size_sub(alignment, n);

    while (m > 16) {
      databuf_append(d, ch, 16);
      m = size_sub(m, 16);
    }
    databuf_append(d, ch, m);
  }
}

/* Checks that name doesn't have any null characters (it must be
null-terminatable), and that it's non-empty (the first four bytes of a
Name field can't be zero). */
int is_valid_for_Name(const uint8_t *name, size_t name_count) {
  if (name_count == 0) {
    return 0;
  }
  for (size_t i = 0; i < name_count; i++) {
    if (name[i] == 0) {
      return 0;
    }
  }
  return 1;
}

uint32_t objfile_add_string(struct objfile *f,
                            const void *string, size_t string_count) {
  const uint8_t *ch = string;
  STATIC_CHECK(sizeof(uint8_t) == 1);
  for (size_t i = 0; i < string_count; i++) {
    CHECK(ch[i] != 0);
  }
  /* The docs say we want an offset into the strings table -- that
  offset includes the leading 4 size bytes, which means the minimum
  possible offset is 4. */
  uint32_t ret = uint32_add(size_to_uint32(f->strings.count), 4);
  databuf_append(&f->strings, string, string_count);
  databuf_append(&f->strings, "\0", 1);
  return ret;
}

void munge_to_Name(struct objfile *f,
                   const uint8_t *name,
                   size_t name_count,
                   union name_eight *Name_out) {
  CHECK(is_valid_for_Name(name, name_count));
  if (name_count <= 8) {
    STATIC_CHECK(sizeof(Name_out->ShortName) == 8);
    memset(Name_out->ShortName, 0, 8);
    memcpy(Name_out->ShortName, name, name_count);
  } else {
    uint32_t offset = objfile_add_string(f, name, name_count);
    Name_out->LongName.Zeroes = 0;
    Name_out->LongName.Offset = offset;
  }
}

uint32_t objfile_add_local_symbol(struct objfile *f,
                                  const uint8_t *name,
                                  size_t name_count,
                                  uint32_t Value,
                                  enum section section,
                                  enum is_static is_static) {
  uint32_t ret = size_to_uint32(f->symbol_table_count);
  union COFF_symbol_record u;
  munge_to_Name(f, name, name_count, &u.standard.Name);
  u.standard.Value = Value;
  u.standard.SectionNumber = section_to_SectionNumber(section);
  u.standard.Type = section == SECTION_TEXT ? kFunctionSymType : kNullSymType;
  /* At some point we might want to support... static functions or
  external or something, idk. */
  u.standard.StorageClass = is_static == IS_STATIC_NO
    ? IMAGE_SYM_CLASS_EXTERNAL
    : IMAGE_SYM_CLASS_STATIC;
  u.standard.NumberOfAuxSymbols = 0;
  SLICE_PUSH(f->symbol_table, f->symbol_table_count, f->symbol_table_limit, u);
  return ret;
}

static const uint16_t IMAGE_SYM_UNDEFINED = 0;

uint32_t objfile_add_remote_symbol(struct objfile *f,
                                   const uint8_t *name,
                                   size_t name_count,
                                   enum is_function is_function) {
  uint32_t ret = size_to_uint32(f->symbol_table_count);
  union COFF_symbol_record u;
  munge_to_Name(f, name, name_count, &u.standard.Name);
  u.standard.Value = 0;
  u.standard.SectionNumber = IMAGE_SYM_UNDEFINED;
  u.standard.Type = is_function == IS_FUNCTION_NO
    ? kNullSymType
    : kFunctionSymType;
  u.standard.StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
  u.standard.NumberOfAuxSymbols = 0;
  SLICE_PUSH(f->symbol_table, f->symbol_table_count, f->symbol_table_limit, u);
  return ret;
}

const uint32_t kNumSectionSymbolsPerSection = 2;

void win_append_section_symbols(
    struct databuf *d,
    struct objfile_section *s,
    uint16_t SectionNumber) {
  {
    union COFF_symbol_record u;
    STATIC_CHECK(sizeof(u.standard.Name.ShortName) == 8);
    memcpy(u.standard.Name.ShortName, s, 8);
    u.standard.Value = 0;
    u.standard.SectionNumber = SectionNumber;
    u.standard.Type = 0;
    u.standard.StorageClass = IMAGE_SYM_CLASS_STATIC;
    u.standard.NumberOfAuxSymbols = 1;
    databuf_append(d, &u, sizeof(u));
  }
  {
    union COFF_symbol_record u;
    u.aux_sectiondef.Length = size_to_uint32(objfile_section_raw_size(s));
    u.aux_sectiondef.NumberOfRelocations
      = objfile_section_small_relocations_count(s);
    u.aux_sectiondef.NumberOfLineNumbers = 0;
    u.aux_sectiondef.CheckSum = 0;
    u.aux_sectiondef.Number = 0;
    u.aux_sectiondef.Selection = 0;
    STATIC_CHECK(sizeof(u.aux_sectiondef.Unused) == 3);
    memset(u.aux_sectiondef.Unused, 0, 3);
    databuf_append(d, &u, sizeof(u));
  }
}

const uint32_t kNumberOfSections = 3;

/* TODO: Rename with "win". */
void objfile_flatten_append_all_section_symbols(struct databuf *d,
                                                struct objfile *f) {
  win_append_section_symbols(d, &f->data, 1);
  win_append_section_symbols(d, &f->rdata, 2);
  win_append_section_symbols(d, &f->text, 3);
}

uint32_t win_symbols_to_write(struct objfile *f) {
  return uint32_add(uint32_mul(kNumSectionSymbolsPerSection, kNumberOfSections),
                    size_to_uint32(f->symbol_table_count));
}

/* TODO: Rename with "win". */
void objfile_flatten(struct objfile *f, struct databuf **out) {
  STATIC_CHECK(LITTLE_ENDIAN);
  struct databuf *d = malloc(sizeof(*d));
  CHECK(d);
  databuf_init(d);

  /* Right now we have .data, .rdata, and .text sections. */
  const uint32_t end_of_section_headers
    = uint32_add(sizeof(struct COFF_Header),
                 uint32_mul(kNumberOfSections,
                            sizeof(struct Section_Header)));

  const uint32_t end_of_symbols
    = uint32_add(end_of_section_headers,
                 uint32_mul(win_symbols_to_write(f),
                            sizeof(union COFF_symbol_record)));
  const uint32_t strings_size
    = uint32_add(size_to_uint32(f->strings.count), 4);
  STATIC_CHECK(sizeof(strings_size) == 4);
  const uint32_t end_of_strings = uint32_add(end_of_symbols, strings_size);
  const uint32_t ceil_end_of_strings
    = uint32_ceil_aligned(end_of_strings, WIN_SECTION_ALIGNMENT);

  const uint32_t start_of_data_raw = ceil_end_of_strings;
  uint32_t start_of_data_relocs;
  uint32_t end_of_data_relocs;
  win_compute_section_dimensions(&f->data, start_of_data_raw,
                                 &start_of_data_relocs, &end_of_data_relocs);
  const uint32_t ceil_end_of_data_relocs
    = uint32_ceil_aligned(end_of_data_relocs, WIN_SECTION_ALIGNMENT);

  const uint32_t start_of_read_data_raw = ceil_end_of_data_relocs;
  uint32_t start_of_read_data_relocs;
  uint32_t end_of_read_data_relocs;
  win_compute_section_dimensions(&f->rdata, start_of_read_data_raw,
                                 &start_of_read_data_relocs,
                                 &end_of_read_data_relocs);
  const uint32_t ceil_end_of_read_data_relocs
    = uint32_ceil_aligned(end_of_read_data_relocs, WIN_SECTION_ALIGNMENT);

  const uint32_t start_of_text_raw = ceil_end_of_read_data_relocs;
  uint32_t start_of_text_relocs;
  uint32_t end_of_text_relocs;
  win_compute_section_dimensions(&f->text, start_of_text_raw,
                                 &start_of_text_relocs, &end_of_text_relocs);


  {
    struct COFF_Header h;
    STATIC_CHECK(sizeof(h) == COFF_Header_EXPECTED_SIZE);
    h.Machine = IMAGE_FILE_MACHINE_I386;
    h.NumberOfSections = kNumberOfSections;
    h.TimeDateStamp = kFakeTimeDateStamp;
    h.PointerToSymbolTable = end_of_section_headers;
    h.NumberOfSymbols = win_symbols_to_write(f);
    /* Should be zero for an object file. */
    h.SizeOfOptionalHeader = 0;
    h.Characteristics = real_file_characteristics();

    databuf_append(d, &h, sizeof(h));
  }

  /* Right now we've got 3 sections. */
  /* TODO: STATIC_CHECK? */
  CHECK(kNumberOfSections == 3);
  win_write_section_header(
      d, &f->data, start_of_data_raw,
      data_section_characteristics(f->data.max_requested_alignment));
  win_write_section_header(
      d, &f->rdata, start_of_read_data_raw,
      rdata_section_characteristics(f->rdata.max_requested_alignment));
  win_write_section_header(d, &f->text, start_of_text_raw,
                           text_section_characteristics());
  CHECK(d->count == end_of_section_headers);

  databuf_append(d, f->symbol_table,
                 size_mul(f->symbol_table_count,
                          sizeof(union COFF_symbol_record)));
  objfile_flatten_append_all_section_symbols(d, f);


  CHECK(d->count == end_of_symbols);
  STATIC_CHECK(sizeof(strings_size) == 4);
  databuf_append(d, &strings_size, sizeof(strings_size));
  databuf_append(d, f->strings.buf, f->strings.count);
  CHECK(d->count == end_of_strings);

  append_zeros_to_align(d, WIN_SECTION_ALIGNMENT);
  CHECK(d->count == ceil_end_of_strings);

  databuf_append(d, f->data.raw.buf, f->data.raw.count);
  append_zeros_to_align(d, 2);
  CHECK(d->count == start_of_data_relocs);

  win_append_relocs(d, f->data.relocs, f->data.relocs_count);
  CHECK(d->count == end_of_data_relocs);

  append_zeros_to_align(d, WIN_SECTION_ALIGNMENT);
  CHECK(d->count == ceil_end_of_data_relocs);

  databuf_append(d, f->rdata.raw.buf, f->rdata.raw.count);
  append_zeros_to_align(d, 2);
  CHECK(d->count == start_of_read_data_relocs);

  win_append_relocs(d, f->rdata.relocs, f->rdata.relocs_count);
  CHECK(d->count == end_of_read_data_relocs);

  append_zeros_to_align(d, WIN_SECTION_ALIGNMENT);
  CHECK(d->count == ceil_end_of_read_data_relocs);

  databuf_append(d, f->text.raw.buf, f->text.raw.count);
  append_zeros_to_align(d, 2);
  CHECK(d->count == start_of_text_relocs);

  win_append_relocs(d, f->text.relocs, f->text.relocs_count);
  CHECK(d->count == end_of_text_relocs);

  *out = d;
}

void objfile_section_append_raw(struct objfile_section *s,
                                const void *buf, size_t n) {
  databuf_append(&s->raw, buf, n);
}

void objfile_section_overwrite_raw(struct objfile_section *s,
                                   size_t offset,
                                   const void *buf, size_t n) {
  databuf_overwrite(&s->raw, offset, buf, n);
}

void objfile_section_align_dword(struct objfile_section *s) {
  append_zeros_to_align(&s->raw, 4);
  if (s->max_requested_alignment < 4) {
    s->max_requested_alignment = 4;
  }
}

void objfile_fillercode_align_double_quadword(struct objfile *f) {
  append_fillercode_to_align(&f->text.raw, 16);
}

void objfile_set_symbol_Value(struct objfile *f,
                              uint32_t SymbolTableIndex,
                              uint32_t Value) {
  CHECK(SymbolTableIndex < f->symbol_table_count);
  /* We should only be assigning this once, I think -- and we set it
  to zero before assigning it. */
  CHECK(f->symbol_table[SymbolTableIndex].standard.Value == 0);
  f->symbol_table[SymbolTableIndex].standard.Value = Value;
}

void objfile_section_append_32bit_reloc(struct objfile_section *s,
                                        uint32_t symbol_table_index,
                                        enum objfile_relocation_type type) {
  struct objfile_relocation reloc;
  reloc.virtual_address = s->raw.count;
  reloc.symbol_table_index = symbol_table_index;
  reloc.type = type;
  SLICE_PUSH(s->relocs, s->relocs_count, s->relocs_limit, reloc);
  uint32_t zero = 0;
  objfile_section_append_raw(s, &zero, sizeof(zero));
}

void objfile_section_append_dir32(struct objfile_section *s,
                                  uint32_t symbol_table_index) {
  objfile_section_append_32bit_reloc(s, symbol_table_index,
                                     OBJFILE_RELOCATION_TYPE_DIR32);
}

void objfile_section_append_dir32nb(struct objfile_section *s,
                                    uint32_t symbol_table_index) {
  objfile_section_append_32bit_reloc(s, symbol_table_index,
                                     OBJFILE_RELOCATION_TYPE_DIR32NB);
}

void objfile_section_append_rel32(struct objfile_section *s,
                                  uint32_t symbol_table_index) {
  objfile_section_append_32bit_reloc(s, symbol_table_index,
                                     OBJFILE_RELOCATION_TYPE_REL32);
}

int objfile_c_symbol_name(const void *name, size_t name_count,
                          void **c_name_out, size_t *c_name_count_out) {
  char *c_name;
  alloc_memcat("_", 1, name, name_count,
               &c_name, c_name_count_out);
  *c_name_out = c_name;
  return 1;
}

struct objfile_section *objfile_data(struct objfile *f) {
  return &f->data;
}
struct objfile_section *objfile_rdata(struct objfile *f) {
  return &f->rdata;
}
struct objfile_section *objfile_text(struct objfile *f) {
  return &f->text;
}

uint32_t objfile_section_size(struct objfile_section *s) {
  return size_to_uint32(s->raw.count);
}
