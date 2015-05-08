#include "objfile/win.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "arith.h"
#include "objfile/structs.h"
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


/* TODO: Section_Header ought to be removed from the header. */
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

/* All section beginnings are aligned to 16 bytes.  I forget if
there's a reason. */
#define WIN_SECTION_ALIGNMENT 16


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

enum { kNumberOfSections = 3 };
enum { kNumSectionSymbolsPerSection = 2 };

void win_append_section_symbols(
    struct databuf *d,
    struct objfile_section *s,
    const char *name,
    uint16_t SectionNumber) {
  {
    struct COFF_symbol_standard_record standard;
    STATIC_CHECK(sizeof(standard.Name.ShortName) == 8);
    size_t name_len = strlen(name);
    CHECK(name_len <= 8);
    memset(standard.Name.ShortName, 0, 8);
    memcpy(standard.Name.ShortName, name, name_len);
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

void win_append_all_section_symbols(struct databuf *d,
                                    struct objfile *f) {
  win_append_section_symbols(d, &f->data, ".data", 1);
  win_append_section_symbols(d, &f->rdata, ".rdata", 2);
  win_append_section_symbols(d, &f->text, ".text", 3);
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
#define IMAGE_REL_I386_REL32 0x0014

void win_append_relocs(struct databuf *d, struct objfile_relocation *relocs,
                       size_t relocs_count) {
  static const uint16_t win_Type[] = {
    [OBJFILE_RELOCATION_TYPE_DIR32] = IMAGE_REL_I386_DIR32,
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
    struct databuf *d, struct objfile_section *s, const char *name,
    uint32_t start_of_raw, uint32_t Characteristics) {
  uint32_t PointerToRelocations;
  uint32_t pointer_to_end;
  win_compute_section_dimensions(s, start_of_raw,
                                 &PointerToRelocations, &pointer_to_end);

  struct Section_Header h;
  STATIC_CHECK(sizeof(h) == Section_Header_EXPECTED_SIZE);
  STATIC_CHECK(sizeof(h.Name) == 8 && sizeof(h.Name[0]) == 1);
  size_t name_len = strlen(name);
  CHECK(name_len <= 8);
  memset(h.Name, 0, 8);
  memcpy(h.Name, name, name_len);
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

void win_write_symbols_and_strings(struct identmap *im,
                                   struct objfile *f,
                                   struct databuf **symbols_out,
                                   struct databuf **strings_out) {
  struct databuf *symbols = malloc(sizeof(*symbols));
  CHECK(symbols);
  databuf_init(symbols);
  struct databuf *strings = malloc(sizeof(*strings));
  CHECK(strings);
  databuf_init(strings);

  /* We start off with a 32-bit size field. */
  databuf_append(strings, "\0\0\0\0", 4);

  struct objfile_symbol_record *symbol_table = f->symbol_table;

  for (size_t i = 0, e = f->symbol_table_count; i < e; i++) {
    struct COFF_symbol_standard_record standard;
    ident_value name = symbol_table[i].name;
    const void *name_buf;
    size_t name_count;
    identmap_lookup(im, name, &name_buf, &name_count);

    CHECK(is_valid_for_Name(name_buf, name_count));
    if (name_count <= 8) {
      STATIC_CHECK(sizeof(standard.Name.ShortName) == 8);
      memset(standard.Name.ShortName, 0, 8);
      memcpy(standard.Name.ShortName, name_buf, name_count);
    } else {
      /* The offset includes the leading 4 bytes -- the minimimum
         possible offset is 4. */
      uint32_t offset = strtab_add(strings, name_buf, name_count);
      standard.Name.LongName.Zeroes = 0;
      standard.Name.LongName.Offset = offset;
    }

    standard.Value = symbol_table[i].value;
    STATIC_CHECK((int)OBJFILE_SYMBOL_SECTION_UNDEFINED == (int)IMAGE_SYM_UNDEFINED);
    standard.SectionNumber = symbol_table[i].section;
    standard.Type = symbol_table[i].is_function == IS_FUNCTION_YES ?
      kFunctionSymType : kNullSymType;
    standard.StorageClass = symbol_table[i].is_static == IS_STATIC_YES ?
      IMAGE_SYM_CLASS_STATIC : IMAGE_SYM_CLASS_EXTERNAL;
    standard.NumberOfAuxSymbols = 0;
    databuf_append(symbols, &standard, sizeof(standard));
  }

  win_append_all_section_symbols(symbols, f);

  const uint32_t strings_size = size_to_uint32(strings->count);
  STATIC_CHECK(sizeof(strings_size) == 4);
  databuf_overwrite(strings, 0, &strings_size, 4);

  *symbols_out = symbols;
  *strings_out = strings;
}

void win_flatten(struct identmap *im, struct objfile *f, struct databuf **out) {
  STATIC_CHECK(LITTLE_ENDIAN);
  struct databuf *d = malloc(sizeof(*d));
  CHECK(d);
  databuf_init(d);

  /* Right now we have .data, .rdata, and .text sections. */
  const uint32_t end_of_section_headers
    = uint32_add(sizeof(struct COFF_Header),
                 uint32_mul(kNumberOfSections,
                            sizeof(struct Section_Header)));

  struct databuf *symbols;
  struct databuf *strings;
  win_write_symbols_and_strings(im, f, &symbols, &strings);

  const uint32_t end_of_symbols
    = uint32_add(end_of_section_headers, symbols->count);
  const uint32_t end_of_strings = uint32_add(end_of_symbols, strings->count);
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
  STATIC_CHECK(kNumberOfSections == 3);
  win_write_section_header(
      d, &f->data, ".data", start_of_data_raw,
      data_section_characteristics(f->data.max_requested_alignment));
  win_write_section_header(
      d, &f->rdata, ".rdata", start_of_read_data_raw,
      rdata_section_characteristics(f->rdata.max_requested_alignment));
  win_write_section_header(d, &f->text, ".text", start_of_text_raw,
                           text_section_characteristics());
  CHECK(d->count == end_of_section_headers);

  databuf_append(d, symbols->buf, symbols->count);
  databuf_destroy(symbols);
  free(symbols);

  CHECK(d->count == end_of_symbols);

  databuf_append(d, strings->buf, strings->count);
  databuf_destroy(strings);
  free(strings);
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
