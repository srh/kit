#include "objfile_osx.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "databuf.h"
#include "objfile_structs.h"
#include "util.h"

/* Beware: The endianness of these fields can depend on the endianness
of the target platform.  This code assumes the target platform is
little-endian.  Which, in the case of 32-bit OS X x86, it is. */

PACK_PUSH
struct mach32_header {
  struct le_u32 magic;
  struct le_u32 cputype;
  struct le_u32 cpusubtype;
  struct le_u32 filetype;
  struct le_u32 ncmds;
  struct le_u32 sizeofcmds;
  struct le_u32 flags;
} PACK_ATTRIBUTE;
PACK_POP

PACK_PUSH
struct mach32_segment_command {
  struct le_u32 cmd;
  struct le_u32 cmdsize;
  uint8_t segname[16];
  struct le_u32 vmaddr;
  struct le_u32 vmsize;
  struct le_u32 fileoff;
  struct le_u32 filesize;
  struct le_u32 maxprot;
  struct le_u32 initprot;
  struct le_u32 nsects;
  struct le_u32 flags;
} PACK_ATTRIBUTE;
PACK_POP

PACK_PUSH
struct mach32_section {
  uint8_t sectname[16];
  uint8_t segname[16];
  struct le_u32 addr;
  struct le_u32 size;
  struct le_u32 offset;
  struct le_u32 align;
  struct le_u32 reloff;
  struct le_u32 nreloc;
  struct le_u32 flags;
  struct le_u32 reserved1;
  struct le_u32 reserved2;
} PACK_ATTRIBUTE;
PACK_POP

PACK_PUSH
struct mach32_symtab {
  struct le_u32 cmd;
  struct le_u32 cmdsize;
  struct le_u32 symoff;
  struct le_u32 nsyms;
  struct le_u32 stroff;
  struct le_u32 strsize;
} PACK_ATTRIBUTE;
PACK_POP

PACK_PUSH
struct mach32_relocation_info {
  struct le_i32 r_address;
  struct le_u32 r_mask;
} PACK_ATTRIBUTE;
PACK_POP

PACK_PUSH
struct mach32_scattered_relocation_info {
  struct le_u32 r_mask;
  struct le_i32 r_value;
} PACK_ATTRIBUTE;
PACK_POP

PACK_PUSH
struct mach32_nlist {
  struct le_u32 n_strx;
  uint8_t n_type;
  uint8_t n_sect;
  struct le_i16 n_desc;
  struct le_u32 n_value;
} PACK_ATTRIBUTE;
PACK_POP

#define MACH32_HEADER_MAGIC 0xFEEDFACElu
#define MACH32_SECTION_TEXT_FLAGS 0x80000400lu

enum {
  MACH32_HEADER_I386_CPUTYPE = 7,
  MACH32_HEADER_I386_CPUSUBTYPE = 3,
  MACH32_HEADER_OBJFILE_FILETYPE = 1,

  MACH32_SEGMENT_COMMAND = 1,
  MACH32_SYMTAB_COMMAND = 2,

  MACH32_SECTION_CONST_FLAGS = 0,
  MACH32_SECTION_DATA_FLAGS = 0,

  /* i.e. these values are sizeof(struct mach32_relocation_info). */
  MACH32_RELOC_ALIGNMENT = 8,
  MACH32_RELOC_SIZE = 8,

  /* mach32_nlist is 12 bytes. */
  MACH32_SYMTAB_ALIGNMENT = 4,

  /* More file format constants. */
  MACH32_RELOCATION_SECTDIF = 2,
  MACH32_RELOCATION_PAIR = 1,
};

void set_fixstr(uint8_t *dest, const char *src, size_t n) {
  /* I think this is strncpy, but I don't feel like verifying that it
  portably fills with zeros. */
  int hitzero = 0;
  for (size_t i = 0; i < n; i++) {
    if (hitzero) {
      dest[i] = 0;
    } else {
      hitzero = (src[i] == 0);
      dest[i] = src[i];
    }
  }
}

struct le_u32 mach32_r_mask(uint32_t r_symbolnum, uint32_t r_pcrel,
                            uint32_t r_length, uint32_t r_extern,
                            uint32_t r_type) {
  /* We have these in some order:
  r_symbolnum 24
  r_pcrel 1
  r_length 2
  r_extern 1
  r_type 4 */

  /* This might not be correct for BIG ENDIAN mach-o files. */
  CHECK(r_symbolnum < (1ul << 24));
  CHECK(r_pcrel < (1u << 1));
  CHECK(r_length < (1u << 2));
  CHECK(r_extern < (1u << 1));
  CHECK(r_type < (1u << 4));
  uint32_t x = r_symbolnum | (r_pcrel << 24) | (r_length << 25)
    | (r_extern << 27) | (r_type << 28);
  return to_le_u32(x);
}

struct le_u32 mach32_r_scattered_mask(uint32_t r_address,
                                      uint32_t r_type,
                                      uint32_t r_length,
                                      uint32_t r_pcrel,
                                      uint32_t r_scattered) {
  /* We have these in some order:
  r_address 24
  r_type 4
  r_length 2
  r_pcrel 1
  r_scattered 1
  */
  CHECK(r_address < (1ul << 24));
  CHECK(r_type < (1u << 4));
  CHECK(r_length < (1u << 2));
  CHECK(r_pcrel < (1u << 1));
  CHECK(r_scattered == 1);
  uint32_t x = r_address | (r_type << 24) | (r_length << 28)
    | (r_pcrel << 30) | (r_scattered << 31);
  return to_le_u32(x);
}

struct section_addrs {
  uint32_t data_addr;
  uint32_t rdata_addr;
  uint32_t text_addr;
};

uint32_t osx32_adjusted_addr(struct section_addrs addrs,
                             enum objfile_symbol_section section,
                             uint32_t value) {
  switch (section) {
  case OBJFILE_SYMBOL_SECTION_UNDEFINED:
    return value;
  case OBJFILE_SYMBOL_SECTION_DATA:
    return uint32_add(addrs.data_addr, value);
  case OBJFILE_SYMBOL_SECTION_RDATA:
    return uint32_add(addrs.rdata_addr, value);
  case OBJFILE_SYMBOL_SECTION_TEXT:
    return uint32_add(addrs.text_addr, value);
  default:
    UNREACHABLE();
  }
}

uint8_t osx32_sect(enum objfile_symbol_section sect);

void osx32_append_relocations_and_mutate_output(
    struct databuf *d, struct objfile *f,
    struct objfile_section *s,
    struct section_addrs addrs,
    enum section sect,
    uint32_t segment_file_offset) {
  for (size_t i = 0, e = s->relocs_count; i < e; i++) {
    struct objfile_relocation *reloc = &s->relocs[i];
    struct objfile_symbol_record *symbol
      = &f->symbol_table[uint32_to_size(reloc->symbol_table_index.value)];
    switch (reloc->type) {
    case OBJFILE_RELOCATION_TYPE_DIR32:
      /* Globals use call/pop to get eip. */
      CRASH("osx32 only uses rel32 relocations");
    case OBJFILE_RELOCATION_TYPE_REL32: {
      struct mach32_relocation_info info;
      const uint32_t r_address
        = osx32_adjusted_addr(addrs, (enum objfile_symbol_section)sect,
                              reloc->virtual_address);
      info.r_address = to_le_i32(uint32_to_int32(r_address));
      uint32_t r_pcrel = 1;
      /* 1 << 2 */
      uint32_t r_length = 2;
      /* Everything is extern.  I _think_ this would be 0 if the value
      was an inline, local reference (into this or section or another
      section), to something that didn't have a symbol, that needed to
      be relocated. */
      uint32_t r_extern = (symbol->section == OBJFILE_SYMBOL_SECTION_UNDEFINED);
      uint32_t r_symbolnum;
      if (r_extern) {
        r_symbolnum = reloc->symbol_table_index.value;
      } else {
        r_symbolnum = osx32_sect(symbol->section);
      }
      uint32_t r_type = 0;
      info.r_mask = mach32_r_mask(r_symbolnum, r_pcrel, r_length,
                                  r_extern, r_type);
      databuf_append(d, &info, sizeof(info));

      /* We even overwrite on external symbols, which mimics behavior
      but might be reproducing an irrelevant implementation detail of
      clang. */
      int32_t target_offset
        = uint32_to_int32(osx32_adjusted_addr(addrs, symbol->section, symbol->value));
      uint32_t displacement = int32_sub(target_offset,
                                        uint32_to_int32(uint32_add(r_address, 4)));
      struct le_u32 le_displacement = to_le_u32(displacement);
      databuf_overwrite(d, uint32_to_size(uint32_add(segment_file_offset, r_address)),
                        &le_displacement, sizeof(le_displacement));
    } break;
    case OBJFILE_RELOCATION_TYPE_DIFF32: {
      /* Notes for scattered diff32 reloc
      - I don't know wtf "scattered" means or is for
      - looks like first pair is address=relocated, value=target segment offset
      - second pair is address=0? value=subtractive offset
      */
      const uint32_t virtual_address
        = osx32_adjusted_addr(addrs, (enum objfile_symbol_section)sect,
                              reloc->virtual_address);
      const int32_t target_offset
        = uint32_to_int32(osx32_adjusted_addr(addrs, symbol->section, symbol->value));
      const int32_t subtracted_offset
        = uint32_to_int32(osx32_adjusted_addr(addrs, (enum objfile_symbol_section)sect,
                                              reloc->subtracted_offset));

      {
        struct mach32_scattered_relocation_info info1;
        const uint32_t r_address = virtual_address;
        const uint32_t r_type = MACH32_RELOCATION_SECTDIF;
        const uint32_t r_length = 2;  /* 1 << 2 */
        const uint32_t r_pcrel = 0;
        const uint32_t r_scattered = 1;
        info1.r_mask = mach32_r_scattered_mask(r_address, r_type, r_length,
                                               r_pcrel, r_scattered);

        info1.r_value = to_le_i32(target_offset);
        databuf_append(d, &info1, sizeof(info1));
      }
      {
        struct mach32_scattered_relocation_info info2;
        /* Zero.  This is the PAIR value. */
        const uint32_t r_address = 0;
        const uint32_t r_type = MACH32_RELOCATION_PAIR;
        const uint32_t r_length = 2;  /* 1 << 2 */
        const uint32_t r_pcrel = 0;
        const uint32_t r_scattered = 1;
        info2.r_mask = mach32_r_scattered_mask(r_address, r_type, r_length,
                                               r_pcrel, r_scattered);
        const int32_t r_value = subtracted_offset;
        info2.r_value = to_le_i32(uint32_to_int32(r_value));
        databuf_append(d, &info2, sizeof(info2));
      }

      int32_t displacement = int32_sub(target_offset, subtracted_offset);
      struct le_i32 le_displacement = to_le_i32(displacement);
      databuf_overwrite(d, uint32_to_size(uint32_add(segment_file_offset, virtual_address)),
                        &le_displacement, sizeof(le_displacement));
    } break;
    default:
      UNREACHABLE();
    }
  }
}

enum {
  kOSX32DataSectionNumber = 3,
  kOSX32RdataSectionNumber = 2,
  kOSX32TextSectionNumber = 1,
};

uint8_t osx32_sect(enum objfile_symbol_section sect) {
  switch (sect) {
  case OBJFILE_SYMBOL_SECTION_UNDEFINED:
    /* 0 means not in a section. */
    return 0;
  case OBJFILE_SYMBOL_SECTION_DATA:
    return kOSX32DataSectionNumber;
  case OBJFILE_SYMBOL_SECTION_RDATA:
    return kOSX32RdataSectionNumber;
  case OBJFILE_SYMBOL_SECTION_TEXT:
    return kOSX32TextSectionNumber;
  default:
    UNREACHABLE();
  }
}


void osx32_write_symbols_and_strings(struct identmap *im, struct objfile *f,
                                     struct section_addrs addrs,
                                     struct databuf **symbols_out,
                                     struct databuf **strings_out) {
  struct databuf *symbols = malloc(sizeof(*symbols));
  CHECK(symbols);
  databuf_init(symbols);
  struct databuf *strings = malloc(sizeof(*strings));
  CHECK(strings);
  databuf_init(strings);

  for (size_t i = 0, e = f->symbol_table_count; i < e; i++) {
    struct objfile_symbol_record *rec = &f->symbol_table[i];
    struct mach32_nlist el;
    const void *name_buf;
    size_t name_count;
    identmap_lookup(im, rec->name, &name_buf, &name_count);

    uint32_t offset = strtab_add(strings, name_buf, name_count);

    el.n_strx = to_le_u32(offset);
    /* We set the external bit, not the "private external" bit.  I
    don't know what the "private external" bit is. */
    el.n_type =
      (rec->section == OBJFILE_SYMBOL_SECTION_UNDEFINED ? 0 : 0xe)
      | (rec->is_static == IS_STATIC_YES ? 0 : 1);

    el.n_sect = osx32_sect(rec->section);
    el.n_desc = to_le_i16(0);
    uint32_t n_value = osx32_adjusted_addr(addrs, rec->section, rec->value);
    el.n_value = to_le_u32(n_value);

    databuf_append(symbols, &el, sizeof(el));
  }

  *symbols_out = symbols;
  *strings_out = strings;
}

void osx32_flatten(struct identmap *im, struct objfile *f, struct databuf **out) {
  struct databuf *d = malloc(sizeof(*d));
  CHECK(d);
  databuf_init(d);

  const uint32_t nsects = 3; /* text, data, rdata -- see below */
  const uint32_t sizeof_segment_cmd = sizeof(struct mach32_segment_command)
    + nsects * sizeof(struct mach32_section);
  const uint32_t sizeof_symtab_cmd = sizeof(struct mach32_symtab);

  const uint32_t ncmds = 2;
  const uint32_t sizeofcmds = sizeof_segment_cmd + sizeof_symtab_cmd;
  const uint32_t end_of_cmds = sizeof(struct mach32_header) + sizeofcmds;

  {
    struct mach32_header h;
    h.magic = to_le_u32(MACH32_HEADER_MAGIC);
    h.cputype = to_le_u32(MACH32_HEADER_I386_CPUTYPE);
    h.cpusubtype = to_le_u32(MACH32_HEADER_I386_CPUSUBTYPE);
    h.filetype = to_le_u32(MACH32_HEADER_OBJFILE_FILETYPE);
    h.ncmds = to_le_u32(ncmds);
    h.sizeofcmds = to_le_u32(sizeofcmds);
    /* We use 0 for flags.  Clang outputs 0x2000
    (MH_SUBSECTIONS_VIA_SYMBOLS) which serves us (making an s1
    compiler) no purpose. */
    h.flags = to_le_u32(0);

    databuf_append(d, &h, sizeof(h));
  }

  /* The [addr, ceil_aligned(addr + size, align)) segments seem to
  connect.  (That's what I observe in other mach-o files.) */
  const uint32_t text_addr = 0;
  const uint32_t text_size = size_to_uint32(f->text.raw.count);
  const uint32_t text_align = uint32_max(16, f->text.max_requested_alignment);
  const uint32_t text_ceil_size = uint32_ceil_aligned(text_size, text_align);

  const uint32_t rdata_align = uint32_max(16, f->rdata.max_requested_alignment);
  const uint32_t rdata_addr = uint32_ceil_aligned(uint32_add(text_addr,
                                                             text_ceil_size),
                                                  rdata_align);
  const uint32_t rdata_size = size_to_uint32(f->rdata.raw.count);
  const uint32_t rdata_ceil_size = uint32_ceil_aligned(rdata_size, rdata_align);

  const uint32_t data_align = uint32_max(16, f->data.max_requested_alignment);
  const uint32_t data_addr = uint32_ceil_aligned(uint32_add(rdata_addr,
                                                            rdata_ceil_size),
                                                 data_align);
  const uint32_t data_size = size_to_uint32(f->data.raw.count);
  const uint32_t data_ceil_size = uint32_ceil_aligned(data_size, data_align);

  struct section_addrs addrs = { 0, 0, 0 };
  addrs.data_addr = data_addr;
  addrs.rdata_addr = rdata_addr;
  addrs.text_addr = text_addr;

  const uint32_t segment_filealign
    = uint32_max(text_align, uint32_max(rdata_align, data_align));
  const uint32_t vmsize = uint32_ceil_aligned(uint32_add(data_addr,
                                                         data_ceil_size),
                                              segment_filealign);

  const uint32_t segment_fileoff = uint32_ceil_aligned(end_of_cmds, segment_filealign);

  const uint32_t segment_end_fileoff = uint32_add(segment_fileoff, vmsize);
  const uint32_t text_reloff = uint32_ceil_aligned(segment_end_fileoff,
                                                   MACH32_RELOC_ALIGNMENT);
  /* OS X diff32 relocs use two pairs, so we count them twice (with
  the 3 size_add's you see here). */
  const uint32_t text_nreloc = size_to_uint32(size_add(f->text.relocs_count,
                                                       f->text.diff32_count));

  STATIC_CHECK(MACH32_RELOC_SIZE == sizeof(struct mach32_relocation_info));
  STATIC_CHECK(MACH32_RELOC_SIZE == sizeof(struct mach32_scattered_relocation_info));
  const uint32_t rdata_reloff
    = uint32_add(text_reloff, uint32_mul(text_nreloc, MACH32_RELOC_SIZE));
  const uint32_t rdata_nreloc = size_to_uint32(size_add(f->rdata.relocs_count,
                                                        f->rdata.diff32_count));
  const uint32_t data_reloff
    = uint32_add(rdata_reloff, uint32_mul(rdata_nreloc, MACH32_RELOC_SIZE));
  const uint32_t data_nreloc = size_to_uint32(size_add(f->data.relocs_count,
                                                       f->data.diff32_count));
  const uint32_t end_of_relocs
    = uint32_add(data_reloff, uint32_mul(data_nreloc, MACH32_RELOC_SIZE));

  const uint32_t symoff = uint32_ceil_aligned(end_of_relocs,
                                              MACH32_SYMTAB_ALIGNMENT);

  struct databuf *symbols;
  struct databuf *strings;
  osx32_write_symbols_and_strings(im, f, addrs, &symbols, &strings);
  const uint32_t nsyms = size_to_uint32(f->symbol_table_count);

  const uint32_t stroff
    = uint32_add(symoff, uint32_mul(nsyms, sizeof(struct mach32_nlist)));
  const uint32_t strsize = size_to_uint32(strings->count);
  const uint32_t file_end_offset = uint32_add(stroff, strsize);

  /* 1st load command */
  {
    struct mach32_segment_command sc;
    sc.cmd = to_le_u32(MACH32_SEGMENT_COMMAND);
    sc.cmdsize = to_le_u32(sizeof_segment_cmd);
    set_fixstr(sc.segname, "", sizeof(sc.segname));
    sc.vmaddr = to_le_u32(0);
    sc.vmsize = to_le_u32(vmsize);
    sc.fileoff = to_le_u32(segment_fileoff);
    /* Consistent with observed behavior, and totally sensible. */
    sc.filesize = to_le_u32(vmsize);
    /* Segment's maximum and initial virtual memory protection?  I see
    7 (r/w/x) being used. */
    sc.maxprot = to_le_u32(7);
    sc.initprot = to_le_u32(7);
    sc.nsects = to_le_u32(nsects);
    /* No interesting flags. */
    sc.flags = to_le_u32(0);

    databuf_append(d, &sc, sizeof(sc));
  }

  const uint32_t text_offset = uint32_add(segment_fileoff, text_addr);
  /* 1st section */
  {
    STATIC_CHECK(kOSX32TextSectionNumber == 1);
    struct mach32_section se;
    set_fixstr(se.sectname, "__text", sizeof(se.sectname));
    set_fixstr(se.segname, "__TEXT", sizeof(se.segname));
    se.addr = to_le_u32(text_addr);
    se.size = to_le_u32(text_ceil_size);
    se.offset = to_le_u32(text_offset);
    se.align = to_le_u32(text_align);
    se.reloff = to_le_u32(text_reloff);
    se.nreloc = to_le_u32(text_nreloc);
    se.flags = to_le_u32(MACH32_SECTION_TEXT_FLAGS);
    se.reserved1 = to_le_u32(0);
    se.reserved2 = to_le_u32(0);

    databuf_append(d, &se, sizeof(se));
  }

  const uint32_t rdata_offset = uint32_add(segment_fileoff, rdata_addr);
  /* 2nd section */
  {
    STATIC_CHECK(kOSX32RdataSectionNumber == 2);
    struct mach32_section se;
    set_fixstr(se.sectname, "__const", sizeof(se.sectname));
    set_fixstr(se.segname, "__TEXT", sizeof(se.segname));
    se.addr = to_le_u32(rdata_addr);
    se.size = to_le_u32(rdata_ceil_size);
    se.offset = to_le_u32(rdata_offset);
    se.align = to_le_u32(rdata_align);
    se.reloff = to_le_u32(rdata_reloff);
    se.nreloc = to_le_u32(rdata_nreloc);
    se.flags = to_le_u32(MACH32_SECTION_CONST_FLAGS);
    se.reserved1 = to_le_u32(0);
    se.reserved2 = to_le_u32(0);

    databuf_append(d, &se, sizeof(se));
  }

  const uint32_t data_offset = uint32_add(segment_fileoff, data_addr);
  /* 3rd section */
  {
    STATIC_CHECK(kOSX32DataSectionNumber == 3);
    struct mach32_section se;
    set_fixstr(se.sectname, "__data", sizeof(se.sectname));
    set_fixstr(se.segname, "__DATA", sizeof(se.segname));
    se.addr = to_le_u32(data_addr);
    se.size = to_le_u32(data_ceil_size);
    se.offset = to_le_u32(data_offset);
    se.align = to_le_u32(data_align);
    se.reloff = to_le_u32(data_reloff);
    se.nreloc = to_le_u32(data_nreloc);
    se.flags = to_le_u32(MACH32_SECTION_DATA_FLAGS);
    se.reserved1 = to_le_u32(0);
    se.reserved2 = to_le_u32(0);

    databuf_append(d, &se, sizeof(se));
  }

  /* 2nd load command */
  {
    struct mach32_symtab st;
    st.cmd = to_le_u32(MACH32_SYMTAB_COMMAND);
    CHECK(sizeof_symtab_cmd == sizeof(struct mach32_symtab));
    st.cmdsize = to_le_u32(sizeof_symtab_cmd);
    st.symoff = to_le_u32(symoff);
    st.nsyms = to_le_u32(nsyms);
    st.stroff = to_le_u32(stroff);
    st.strsize = to_le_u32(strsize);

    databuf_append(d, &st, sizeof(st));
  }

  append_zeros_to_align(d, segment_filealign);
  CHECK(d->count == segment_fileoff);
  CHECK(d->count == text_offset);
  databuf_append(d, f->text.raw.buf, f->text.raw.count);
  /* TODO: Assert that f->text is already to this alignment (pad after
  funcs in codegen). */
  /* TODO: This should be 0xCC?  No, do the assertion. */
  append_zeros_to_align(d, text_align);
  append_zeros_to_align(d, rdata_align);

  CHECK(d->count == rdata_offset);
  databuf_append(d, f->rdata.raw.buf, f->rdata.raw.count);
  append_zeros_to_align(d, rdata_align);
  append_zeros_to_align(d, data_align);

  CHECK(d->count == data_offset);
  databuf_append(d, f->data.raw.buf, f->data.raw.count);
  append_zeros_to_align(d, segment_filealign);

  CHECK(d->count == segment_end_fileoff);
  append_zeros_to_align(d, MACH32_RELOC_ALIGNMENT);

  CHECK(d->count == text_reloff);
  osx32_append_relocations_and_mutate_output(d, f, &f->text, addrs, SECTION_TEXT, uint32_add(segment_fileoff, text_addr));
  CHECK(d->count == rdata_reloff);
  osx32_append_relocations_and_mutate_output(d, f, &f->rdata, addrs, SECTION_RDATA, uint32_add(segment_fileoff, rdata_addr));
  CHECK(d->count == data_reloff);
  osx32_append_relocations_and_mutate_output(d, f, &f->data, addrs, SECTION_DATA, uint32_add(segment_fileoff, data_addr));
  CHECK(d->count == end_of_relocs);

  append_zeros_to_align(d, MACH32_SYMTAB_ALIGNMENT);
  CHECK(d->count == symoff);

  databuf_append(d, symbols->buf, symbols->count);
  databuf_destroy(symbols);
  free(symbols);
  CHECK(d->count == stroff);
  databuf_append(d, strings->buf, strings->count);
  databuf_destroy(strings);
  free(strings);
  CHECK(d->count == file_end_offset);

  *out = d;
}
