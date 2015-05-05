#ifndef KIT_OBJFILE_WIN_H_
#define KIT_OBJFILE_WIN_H_

#include <stddef.h>
#include <stdint.h>

#include "util.h"

struct databuf;
struct objfile_relocation;
struct objfile_section;

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


/* TODO: Can WIN_SECTION_ALIGNMENT (or other stuff here) be removed from the header file? */

/* All section beginnings are aligned to 16 bytes.  I forget if
there's a reason. */
#define WIN_SECTION_ALIGNMENT 16

void win_compute_section_dimensions(struct objfile_section *s,
                                    uint32_t start_of_raw,
                                    uint32_t *PointerToRelocations_out,
                                    uint32_t *pointer_to_end_out);
void win_append_relocs(struct databuf *d, struct objfile_relocation *relocs,
                       size_t relocs_count);

void win_write_section_header(
    struct databuf *d, struct objfile_section *s,
    uint32_t start_of_raw, uint32_t Characteristics);

#endif /* KIT_OBJFILE_WIN_H_ */
