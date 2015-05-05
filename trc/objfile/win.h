#ifndef KIT_OBJFILE_WIN_H_
#define KIT_OBJFILE_WIN_H_

#include <stddef.h>
#include <stdint.h>

struct databuf;
struct objfile_relocation;
struct objfile_section;

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

#endif /* KIT_OBJFILE_WIN_H_ */
