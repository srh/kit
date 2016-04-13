#ifndef KIT_OBJFILE_WIN_H_
#define KIT_OBJFILE_WIN_H_

struct databuf;
struct identmap;
struct objfile;

void win_flatten(struct identmap *im, struct objfile *f, struct databuf **out);

#endif /* KIT_OBJFILE_WIN_H_ */
