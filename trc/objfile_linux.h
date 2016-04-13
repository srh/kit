#ifndef OBJFILE_LINUX_H_
#define OBJFILE_LINUX_H_

struct databuf;
struct identmap;
struct objfile;

void linux32_flatten(struct identmap *im, struct objfile *f, struct databuf **out);

#endif /* OBJFILE_LINUX_H_ */
