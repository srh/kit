#ifndef OBJFILE_LINUX64_H_
#define OBJFILE_LINUX64_H_

struct databuf;
struct identmap;
struct objfile;

void linux64_flatten(struct identmap *im, struct objfile *f, struct databuf **out);

#endif /* OBJFILE_LINUX64_H_ */
