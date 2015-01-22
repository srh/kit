#ifndef KIRA_X86_H_
#define KIRA_X86_H_

struct checkstate;
struct funcgraph;
struct objfile;

int gen_x86_function(struct checkstate *cs, struct objfile *f,
                     struct funcgraph *g);

#endif /* KIRA_X86_H_ */

