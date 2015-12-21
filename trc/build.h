#ifndef KIT_BUILD_H_
#define KIT_BUILD_H_

#include "typecheck.h"
#include "objfile/objfile.h"

int build_module(struct identmap *im, enum target_platform platform,
                 void *loader_ctx, module_loader *loader,
                 ident_value name);

#endif /* KIT_BUILD_H_ */
