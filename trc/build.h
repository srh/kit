#ifndef KIT_BUILD_H_
#define KIT_BUILD_H_

#include "typecheck.h"

int build_module(struct identmap *im, module_loader *loader,
                 ident_value name);

#endif /* KIT_BUILD_H_ */
