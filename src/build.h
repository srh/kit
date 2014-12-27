#ifndef KIRA_BUILD_H_
#define KIRA_BUILD_H_

#include "typecheck.h"

int build_module(struct identmap *im, module_loader *loader,
                 ident_value name);

#endif /* KIRA_BUILD_H_ */
