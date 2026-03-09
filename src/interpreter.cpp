// Habitat of 'global' vm related stuff
//

#include "interpreter.h"
#include "data_types.h"
#include "operators.h"
#include <cstdint>


namespace Interpreter {

    vmword encodeVecDescriptor(const VecDescriptor desc) {
        return  (
              (((uint64_t) desc.dtype)     << DE_DTYPE_SHIFT)
            | (((uint64_t) desc.oper)      << DE_OPER_SHIFT)
            | (((uint64_t) desc.srcDtype)  << DE_SRC_DTYPE_SHIFT)
            | (((uint64_t) desc.flags)     << DE_FLAGS_SHIFT)
        );
    }

    VecDescriptor decodeVecDescriptor(const vmword word) {
        return {
            .dtype    = (DataTypeEnum)
                ((word & DE_DTYPE_MASK) >> DE_DTYPE_SHIFT),
            .oper     = (OperatorEnum)
                ((word & DE_OPER_MASK) >> DE_OPER_SHIFT),
            .srcDtype = (DataTypeEnum)
                ((word & DE_SRC_DTYPE_MASK) >> DE_SRC_DTYPE_SHIFT),
            .flags    = (uint32_t)
                ((word & DE_FLAGS_MASK) >> DE_FLAGS_SHIFT)
        };
    }

}
