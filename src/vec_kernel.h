#pragma once
#include "data_types.h"
#include "operators.h"
#include "syntax.h"
#include <cstdint>
#include <stdint.h>



typedef void (*VecFunctionBinary)(void* out, void* a, void* b, const int len);
typedef void (*VecFunctionScalar)(void* out, void* a, uint64_t s, const int len);
typedef void (*VecFunctionUnary) (void* out, void* a, const int len);
typedef void (*VecFunctionCast)  (void* out, void* a, const int len);
typedef void (*VecFunctionFill)  (void* out, uint64_t val, const int len);

extern VecFunctionBinary vecDispatchBinary[];
extern VecFunctionScalar vecDispatchScalarR[];
extern VecFunctionScalar vecDispatchScalarL[];
extern VecFunctionUnary  vecDispatchUnary[];
extern VecFunctionCast   vecDispatchCast[];
extern VecFunctionFill   vecDispatchFill[];

inline VecFunctionBinary vecGetBinary(Type::Kind dtype, OperatorEnum oper) {
    const int col = (oper - OP_BINARY_BEGIN) * Type::DT_F64;
    const int row = dtype - Type::DT_I8;
    return vecDispatchBinary[col + row];
}

inline VecFunctionScalar vecGetScalarR(Type::Kind dtype, OperatorEnum oper) {
    const int col = (oper - OP_BINARY_BEGIN) * Type::DT_F64;
    const int row = dtype - Type::DT_I8;
    return vecDispatchScalarR[col + row];
}

inline VecFunctionScalar vecGetScalarL(Type::Kind dtype, OperatorEnum oper) {
    const int col = (oper - OP_BINARY_BEGIN) * Type::DT_F64;
    const int row = dtype - Type::DT_I8;
    return vecDispatchScalarL[col + row];
}

inline VecFunctionUnary vecGetUnary(Type::Kind dtype, OperatorEnum oper) {
    const int col = (oper - OP_UNARY_BEGIN) * Type::DT_F64;
    const int row = dtype - Type::DT_I8;
    return vecDispatchUnary[col + row];
}

inline VecFunctionCast vecGetCast(Type::Kind dest, Type::Kind src) {
    // Here we need to support whole dtype spectrum
    const int col = (dest - Type::DT_I8) * Type::DT_F64;
    const int row = src - Type::DT_I8;
    return vecDispatchCast[col + row];
}

inline VecFunctionFill vecGetFill(Type::Kind dtype) {
    const int idx = dtype - Type::DT_I32;
    return vecDispatchFill[idx];
}
