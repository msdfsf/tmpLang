#pragma once

#include "string.h"
#include <cstdint>

struct DataType {
    int size; // in bytes
    int rank;

    // String name;
};

extern DataType dataTypes[];

enum DataTypeEnum {
    DT_VOID = 0,
    DT_INT,
    DT_I8,
    DT_I16,
    DT_I32,
    DT_I64,
    DT_U8,
    DT_U16,
    DT_U32,
    DT_U64,
    DT_F32,
    DT_F64,
    DT_STRING,
    DT_POINTER,
    DT_ARRAY,
    DT_SLICE,
    DT_MULTIPLE_TYPES,
    DT_CUSTOM,
    DT_UNION,
    DT_ERROR,
    DT_MEMBER, // dont know about this one, represents the right side of member reference operator 'point . x'
    DT_ENUM,
    DT_FUNCTION, // FunctionPrototype
    DT_UNDEFINED,
    DT_COUNT,

    DT_BOOL = DT_U64,
};

inline int isInt(int x) {
    return x >= DT_INT && x <= DT_U64;
}

inline int isSignedInt(int x) {
    return x >= DT_INT && x <= DT_I64;
}

inline int isUnsignedInt(int x) {
    return x >= DT_U8 && x <= DT_U64;
}

inline int isFloat(int x) {
    return x >= DT_F32 && x <= DT_F64;
}
