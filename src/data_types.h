#pragma once

#include "string.h"
#include <cstdint>



enum DataTypeEnum : uint8_t {
    DT_VOID = 0,
    
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
    DT_POINTER,
    DT_STRING,
    DT_ARRAY,
    DT_SLICE,
    DT_MULTIPLE_TYPES,
    DT_CUSTOM, // TODO : rename as DT_STRUCT
    DT_UNION,
    DT_ERROR,
    DT_MEMBER, // dont know about this one, represents the right side of member reference operator 'point . x'
    DT_ENUM,
    DT_FUNCTION, // FunctionPrototype
    DT_UNDEFINED,
    DT_COUNT,

    DT_INT  = DT_I64, // TODO : delete?
    DT_BOOL = DT_U64,
};

struct DataType {
    int size; // in bytes
    int rank;
    int align;

    DataTypeEnum kind;
    int id; // id for custom types : TODO -> just use one var for enum and this

    void* runtimeInfo;
};

extern DataType dataTypes[];

inline int isInt(int x) {
    return x >= DT_I8 && x <= DT_U64;
}

inline int isSignedInt(int x) {
    return x >= DT_I8 && x <= DT_I64;
}

inline int isUnsignedInt(int x) {
    return x >= DT_U8 && x <= DT_U64;
}

inline int isFloat(int x) {
    return x >= DT_F32 && x <= DT_F64;
}

inline int isPrimitive(int x) {
    return x >= DT_I8 && x <= DT_F64;
}

inline int isStructLike(int x) {
    return x == DT_CUSTOM || x == DT_UNION;
}

inline int isIndexable(int x) {
    return x == DT_POINTER || x == DT_ARRAY || x == DT_SLICE;
}
