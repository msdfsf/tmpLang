#include "data_types.h"
#include "lexer.h"
DataType dataTypes[] = {

    // DT_VOID
    {
        .size  = 0,
        .rank  = 0,
        .align = 1,
        .kind  = DT_VOID
    },

    // DT_I8
    {
        .size  = 1,
        .rank  = 1,
        .align = 1,
        .kind  = DT_I8
    },

    // DT_I16
    {
        .size  = 2,
        .rank  = 2,
        .align = 2,
        .kind  = DT_I16
    },

    // DT_I32
    {
        .size  = 4,
        .rank  = 3,
        .align = 4,
        .kind  = DT_I32
    },

    // DT_I64
    {
        .size  = 8,
        .rank  = 4,
        .align = 8,
        .kind  = DT_I64
    },

    // DT_U8
    {
        .size  = 1,
        .rank  = 1,
        .align = 1,
        .kind  = DT_U8
    },

    // DT_U16
    {
        .size  = 2,
        .rank  = 2,
        .align = 2,
        .kind  = DT_U16
    },

    // DT_U32
    {
        .size  = 4,
        .rank  = 3,
        .align = 4,
        .kind  = DT_U32
    },

    // DT_U64
    {
        .size  = 8,
        .rank  = 4,
        .align = 8,
        .kind  = DT_U64
    },

    // DT_F32
    {
        .size  = 4,
        .rank  = 6,
        .align = 4,
        .kind  = DT_F32
    },

    // DT_F64
    {
        .size  = 8,
        .rank  = 7,
        .align = 8,
        .kind  = DT_F64
    },

    // DT_STRING (Ptr + Len = 16 bytes)
    {
        .size  = 16,
        .rank  = 5,
        .align = 8,
        .kind  = DT_STRING
    },

    // DT_POINTER (64-bit Ptr)
    {
        .size  = 8,
        .rank  = 5,
        .align = 8,
        .kind  = DT_POINTER
    },

    // DT_ARRAY
    {
        .size  = 8,
        .rank  = 5,
        .align = 8,
        .kind  = DT_ARRAY
    },

    // DT_SLICE
    {
        .size  = 8,
        .rank  = 5,
        .align = 8,
        .kind  = DT_SLICE
    },

    // DT_MULTIPLE_TYPES
    {
        .size  = 0,
        .rank  = 0,
        .align = 1,
        .kind  = DT_MULTIPLE_TYPES
    },

    // DT_CUSTOM
    {
        .size  = 0,
        .rank  = 10,
        .align = 1,
        .kind  = DT_CUSTOM
    },

    // DT_MEMBER
    {
        .size  = 0,
        .rank  = 0,
        .align = 1,
        .kind  = DT_MEMBER
    },

    // DT_ENUM
    {
        .size  = 4,
        .rank  = 0,
        .align = 4,
        .kind  = DT_ENUM
    },

    // DT_UNDEFINED
    {
        .size  = 0,
        .rank  = 0,
        .align = 1,
        .kind  = DT_UNDEFINED
    }

};
