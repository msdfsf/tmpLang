#include "data_types.h"
#include "lexer.h"
DataType dataTypes[] = {

    // DT_VOID
    {
        .size  = 0,
        .rank  = 0,
        .align = 1
    },

    // DT_INT (Usually i32)
    {
        .size  = 4,
        .rank  = 1,
        .align = 4
    },

    // DT_I8
    {
        .size  = 1,
        .rank  = 1,
        .align = 1
    },

    // DT_I16
    {
        .size  = 2,
        .rank  = 1,
        .align = 2
    },

    // DT_I32
    {
        .size  = 4,
        .rank  = 1,
        .align = 4
    },

    // DT_I64
    {
        .size  = 8,
        .rank  = 2,
        .align = 8
    },

    // DT_U8
    {
        .size  = 1,
        .rank  = 1,
        .align = 1
    },

    // DT_U16
    {
        .size  = 2,
        .rank  = 1,
        .align = 2
    },

    // DT_U32
    {
        .size  = 4,
        .rank  = 1,
        .align = 4
    },

    // DT_U64
    {
        .size  = 8,
        .rank  = 2,
        .align = 8
    },

    // DT_F32
    {
        .size  = 4,
        .rank  = 3,
        .align = 4
    },

    // DT_F64
    {
        .size  = 8,
        .rank  = 4,
        .align = 8
    },

    // DT_STRING (Ptr + Len = 16 bytes)
    {
        .size  = 16,
        .rank  = 5,
        .align = 8
    },

    // DT_POINTER (64-bit Ptr)
    {
        .size  = 8,
        .rank  = 5,
        .align = 8
    },

    // DT_ARRAY (Ptr + Len = 16 bytes, usually)
    {
        .size  = 16,
        .rank  = 5,
        .align = 8
    },

    // DT_SLICE (Ptr + Len = 16 bytes)
    {
        .size  = 16,
        .rank  = 5,
        .align = 8
    },

    // DT_MULTIPLE_TYPES
    {
        .size  = 0,
        .rank  = 0,
        .align = 1
    },

    // DT_CUSTOM (Dynamic size, use defaults)
    {
        .size  = 0,
        .rank  = 10,
        .align = 1
    },

    // DT_MEMBER
    {
        .size  = 0,
        .rank  = 0,
        .align = 1
    },

    // DT_ENUM (Usually i32 backed)
    {
        .size  = 4,
        .rank  = 0,
        .align = 4
    },

    // DT_UNDEFINED
    {
        .size  = 0,
        .rank  = 0,
        .align = 1
    }

};
