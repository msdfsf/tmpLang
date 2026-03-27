#include "data_types.h"

namespace Type {

    TypeInfo basicTypes[] = {

        {
            .kind  = DT_VOID,
            .rank  = 0,
            .size  = 0,
            .align = 1,
        },

        {
            .kind  = DT_I8,
            .rank  = 1,
            .size  = 1,
            .align = 1,
        },

        {
            .kind  = DT_I16,
            .rank  = 2,
            .size  = 2,
            .align = 2,
        },

        {
            .kind  = DT_I32,
            .rank  = 3,
            .size  = 4,
            .align = 4,
        },

        {
            .kind  = DT_I64,
            .rank  = 4,
            .size  = 8,
            .align = 8,
        },

        {
            .kind  = DT_U8,
            .rank  = 1,
            .size  = 1,
            .align = 1,
        },

        {
            .kind  = DT_U16,
            .rank  = 2,
            .size  = 2,
            .align = 2,
        },

        {
            .kind  = DT_U32,
            .rank  = 3,
            .size  = 4,
            .align = 4,
        },

        {
            .kind  = DT_U64,
            .rank  = 4,
            .size  = 8,
            .align = 8,
        },

        {
            .kind  = DT_F32,
            .rank  = 6,
            .size  = 4,
            .align = 4,
        },

        {
            .kind  = DT_F64,
            .rank  = 7,
            .size  = 8,
            .align = 8,
        },

        {
            .kind  = DT_STRING,
            .rank  = 5,
            .size  = 16,
            .align = 8,
        },

        {
            .kind  = DT_POINTER,
            .rank  = 5,
            .size  = 8,
            .align = 8,
        },

        {
            .kind  = DT_ARRAY,
            .rank  = 5,
            .size  = 8,
            .align = 8,
        },

        {
            .kind  = DT_SLICE,
            .rank  = 5,
            .size  = 8,
            .align = 8,
        },

        {
            .kind  = DT_MULTIPLE_TYPES,
            .rank  = 0,
            .size  = 0,
            .align = 1,
        },

        {
            .kind  = DT_CUSTOM,
            .rank  = 10,
            .size  = 0,
            .align = 1,
        },

        {
            .kind  = DT_ENUM,
            .rank  = 0,
            .size  = 4,
            .align = 4,
        },

        {
            .kind  = DT_UNDEFINED,
            .rank  = 0,
            .size  = 0,
            .align = 1,
        }

    };

    const char* str(Kind kind) {
        switch (kind) {
            case Type::DT_VOID: return "void";
            case Type::DT_I8:   return "i8";
            case Type::DT_I16:  return "i16";
            case Type::DT_I32:  return "i32";
            case Type::DT_I64:  return "i64";
            case Type::DT_U8:   return "u8";
            case Type::DT_U16:  return "u16";
            case Type::DT_U32:  return "u32";
            case Type::DT_U64:  return "u64";
            case Type::DT_F32:  return "f32";
            case Type::DT_F64:  return "f64";

            case Type::DT_STRING:  return "string";
            case Type::DT_POINTER: return "ptr";
            case Type::DT_ARRAY:   return "array";
            case Type::DT_SLICE:   return "slice";
            case Type::DT_CUSTOM:  return "DT_CUSTOM";
            case Type::DT_UNION:   return "DT_UNION";
            case Type::DT_ERROR:   return "DT_ERROR";
            case Type::DT_ENUM:    return "DT_ENUM";

            case Type::DT_FUNCTION: return "DT_FUNCTION";

            case Type::DT_UNDEFINED:      return "DT_UNDEFINED";
            case Type::DT_MULTIPLE_TYPES: return "DT_MULTIPLE_TYPES";
        }
        return "Unknown";
    }

};
