#include "data_types.h"
#include "lexer.h"

DataType dataTypes[] = {

        // VOID
        {
            0,
            0
        },

        // INT
        {
            4,
            1
        },

        // INT_8
        {
            1,
            1
        },

        // INT_16
        {
            2,
            1
        },

        // INT_32
        {
            4,
            1
        },

        // INT_64
        {
            8,
            2
        },

        // UINT_8
        {
            1,
            1
        },

        // UINT_16
        {
            2,
            1
        },

        // UINT_32
        {
            4,
            1
        },

        // UINT_64
        {
            8,
            2
        },

        // FLOAT_32
        {
            4,
            3
        },

        // FLOAT_64
        {
            8,
            4
        },

        // STRING
        {
            8 * 2,
            5
        },

        // POINTER
        {
            8 * 8,
            5
        },

        // ARRAY
        {
            8 * 8,
            5
        },

        // SLICE
        {
            8 * 8,
            5
        },

        // MULTIPLE_TYPES
        {
            0,
            0
        },

        // CUSTOM
        {
            0,
            10
        },

        // MEMBER
        {
            0,
            0
        },

        // ENUM
        {
            0,
            0
        },

        // UNDEFINED
        {
            0,
            0
        }

};
