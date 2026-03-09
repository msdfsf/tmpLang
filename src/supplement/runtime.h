#include "stdint.h"
#include "../array_list.h"
#include "../data_types.h"
#include <cstdint>



struct Variable;

namespace Runtime {

    typedef DataTypeEnum _TypeKind;

    struct _Slice {
        char* ptr;
        uint64_t len;
    };

    struct _String {
        char* buff;
        uint64_t len;
    };

    struct _TypeInfo {
        _TypeKind kind;
        uint64_t size;
        uint64_t align;
    };

    struct _StructMemberInfo {
        _String name;
        _TypeInfo* info;
        uint64_t offset;
    };

    struct _StructInfo {
        _TypeInfo base;

        _String name;

        uint64_t memberCount;
        _StructMemberInfo* members;
    };

    struct _ArrayInfo {
        _TypeInfo base;

        _TypeInfo* elementType; // TODO : rename to element info?
        // uint64_t elementCount;
    };

    struct _PointerInfo {
        _TypeInfo base;
        _TypeInfo* elementType;
    };

    struct _Any {
        _TypeInfo* info;
        union {
            int64_t  i;
            uint64_t u;
            double   f;
            void*    p;
            uint8_t* bp;
            _Slice*  s;
        };
    };

    extern _TypeInfo primitives[];
    extern _ArrayInfo primitiveArrayTemplates[];

    _TypeInfo* resolve(DataType* astType, void* payload);
    _TypeInfo* getType(Variable* var);

    void print(char* fmt, int fmtLen, int argsCnt, _Any* args);
    void printValue(_Any val);

}
