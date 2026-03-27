#include "stdint.h"
#include "../array_list.h"
#include "../data_types.h"
#include <cstdint>



struct Variable;

namespace Runtime {

    // We basically just alias types for most parts
    // so we can reuse them
    typedef Type::Kind             _TypeKind;
    typedef Type::_String          _String;
    typedef Type::TypeInfo         _TypeInfo;
    typedef Type::TypeInfoEx       _TypeInfoEx;
    typedef Type::StructInfo       _StructInfo;
    typedef Type::ArrayInfo        _ArrayInfo;
    typedef Type::PointerInfo      _PointerInfo;
    typedef Type::StructMemberInfo _StructMemberInfo;

    struct _Slice {
        char* ptr;
        uint64_t len;
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

    _TypeInfo* resolve(Type::TypeInfoEx* astType, void* payload);
    _TypeInfo* getType(Variable* var);

    void print(char* fmt, int fmtLen, int argsCnt, _Any* args);
    void printValue(_Any val);

}
