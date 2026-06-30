#include "data_types.h"
#include "foreign_code.h"
#include <cstdint>

namespace Extern::Abi {
    // System V Application Binary Interface
    // AMD64 Architecture Processor Supplement
    // 2012

    // make sure to align values with Abi::ArgKind
    // for simplicity
    enum SysVClass : uint8_t {
        SC_INTEGER = Abi::AK_INT,
        SC_SSE     = Abi::AK_FLOAT,
        SC_SEEUP   = Abi::AK_FLOAT_UP,
        SC_MEMORY  = Abi::AK_AGGREGATE,
        SC_NONE    = Abi::AK_NONE,
    };

    SysVClass getPrimitiveClass(Type::Kind kind) {
        return Type::isFloat(kind) ? SC_SSE : SC_INTEGER;
    }

    SysVClass merge(SysVClass current, SysVClass field) {
        if (current == field) return current;
        if (current == SC_NONE) return field;
        if (field == SC_NONE) return current;
        if (current == SC_MEMORY || field == SC_MEMORY) return SC_MEMORY;
        if (current == SC_INTEGER || field == SC_INTEGER) return SC_INTEGER;
        return SC_SSE;
    }

    void classify(SysVClass chunks[2], Type::TypeInfo* type, uint32_t baseOffset) {
        const int chunkIdx = baseOffset % 8;

        if (baseOffset % type->align != 0) {
            constexpr int count = sizeof(chunks) / sizeof(SysVClass);
            for (int i = 0; i < count; i++) {
                chunks[chunkIdx] = SC_MEMORY;
            }
            return;
        }

        if (Type::isStructLike(type->kind)) {
            Type::StructInfo* sInfo = (Type::StructInfo*) type;
            for (int i = 0; i < sInfo->memberCount; i++) {
                Type::StructMemberInfo* mInfo = sInfo->members + i;
                classify(chunks, mInfo->type, baseOffset + mInfo->offset);
            }
        } else if (type->kind == Type::DT_ARRAY) {
            Type::ArrayInfo* aInfo = (Type::ArrayInfo*)type;
            // TODO
        } else {
            const int chunkIdx = baseOffset / 8;
            if (chunkIdx < 2) {
                SysVClass fieldClass = getPrimitiveClass(type->kind);
                chunks[chunkIdx] = merge(chunks[chunkIdx], fieldClass);
            }
        }
    }

    void sysVClassify(Arg* arg, Value* val) {
        switch (val->typeKind) {
            case Type::DT_F32:
            case Type::DT_F64: {
                arg->kind = AK_FLOAT;
                arg->pass = PK_REG_FLOAT;
                break;
            }

            case Type::DT_U8:
            case Type::DT_I8:
            case Type::DT_U16:
            case Type::DT_I16:
            case Type::DT_U32:
            case Type::DT_I32:
            case Type::DT_U64:
            case Type::DT_I64:
            case Type::DT_POINTER: {
                arg->kind = AK_INT;
                arg->pass = PK_REG_INT;
                break;
            }

            case Type::DT_CUSTOM:
            case Type::DT_UNION: {
                Type::StructInfo* sInfo = &val->def->typeInfoAbi->info->str;
                const uint32_t size = sInfo->base.size;

                // TODO : for now only max 16 bytes support
                if (size > 8 * 2) {
                    arg->kind = Abi::AK_AGGREGATE;
                    arg->pass = Abi::PK_MEM_STRUCT;
                    break;
                }

                SysVClass chunks[2];
                classify(chunks, &sInfo->base, 0);

                arg->kind = (Abi::ArgKind) chunks[0];
                arg->pass = chunks[0] == SC_MEMORY ? PK_MEM_STRUCT : PK_REG_STRUCT_SPLIT;

                break;
            }

            default: {
                arg->kind = AK_NONE;
                break;
            }
        }
    }

    extern "C" void asm_sysVInvoke(CallContext* ctx);

    Driver sysV = {
        .iRegCount = 6,
        .fRegCount = 8,
        .isUniform = false,
        .stackAlign = 16,
        .indirectAlignment = 16,

        .layout = {
            .wordSize = 8,
            .minAlign = 1,
            .maxAlign = 16
        },

        .classify = sysVClassify,
        .invoke   = asm_sysVInvoke
    };
}
