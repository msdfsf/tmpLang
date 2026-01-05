// interpreter related code that suppose
// to handle bytecode interpretatione

#include "data_types.h"
#include "interpreter.h"

#include <cstdint>
#include <cstddef>
#include <cstring>

#include "error.h"
#include "logger.h"
#include "syntax.h"

namespace Interpreter {

    typedef uint64_t vmword;

    Logger::Type logErr = { Logger::ERROR };
    Logger::Type LogWrn = { Logger::WARNING };

    vmword* stack;
    uint64_t stackSize;



    vmword* getFreeStack() {
        // TODO for now no concurency
        return stack;
    }

    void initExec() {
        stackSize = 1024 * 1024;
        stack = (vmword*) alloc(alc, stackSize, 8);
    }

    Err::Err toValue(Function* fcn, vmword* buff, uint64_t buffSize, Value* out) {

        VariableDefinition* def = fcn->prototype.outArg;
        Value* val = &def->var->cvalue;

        switch (val->dtypeEnum) {
            case DT_I8:
            case DT_U8:
            case DT_I16:
            case DT_U16:
            case DT_I32:
            case DT_U32:
            case DT_I64:
            case DT_U64:
            case DT_F32:
            case DT_F64: {
                val->u64 = *buff;
                break;
            }

            case DT_SLICE:
            case DT_ERROR:
            case DT_CUSTOM:
            case DT_POINTER:
            case DT_FUNCTION:
            case DT_MEMBER:
            case DT_COUNT:
            case DT_MULTIPLE_TYPES:
            case DT_ARRAY: {
                Logger::log(logErr, "Datatype is not yet supported as output type in compile time context.", fcn->base.span);
                return Err::NOT_YET_IMPLEMENTED;
            }

            default: {
                Logger::log(logErr, "tmp", fcn->base.span);
                return Err::NOT_YET_IMPLEMENTED;
            }
        }

        out->dtypeEnum = def->var->cvalue.dtypeEnum;
        out->hasValue = 1;

        return Err::OK;

    }


    // some useful functions to not copy-paste that much
    // hopefuly they get optimized

    template <typename Dtype>
    inline Dtype _unaligned_fetch(uint8_t*& buffer) {
        Dtype val;
        memcpy(&val, buffer, sizeof(Dtype));
        buffer += sizeof(Dtype);
        return val;
    }

    template <typename Dtype>
    inline Dtype _aligned_fetch(uint8_t*& buffer) {
        Dtype val = *(Dtype*) buffer;
        buffer -= sizeof(Dtype);
        return val;
    }

    inline void _push(vmword*& stack, vmword word) {
        memcpy(stack, &word, sizeof(vmword));
        stack++;
    }

    inline void setupCall(ExeBlock* exe, vmword*& sp, uint8_t*& ip) {
        PUSH(sp, (uint64_t) ip);
        PUSH(sp, (uint64_t) fp);

        fp = (uint8_t*) sp;

        GROW(sp, exe->localsSize);
        memcpy(fp, exe->locals, exe->localsSize);

        ip = exe->bytecode;
    }

    // macros so we can change the implementation
    // for platforms/build modes

    #define GROW(stack, size) (stack += ((size) + 7) >> 3)
    #define DROP(stack, size) (stack -= ((size) + 7) >> 3)

    #define FETCH(buffer, dtype) (_unaligned_fetch<dtype>(buffer))
    #define FETCH_ALIGN(buffer, dtype) (_aligned_fetch<dtype>(buffer))
    #define PUSH(stack, word) (_push(stack, word))
    #define POP(stack) (*stack -= 1)

    #define BINARY_EXP(dtype, op) BINARY_EXP_EX(dtype, dtype, op)
    #define BINARY_EXP_EX(dtype, resultCast, op) \
        dtype right = (dtype) POP(sp); \
        dtype left = (dtype) POP(sp); \
        dtype ans = left op right; \
        PUSH(sp, (resultCast) ans);

    #define CAST(dest, src) CAST_EX(dest, src, uint64_t)
    #define CAST_EX(dest, src, store) \
        dest val = (dest) (*(src*) (sp - 1)); \
        *(sp - 1) = *(store*) &val;



    Err::Err exec(Function* fcn, Value* out) {

        ExeBlock* block = fcn->exe;

        vmword* sp = getFreeStack(); // operand stack pointer
        uint8_t* ip; // bytecode instruction pointer
        uint8_t* fp; // current frame on operand stack

        // setup 'fake' call with exit
        uint8_t trap[] = { OC_HALT };
        PUSH(sp, (uint64_t) trap);
        PUSH(sp, 0);

        fp = (uint8_t*) sp;
        ip = block->bytecode;

        while (1) {

            Opcode opcode = (Opcode) *ip;
            ip += sizeof(Opcode);

            switch(opcode) {

                case OC_PUSH_I8: {
                    PUSH(sp, FETCH(ip, int8_t));
                    break;
                }

                case OC_PUSH_U8: {
                    PUSH(sp, FETCH(ip, uint8_t));
                    break;
                }

                case OC_PUSH_I16: {
                    PUSH(sp, FETCH(ip, int16_t));
                    break;
                }

                case OC_PUSH_U16: {
                    PUSH(sp, FETCH(ip, uint16_t));
                    break;
                }

                case OC_PUSH_I32: {
                    PUSH(sp, FETCH(ip, int32_t));
                    break;
                }

                case OC_PUSH_U32:
                case OC_PUSH_F32: {
                    PUSH(sp, FETCH(ip, uint32_t));
                    break;
                }

                case OC_PUSH_I64:
                case OC_PUSH_U64:
                case OC_PUSH_F64:
                case OC_PUSH_PTR: {
                    PUSH(sp, FETCH(ip, uint64_t));
                    break;
                }

                case OC_PUSH_BLOB: {
                   const uint64_t offset = FETCH(ip, uint64_t);
                   const uint64_t size = FETCH(ip, uint64_t);

                   GROW(sp, size);
                   memcpy(sp, fp + offset, size);
                }

                case OC_SET_I8:
                case OC_SET_U8: {
                    const uint64_t offset = FETCH(ip, uint64_t);

                    vmword word = POP(sp);
                    memcpy(fp + offset, &word, sizeof(uint8_t));
                }

                case OC_SET_I16:
                case OC_SET_U16: {
                    const uint64_t offset = FETCH(ip, uint64_t);

                    vmword word = POP(sp);
                    memcpy(fp + offset, &word, sizeof(uint16_t));
                }

                case OC_SET_I32:
                case OC_SET_U32:
                case OC_SET_F32: {
                    const uint64_t offset = FETCH(ip, uint64_t);

                    vmword word = POP(sp);
                    memcpy(fp + offset, &word, sizeof(uint32_t));
                }

                case OC_SET_I64:
                case OC_SET_U64:
                case OC_SET_F64: {
                    const uint64_t offset = FETCH(ip, uint64_t);

                    vmword word = POP(sp);
                    memcpy(fp + offset, &word, sizeof(uint64_t));
                }

                case OC_SET_BLOB: {
                    const uint64_t offset = FETCH(ip, uint64_t);
                    const uint64_t size = FETCH(ip, uint64_t);

                    DROP(sp, size);
                    memcpy(fp + offset, sp, size);
                }

                case OC_GET_I8: {
                    const uint64_t offset = FETCH(ip, uint64_t);
                    PUSH(sp, *(int8_t*) (fp + offset));
                }

                case OC_GET_U8: {
                    const uint64_t offset = FETCH(ip, uint64_t);
                    PUSH(sp, *(uint8_t*) (fp + offset));
                }

                case OC_GET_I16: {
                    const uint64_t offset = FETCH(ip, uint64_t);
                    PUSH(sp, *(int16_t*) (fp + offset));
                }

                case OC_GET_U16: {
                    const uint64_t offset = FETCH(ip, uint64_t);
                    PUSH(sp, *(uint16_t*) (fp + offset));
                }

                case OC_GET_I32: {
                    const uint64_t offset = FETCH(ip, uint64_t);
                    PUSH(sp, *(int32_t*) (fp + offset));
                }

                case OC_GET_U32:
                case OC_GET_F32: {
                    const uint64_t offset = FETCH(ip, uint64_t);
                    PUSH(sp, *(uint32_t*) (fp + offset));
                }

                case OC_GET_I64:
                case OC_GET_U64:
                case OC_GET_F64:
                case OC_GET_PTR: {
                    const uint64_t offset = FETCH(ip, uint64_t);
                    PUSH(sp, *(uint64_t*) (fp + offset));
                }

                case OC_GET_BLOB: {
                    const uint64_t offset = FETCH(ip, uint64_t);
                    const uint64_t size = FETCH(ip, uint64_t);

                    vmword* dest = sp;
                    GROW(sp, size);
                    memcpy(dest, fp + offset, size);
                }

                case OC_LEA: {

                }

                case OC_PTR_IDX: {

                }

                // DUMAT: can align > 8 ocure on my operand
                //        stack if I am on meta level
                // pointer in the stack
                // load data from
                //
                case OC_LOAD_I8: {
                    uintptr_t ptr = POP(sp);
                    PUSH(sp, *(int8_t*) ptr);
                    break;
                }

                case OC_LOAD_U8: {
                    uintptr_t ptr = POP(sp);
                    PUSH(sp, *(uint8_t*) ptr);
                    break;
                }

                case OC_LOAD_I16: {
                    uintptr_t ptr = POP(sp);
                    PUSH(sp, *(int16_t*) ptr);
                    break;
                }

                case OC_LOAD_U16: {
                    uintptr_t ptr = POP(sp);
                    PUSH(sp, *(uint16_t*) ptr);
                    break;
                }

                case OC_LOAD_I32: {
                    uintptr_t ptr = POP(sp);
                    PUSH(sp, *(int32_t*) ptr);
                    break;
                }

                case OC_LOAD_U32:
                case OC_LOAD_F32: {
                    uintptr_t ptr = POP(sp);
                    PUSH(sp, *(uint32_t*) ptr);
                    break;
                }

                case OC_LOAD_I64:
                case OC_LOAD_U64:
                case OC_LOAD_F64:
                case OC_LOAD_PTR: {
                    uintptr_t ptr = POP(sp);
                    PUSH(sp, *(uint64_t*) ptr);
                    break;
                }

                case OC_LOAD_BLOB: {
                    const uint64_t size = FETCH(ip, uint64_t);
                    const void* ptr = (void*) POP(sp);

                    void* dest = sp;
                    GROW(sp, size);
                    memcpy(dest, ptr, size);

                    break;
                }

                case OC_STORE_I8:
                case OC_STORE_U8: {
                    int64_t val = POP(sp);
                    uint8_t* ptr = (uint8_t*) POP(sp);

                    *ptr = (uint8_t) val;
                    break;
                }

                case OC_STORE_I16:
                case OC_STORE_U16: {
                    int64_t val = POP(sp);
                    uint16_t* ptr = (uint16_t*) POP(sp);

                    *ptr = (uint16_t) val;
                    break;
                }

                case OC_STORE_I32:
                case OC_STORE_U32:
                case OC_STORE_F32: {
                    int64_t val = POP(sp);
                    uint32_t* ptr = (uint32_t*) POP(sp);

                    *ptr = (uint32_t) val;
                    break;
                }

                case OC_STORE_I64:
                case OC_STORE_U64:
                case OC_STORE_F64:
                case OC_STORE_PTR: {
                    int64_t val = POP(sp);
                    uint64_t* ptr = (uint64_t*) POP(sp);

                    *ptr = (uint64_t) val;
                    break;
                }

                case OC_STORE_BLOB: {
                    const uint64_t size = FETCH(ip, uint64_t);

                    DROP(sp, size);

                    void* src = sp;
                    void* dest = (void*) POP(sp);
                    memcpy(dest, src, size);

                    break;
                }


                case OC_ADD_I32: {
                    BINARY_EXP(int32_t, +);
                    break;
                }

                case OC_ADD_U32: {
                    BINARY_EXP(uint32_t, +);
                    break;
                }

                case OC_ADD_I64: {
                    BINARY_EXP(int64_t, +);
                    break;
                }

                case OC_ADD_U64: {
                    BINARY_EXP(uint64_t, +);
                    break;
                }

                case OC_ADD_F32: {
                    BINARY_EXP_EX(float, uint32_t, +);
                    break;
                }

                case OC_ADD_F64: {
                    BINARY_EXP_EX(double, uint64_t, +)
                    break;
                }

                case OC_SUB_I32: {
                    BINARY_EXP(int32_t, -);
                    break;
                }

                case OC_SUB_U32: {
                    BINARY_EXP(uint32_t, -);
                    break;
                }

                case OC_SUB_I64: {
                    BINARY_EXP(int64_t, -);
                    break;
                }

                case OC_SUB_U64: {
                    BINARY_EXP(uint64_t, -);
                    break;
                }

                case OC_SUB_F32: {
                    BINARY_EXP_EX(float, uint32_t, -);
                    break;
                }

                case OC_SUB_F64: {
                    BINARY_EXP_EX(double, uint64_t, -);
                    break;
                }

                case OC_MUL_I32: {
                    BINARY_EXP(int32_t, *);
                    break;
                }

                case OC_MUL_U32: {
                    BINARY_EXP(uint32_t, *);
                    break;
                }

                case OC_MUL_I64: {
                    BINARY_EXP(int64_t, *);
                    break;
                }

                case OC_MUL_U64: {
                    BINARY_EXP(uint64_t, *);
                    break;
                }

                case OC_MUL_F32: {
                    BINARY_EXP_EX(float, uint32_t, *);
                    break;
                }

                case OC_MUL_F64: {
                    BINARY_EXP_EX(double, uint64_t, *);
                    break;
                }

                case OC_DIV_I32: {
                    BINARY_EXP(int32_t, /);
                    break;
                }

                case OC_DIV_U32: {
                    BINARY_EXP(uint32_t, /);
                    break;
                }

                case OC_DIV_I64: {
                    BINARY_EXP(int64_t, /);
                    break;
                }

                case OC_DIV_U64: {
                    BINARY_EXP(uint64_t, /);
                    break;
                }

                case OC_DIV_F32: {
                    BINARY_EXP_EX(float, uint32_t, /);
                    break;
                }

                case OC_DIV_F64: {
                    BINARY_EXP_EX(double, uint64_t, /);
                    break;
                }

                case OC_AND_I32: {
                    BINARY_EXP(int32_t, &);
                    break;
                }

                case OC_AND_U32: {
                    BINARY_EXP(uint32_t, &);
                    break;
                }

                case OC_AND_I64: {
                    BINARY_EXP(int64_t, &);
                    break;
                }

                case OC_AND_U64: {
                    BINARY_EXP(uint64_t, &);
                    break;
                }

                case OC_OR_I32: {
                    BINARY_EXP(int32_t, &);
                    break;
                }

                case OC_OR_U32: {
                    BINARY_EXP(uint32_t, &);
                    break;
                }

                case OC_OR_I64: {
                    BINARY_EXP(int64_t, &);
                    break;
                }

                case OC_OR_U64: {
                    BINARY_EXP(uint64_t, &);
                    break;
                }

                case OC_XOR_I32: {
                    BINARY_EXP(int32_t, &);
                    break;
                }

                case OC_XOR_U32: {
                    BINARY_EXP(uint32_t, &);
                    break;
                }

                case OC_XOR_I64: {
                    BINARY_EXP(int64_t, &);
                    break;
                }

                case OC_XOR_U64: {
                    BINARY_EXP(uint64_t, &);
                    break;
                }

                case OC_SHL_I32: {
                    BINARY_EXP(int32_t, <<);
                    break;
                }

                case OC_SHL_U32: {
                    BINARY_EXP(uint32_t, <<);
                    break;
                }

                case OC_SHL_I64: {
                    BINARY_EXP(int64_t, <<);
                    break;
                }

                case OC_SHL_U64: {
                    BINARY_EXP(uint64_t, <<);
                    break;
                }

                case OC_SHR_I32: {
                    BINARY_EXP(int32_t, >>);
                    break;
                }

                case OC_SHR_U32: {
                    BINARY_EXP(uint32_t, >>);
                    break;
                }

                case OC_SHR_I64: {
                    BINARY_EXP(int64_t, >>);
                    break;
                }

                case OC_SHR_U64: {
                    BINARY_EXP(uint64_t, >>);
                    break;
                }

                case OC_NOT_BOOL: {
                    vmword word = POP(sp);
                    PUSH(sp, !word);
                    break;
                }

                case OC_BOOL_I32: {
                    CAST(bool, uint32_t);
                    break;
                }
                case OC_BOOL_F32: {
                    CAST(bool, float);
                    break;
                }

                case OC_BOOL_I64: {
                    CAST(bool, uint64_t);
                    break;
                }

                case OC_BOOL_F64: {
                    CAST(bool, double);
                    break;
                }

                case OC_SEXT_32_TO_64: {
                    CAST(int32_t, int64_t);
                    break;
                }

                case OC_ZEXT_32_TO_64: {
                    CAST(uint32_t, uint64_t);
                    break;
                }

                case OC_TRUNC_64_TO_32: {
                    CAST(uint64_t, int32_t);
                    break;
                }

                case OC_CAST_I32_TO_U32: {
                    CAST(int32_t, uint32_t);
                    break;
                }

                case OC_CAST_I32_TO_F32: {
                    CAST(int32_t, float);
                    break;
                }

                case OC_CAST_I32_TO_F64: {
                    CAST(int32_t, double);
                    break;
                }

                case OC_CAST_U32_TO_I32: {
                    CAST_EX(uint32_t, int32_t, int64_t);
                    break;
                }

                case OC_CAST_U32_TO_F32: {
                    CAST(uint32_t, float);
                    break;
                }

                case OC_CAST_U32_TO_F64: {
                    CAST(uint32_t, double);
                    break;
                }

                case OC_CAST_I64_TO_U64: {
                    CAST(int64_t, uint64_t);
                    break;
                }

                case OC_CAST_I64_TO_F32: {
                    CAST(int64_t, float);
                    break;
                }

                case OC_CAST_I64_TO_F64: {
                    CAST(int64_t, double);
                    break;
                }

                case OC_CAST_U64_TO_I64: {
                    CAST(uint64_t, int64_t);
                    break;
                }

                case OC_CAST_U64_TO_F32: {
                    CAST(uint64_t, float);
                    break;
                }

                case OC_CAST_U64_TO_F64: {
                    CAST(uint64_t, double);
                    break;
                }

                case OC_CAST_F32_TO_I32: {
                    CAST_EX(float, int32_t, int64_t);
                    break;
                }

                case OC_CAST_F32_TO_I64: {
                    CAST(float, int64_t);
                    break;
                }

                case OC_CAST_F32_TO_U32: {
                    CAST(float, uint32_t);
                    break;
                }

                case OC_CAST_F32_TO_U64: {
                    CAST(float, uint64_t);
                    break;
                }

                case OC_CAST_F32_TO_F64: {
                    CAST(float, double);
                    break;
                }

                case OC_CAST_F64_TO_I32: {
                    CAST_EX(double, int32_t, int64_t);
                    break;
                }

                case OC_CAST_F64_TO_I64: {
                    CAST(double, int64_t);
                    break;
                }

                case OC_CAST_F64_TO_U32: {
                    CAST(double, uint32_t);
                    break;
                }

                case OC_CAST_F64_TO_U64: {
                    CAST(double, uint64_t);
                    break;
                }

                case OC_CAST_F64_TO_F32: {
                    CAST(float, uint32_t);
                    break;
                }

                case OC_JUMP: {
                    uint64_t target = FETCH(ip, uint64_t);
                    ip = block->bytecode + target;
                    break;
                }

                case OC_JUMP_IF_TRUE: {
                    uint64_t target = FETCH(ip, uint64_t);

                    if (POP(sp)) {
                        ip = block->bytecode + target;
                    }

                    break;
                }

                case OC_JUMP_IF_FALSE: {
                    uint64_t target = FETCH(ip, uint64_t);

                    if (POP(sp) == 0) {
                        ip = block->bytecode + target;
                    }

                    break;
                }

                case OC_POP: {
                    POP(sp);
                    break;
                }

                case OC_DUP: {
                    PUSH(sp, sp[-1]);
                    break;
                }

                case OC_CALL: {
                    Function* fcn = (Function*) POP(sp);
                    ExeBlock* exe = fcn->exe;

                    PUSH(sp, (uint64_t) ip);
                    PUSH(sp, (uint64_t) fp);

                    fp = (uint8_t*) sp;

                    GROW(sp, exe->localsSize);
                    memcpy(fp, exe->locals, exe->localsSize);

                    ip = exe->bytecode;

                    break;
                }

                case OC_RET: {
                    uint64_t size = FETCH(ip, uint64_t);

                    DROP(sp, size);
                    void* returnData = sp;

                    sp = ((uint64_t*) fp) - 2;
                    fp = (uint8_t*) sp[1];
                    ip = (uint8_t*) sp[0];

                    memmove(sp, returnData, size);
                    GROW(sp, size);

                    break;
                }

                case OC_HALT: {
                    break;
                }

                default: {
                    Logger::log(logErr, "tmp");
                    return Err::NOT_YET_IMPLEMENTED;
                }

            }

        }

        uint64_t ansSize = FETCH(ip, uint64_t);
        DROP(sp, ansSize);
        vmword* ans = sp;

        return toValue(fcn, ans, ansSize, out);

    }

}
