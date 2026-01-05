// interpreter related code that provide
// some helper function to get around
// while debuging or any other stuff when
// its good to get some info and hang out
// with metadata closer

#include "interpreter.h"



namespace Interpreter {

    const char* toStr(Opcode opcode) {

        switch (opcode) {

            case OC_PUSH_I8:    return "push_i8";
            case OC_PUSH_U8:    return "push_u8";
            case OC_PUSH_I16:   return "push_i16";
            case OC_PUSH_U16:   return "push_u16";
            case OC_PUSH_I32:   return "push_i32";
            case OC_PUSH_U32:   return "push_u32";
            case OC_PUSH_I64:   return "push_i64";
            case OC_PUSH_U64:   return "push_u64";
            case OC_PUSH_F32:   return "push_f32";
            case OC_PUSH_F64:   return "push_f64";
            case OC_PUSH_PTR:   return "push_ptr";
            case OC_PUSH_BLOB: return "push_const";

            case OC_SET_I8:    return "set_i8";
            case OC_SET_U8:    return "set_u8";
            case OC_SET_I16:   return "set_i16";
            case OC_SET_U16:   return "set_u16";
            case OC_SET_I32:   return "set_i32";
            case OC_SET_U32:   return "set_u32";
            case OC_SET_I64:   return "set_i64";
            case OC_SET_U64:   return "set_u64";
            case OC_SET_F32:   return "set_f32";
            case OC_SET_F64:   return "set_f64";
            case OC_SET_PTR:   return "set_ptr";
            case OC_SET_BLOB: return "set_local";

            case OC_GET_I8:    return "get_i8";
            case OC_GET_U8:    return "get_u8";
            case OC_GET_I16:   return "get_i16";
            case OC_GET_U16:   return "get_u16";
            case OC_GET_I32:   return "get_i32";
            case OC_GET_U32:   return "get_u32";
            case OC_GET_I64:   return "get_i64";
            case OC_GET_U64:   return "get_u64";
            case OC_GET_F32:   return "get_f32";
            case OC_GET_F64:   return "get_f64";
            case OC_GET_PTR:   return "get_ptr";
            case OC_GET_BLOB: return "get_local";

            case OC_LEA: return "lea";
            case OC_PTR_IDX: return "ptr_idx";

            case OC_LOAD_I8:   return "load_i8";
            case OC_LOAD_U8:   return "load_u8";
            case OC_LOAD_I16:  return "load_i16";
            case OC_LOAD_U16:  return "load_u16";
            case OC_LOAD_I32:  return "load_i32";
            case OC_LOAD_U32:  return "load_u32";
            case OC_LOAD_I64:  return "load_i64";
            case OC_LOAD_U64:  return "load_u64";
            case OC_LOAD_F32:  return "load_f32";
            case OC_LOAD_F64:  return "load_f64";
            case OC_LOAD_PTR:  return "load_ptr";
            case OC_LOAD_BLOB: return "load_blob";

            case OC_STORE_I8:   return "store_i8";
            case OC_STORE_U8:   return "store_u8";
            case OC_STORE_I16:  return "store_i16";
            case OC_STORE_U16:  return "store_u16";
            case OC_STORE_I32:  return "store_i32";
            case OC_STORE_U32:  return "store_u32";
            case OC_STORE_I64:  return "store_i64";
            case OC_STORE_U64:  return "store_u64";
            case OC_STORE_F32:  return "store_f32";
            case OC_STORE_F64:  return "store_f64";
            case OC_STORE_PTR:  return "store_ptr";
            case OC_STORE_BLOB: return "store_blob";

            case OC_ADD_I32: return "add_i32";
            case OC_ADD_U32: return "add_u32";
            case OC_ADD_I64: return "add_i64";
            case OC_ADD_U64: return "add_u64";
            case OC_ADD_F32: return "add_f32";
            case OC_ADD_F64: return "add_f64";

            case OC_SUB_I32: return "sub_i32";
            case OC_SUB_U32: return "sub_u32";
            case OC_SUB_I64: return "sub_i64";
            case OC_SUB_U64: return "sub_u64";
            case OC_SUB_F32: return "sub_f32";
            case OC_SUB_F64: return "sub_f64";

            case OC_MUL_I32: return "mul_i32";
            case OC_MUL_U32: return "mul_u32";
            case OC_MUL_I64: return "mul_i64";
            case OC_MUL_U64: return "mul_u64";
            case OC_MUL_F32: return "mul_f32";
            case OC_MUL_F64: return "mul_f64";

            case OC_DIV_I32: return "div_i32";
            case OC_DIV_U32: return "div_u32";
            case OC_DIV_I64: return "div_i64";
            case OC_DIV_U64: return "div_u64";
            case OC_DIV_F32: return "div_f32";
            case OC_DIV_F64: return "div_f64";

            case OC_AND_I32: return "and_i32";
            case OC_AND_U32: return "and_u32";
            case OC_AND_I64: return "and_i64";
            case OC_AND_U64: return "and_u64";

            case OC_OR_I32: return "or_i32";
            case OC_OR_U32: return "or_u32";
            case OC_OR_I64: return "or_i64";
            case OC_OR_U64: return "or_u64";

            case OC_XOR_I32: return "xor_i32";
            case OC_XOR_U32: return "xor_u32";
            case OC_XOR_I64: return "xor_i64";
            case OC_XOR_U64: return "xor_u64";

            case OC_SHL_I32: return "shl_i32";
            case OC_SHL_U32: return "shl_u32";
            case OC_SHL_I64: return "shl_i64";
            case OC_SHL_U64: return "shl_u64";

            case OC_SHR_I32: return "shr_i32";
            case OC_SHR_U32: return "shr_u32";
            case OC_SHR_I64: return "shr_i64";
            case OC_SHR_U64: return "shr_u64";

            case OC_NOT_BOOL: return "not_bool";

            case OC_BOOL_I32: return "bool_cast_i32";
            case OC_BOOL_I64: return "bool_cast_i64";
            case OC_BOOL_F32: return "bool_cast_f32";
            case OC_BOOL_F64: return "bool_cast_f64";

            case OC_SEXT_32_TO_64: return "sext_32_to_64";
            case OC_ZEXT_32_TO_64: return "zext_32_to_64";
            case OC_TRUNC_64_TO_32: return "trunc_64_to_32";

            case OC_CAST_I32_TO_U32: return "cast_i32_to_u32";
            case OC_CAST_I32_TO_F32: return "cast_i32_to_f32";
            case OC_CAST_I32_TO_F64: return "cast_i32_to_f64";

            case OC_CAST_U32_TO_I32: return "cast_u32_to_i32";
            case OC_CAST_U32_TO_F32: return "cast_u32_to_f32";
            case OC_CAST_U32_TO_F64: return "cast_u32_to_f64";

            case OC_CAST_I64_TO_U64: return "cast_i64_to_u64";
            case OC_CAST_I64_TO_F32: return "cast_i64_to_f32";
            case OC_CAST_I64_TO_F64: return "cast_i64_to_f64";

            case OC_CAST_U64_TO_I64: return "cast_u64_to_i64";
            case OC_CAST_U64_TO_F32: return "cast_u64_to_f32";
            case OC_CAST_U64_TO_F64: return "cast_u64_to_f64";

            case OC_CAST_F32_TO_I32: return "cast_f32_to_i32";
            case OC_CAST_F32_TO_I64: return "cast_f32_to_i64";
            case OC_CAST_F32_TO_U32: return "cast_f32_to_u32";
            case OC_CAST_F32_TO_U64: return "cast_f32_to_u64";
            case OC_CAST_F32_TO_F64: return "cast_f32_to_f64";

            case OC_CAST_F64_TO_I32: return "cast_f64_to_i32";
            case OC_CAST_F64_TO_I64: return "cast_f64_to_i64";
            case OC_CAST_F64_TO_U32: return "cast_f64_to_u32";
            case OC_CAST_F64_TO_U64: return "cast_f64_to_u64";
            case OC_CAST_F64_TO_F32: return "cast_f64_to_f32";

            case OC_JUMP:          return "jump";
            case OC_JUMP_IF_TRUE:  return "jump_if_true";
            case OC_JUMP_IF_FALSE: return "jump_if_false";

            case OC_POP: return "pop";
            case OC_DUP: return "dup";

            case OC_CALL: return "call";
            case OC_RET:  return "return";

            case OC_NOP: return "no_operation";
            default: return "unknown";

        }

    }

    FILE* stream = stdout;
    void printBytecode(uint8_t* buffer, uint64_t size) {

        uint8_t* startPtr = buffer;
        uint8_t* endPtr = buffer + size;

        while (buffer < endPtr) {

            fprintf(stream, "%04ld: ", (long)(buffer - startPtr));

            Opcode opcode;
            memcpy(&opcode, buffer, sizeof(Opcode));
            buffer += sizeof(Opcode);

            fprintf(stream, "%-15s", toStr(opcode));

            switch (opcode) {

                case OC_PUSH_I8: {
                    int8_t val;
                    memcpy(&val, buffer, 1);
                    buffer += 1;

                    fprintf(stream, " %d\n", val);
                    break;
                }

                case OC_PUSH_U8: {
                    uint8_t val;
                    memcpy(&val, buffer, 1);
                    buffer += 1;

                    fprintf(stream, " %u\n", val);
                    break;
                }

                case OC_PUSH_I16: {
                    int16_t val;
                    memcpy(&val, buffer, 2);
                    buffer += 2;

                    fprintf(stream, " %d\n", val);
                    break;
                }

                case OC_PUSH_U16: {
                    uint16_t val;
                    memcpy(&val, buffer, 2);
                    buffer += 2;

                    fprintf(stream, " %u\n", val);
                    break;
                }

                case OC_PUSH_I32: {
                    int32_t val;
                    memcpy(&val, buffer, 4);
                    buffer += 4;

                    fprintf(stream, " %d\n", val);
                    break;
                }

                case OC_PUSH_U32: {
                    uint32_t val;
                    memcpy(&val, buffer, 4);
                    buffer += 4;

                    fprintf(stream, " %u\n", val);
                    break;
                }

                case OC_PUSH_I64: {
                    int64_t val;
                    memcpy(&val, buffer, 8);
                    buffer += 8;

                    fprintf(stream, " %lld\n", val);
                    break;
                }

                case OC_PUSH_U64: {
                    uint64_t val;
                    memcpy(&val, buffer, 8);
                    buffer += 8;

                    fprintf(stream, " %llu\n", val);
                    break;
                }

                case OC_PUSH_F32: {
                    float val;
                    memcpy(&val, buffer, 4);
                    buffer += 4;

                    fprintf(stream, " %f\n", val);
                    break;
                }

                case OC_PUSH_F64: {
                    double val;
                    memcpy(&val, buffer, 8);
                    buffer += 8;

                    fprintf(stream, " %lf\n", val);
                    break;
                }

                case OC_PUSH_PTR: {
                    uint64_t val;
                    memcpy(&val, buffer, 8);
                    buffer += 8;

                    fprintf(stream, " 0x%016llx\n", val);
                    break;
                }

                case OC_SET_I8:  case OC_GET_I8:
                case OC_SET_U8:  case OC_GET_U8:
                case OC_SET_I16: case OC_GET_I16:
                case OC_SET_U16: case OC_GET_U16:
                case OC_SET_I32: case OC_GET_I32:
                case OC_SET_U32: case OC_GET_U32:
                case OC_SET_I64: case OC_GET_I64:
                case OC_SET_U64: case OC_GET_U64:
                case OC_SET_F32: case OC_GET_F32:
                case OC_SET_F64: case OC_GET_F64:
                case OC_SET_PTR: case OC_GET_PTR: {
                    uint64_t offset;
                    memcpy(&offset, buffer, 8);
                    buffer += 8;

                    fprintf(stream, " [%llu]\n", offset);
                    break;
                }

                case OC_PUSH_BLOB:
                case OC_GET_BLOB:
                case OC_SET_BLOB: {
                    uint64_t size;
                    memcpy(&size, buffer, 8);
                    buffer += 8;

                    uint64_t offset;
                    memcpy(&offset, buffer, 8);
                    buffer += 8;

                    fprintf(stream, " size=%llu offset=%llu\n", size, offset);
                    break;
                }

                case OC_JUMP:
                case OC_JUMP_IF_TRUE:
                case OC_JUMP_IF_FALSE: {
                    uint64_t target;
                    memcpy(&target, buffer, 8);
                    buffer += 8;

                    fprintf(stream, " %llu\n", target);
                    break;
                }

                case OC_CALL: {
                    uint64_t target;
                    memcpy(&target, buffer, 8);
                    buffer += 8;

                    Function* fcn = (Function*) target;

                    fprintf(stream, " %llu: %.*s\n", target, fcn->name.len, fcn->name.buff);
                    break;
                }

                case OC_LEA:
                case OC_PTR_IDX: {
                    uint64_t offset;
                    memcpy(&offset, buffer, 8);
                    buffer += 8;

                    fprintf(stream, " %llu\n", offset);
                    break;
                }

                default: {
                    if (opcode < OC_COUNT) {
                        fprintf(stream, "\n");
                        break;
                    }

                    fprintf(stream, " (Unknown Opcode)\n");
                    return;
                }
            }
        }
    }

    void printDtype(DataType* dtype) {

        fprintf(stream, "dtype { rank: %i, size: %i }\n", dtype->rank, dtype->size);

    }

    void printLocals(OrderedDict::Container* dict) {

        OrderedDict::Pair* first = OrderedDict::getNext(dict);
        OrderedDict::Pair* ptr = first;

        while (ptr) {

            LocalVarInfo* info = (LocalVarInfo*) ptr->data;

            uint64_t offset;
            memcpy(&offset, ptr->str.buff, ptr->str.len);


            printf("offset: %lu\n", offset);
            printf("  %p: %.*s\n", info->var, info->var->name.len, info->var->name.buff);

            ptr = OrderedDict::getNext(dict);
            if (ptr == first) break;

        }

    }

    void print(ExeBlock* block) {

        fprintf(stream, "Loclas:\n");
        printLocals(block->localsInfoMap);

        fprintf(stream, "\n");

        fprintf(stream, "Bytecode:\n");
        printBytecode(block->bytecode, block->bytecodeSize);

    }

}
