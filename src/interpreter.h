#pragma once

#include <cstdint>

#include "dynamic_arena.h"
#include "error.h"
#include "ordered_dict.h"
#include "syntax.h"



namespace Interpreter {

    constexpr int VM_STACK_ALIGNMENT = 16;

    // order of dtypes matters, see DtypeOffset
    enum Opcode : uint8_t {

        OC_PUSH_I8,
        OC_PUSH_U8,
        OC_PUSH_I16,
        OC_PUSH_U16,
        OC_PUSH_I32,
        OC_PUSH_U32,
        OC_PUSH_I64,
        OC_PUSH_U64,
        OC_PUSH_F32,
        OC_PUSH_F64,
        OC_PUSH_PTR,
        OC_PUSH_BLOB,

        OC_SET_I8,
        OC_SET_U8,
        OC_SET_I16,
        OC_SET_U16,
        OC_SET_I32,
        OC_SET_U32,
        OC_SET_I64,
        OC_SET_U64,
        OC_SET_F32,
        OC_SET_F64,
        OC_SET_PTR,
        OC_SET_BLOB,

        OC_GET_I8,
        OC_GET_U8,
        OC_GET_I16,
        OC_GET_U16,
        OC_GET_I32,
        OC_GET_U32,
        OC_GET_I64,
        OC_GET_U64,
        OC_GET_F32,
        OC_GET_F64,
        OC_GET_PTR,
        OC_GET_BLOB,

        OC_LEA,
        OC_PTR_IDX,

        OC_LOAD_I8,
        OC_LOAD_U8,
        OC_LOAD_I16,
        OC_LOAD_U16,
        OC_LOAD_I32,
        OC_LOAD_U32,
        OC_LOAD_I64,
        OC_LOAD_U64,
        OC_LOAD_F32,
        OC_LOAD_F64,
        OC_LOAD_PTR,
        OC_LOAD_BLOB,

        OC_STORE_I8,
        OC_STORE_U8,
        OC_STORE_I16,
        OC_STORE_U16,
        OC_STORE_I32,
        OC_STORE_U32,
        OC_STORE_I64,
        OC_STORE_U64,
        OC_STORE_F32,
        OC_STORE_F64,
        OC_STORE_PTR,
        OC_STORE_BLOB,

        OC_POP,
        OC_POP_N,
        OC_DUP,

        OC_ADD_I32,
        OC_ADD_U32,
        OC_ADD_I64,
        OC_ADD_U64,
        OC_ADD_F32,
        OC_ADD_F64,

        OC_SUB_I32,
        OC_SUB_U32,
        OC_SUB_I64,
        OC_SUB_U64,
        OC_SUB_F32,
        OC_SUB_F64,

        OC_MUL_I32,
        OC_MUL_U32,
        OC_MUL_I64,
        OC_MUL_U64,
        OC_MUL_F32,
        OC_MUL_F64,

        OC_DIV_I32,
        OC_DIV_U32,
        OC_DIV_I64,
        OC_DIV_U64,
        OC_DIV_F32,
        OC_DIV_F64,

        OC_MOD_I32,
        OC_MOD_U32,
        OC_MOD_I64,
        OC_MOD_U64,

        OC_AND_I32,
        OC_AND_U32,
        OC_AND_I64,
        OC_AND_U64,

        OC_OR_I32,
        OC_OR_U32,
        OC_OR_I64,
        OC_OR_U64,

        OC_XOR_I32,
        OC_XOR_U32,
        OC_XOR_I64,
        OC_XOR_U64,

        OC_SHL_I32,
        OC_SHL_U32,
        OC_SHL_I64,
        OC_SHL_U64,

        OC_SHR_I32,
        OC_SHR_U32,
        OC_SHR_I64,
        OC_SHR_U64,

        OC_EQ_I32,
        OC_EQ_I64,
        OC_EQ_F32,
        OC_EQ_F64,

        OC_NE_I32,
        OC_NE_I64,
        OC_NE_F32,
        OC_NE_F64,

        OC_LT_I32,
        OC_LT_U32,
        OC_LT_I64,
        OC_LT_U64,
        OC_LT_F32,
        OC_LT_F64,

        OC_LE_I32,
        OC_LE_U32,
        OC_LE_I64,
        OC_LE_U64,
        OC_LE_F32,
        OC_LE_F64,

        OC_GT_I32,
        OC_GT_U32,
        OC_GT_I64,
        OC_GT_U64,
        OC_GT_F32,
        OC_GT_F64,

        OC_GE_I32,
        OC_GE_U32,
        OC_GE_I64,
        OC_GE_U64,
        OC_GE_F32,
        OC_GE_F64,

        OC_NOT_BOOL,

        OC_BOOL_I32, // also for unsigned
        OC_BOOL_I64, // also for unsigned/ptr
        OC_BOOL_F32,
        OC_BOOL_F64,

        OC_SEXT_32_TO_64, // sign extended
        OC_ZEXT_32_TO_64, // zero extended
        OC_TRUNC_64_TO_32,

        OC_CAST_I32_TO_U32,
        OC_CAST_I32_TO_F32,
        OC_CAST_I32_TO_F64,

        OC_CAST_U32_TO_I32,
        OC_CAST_U32_TO_F32,
        OC_CAST_U32_TO_F64,

        OC_CAST_I64_TO_U64,
        OC_CAST_I64_TO_F32,
        OC_CAST_I64_TO_F64,

        OC_CAST_U64_TO_I64,
        OC_CAST_U64_TO_F32,
        OC_CAST_U64_TO_F64,

        OC_CAST_F32_TO_I32,
        OC_CAST_F32_TO_U32,
        OC_CAST_F32_TO_I64,
        OC_CAST_F32_TO_U64,
        OC_CAST_F32_TO_F64,

        OC_CAST_F64_TO_I32,
        OC_CAST_F64_TO_U32,
        OC_CAST_F64_TO_I64,
        OC_CAST_F64_TO_U64,
        OC_CAST_F64_TO_F32,

        OC_JUMP,
        OC_JUMP_IF_TRUE, // pops
        OC_JUMP_IF_FALSE, // pops

        OC_CALL,
        OC_RET, // TODO separate RET_BLOB

        OC_NOP,
        OC_HALT,

        OC_COUNT

    };

    enum DtypeOffset {
        OFF_I8,
        OFF_U8,
        OFF_I16,
        OFF_U16,
        OFF_I32,
        OFF_U32,
        OFF_I64,
        OFF_U64,
        OFF_F32,
        OFF_F64,
        OFF_PTR,
        OFF_GENERIC,
    };

    struct LocalVarInfo {
        // for now enough
        Variable* var;
    };

    struct LocalFcnInfo {
        // for now enough
        Function* fcn;
    };

    struct CompilerState {

        // dynamic stacks used during compilation
        Arena::Container locals;
        Arena::Container bytecode;

        // stores raw const data such as string literals that
        // bytecode can refer to
        Arena::Container rawData;

        // keys are locals offsets, values are type of LocalVarInfo
        OrderedDict::Container localsInfoMap;

        // max encoutered alignment during building locals stack
        uint64_t maxAlign;

        Opcode lastOpcode;

    };

    struct ExeBlock {

        // raw bytes for initial local variable state
        uint8_t* locals;
        uint64_t localsSize;

        uint8_t* bytecode;
        uint64_t bytecodeSize;

        // maps offsets in locals to their info as LocalVarDebugInfo
        OrderedDict::Container* localsInfoMap;

        // original 'ast' node its based on
        void* node;

    };



    void initBuild();
    void initExec();
    void initEval();
    inline void init() {
        initBuild();
        initExec();
        initEval();
    }

    Err::Err compile(Function* fcn);

    Err::Err exec(Function* fcn, Value* out);

    void print(ExeBlock* block);



    // returns error or dtype enumeric value of the result
    // stores ivalue and idtypeEnum into ans value and dtypeValue
    int execFunction(Function* fcn, Variable* ans);


    inline int evaluate(Variable* op);

    int applyOperator(OperatorEnum oper, Value* value);
    int applyOperator(OperatorEnum oper, Value* valueA, Value* valueB);

};
