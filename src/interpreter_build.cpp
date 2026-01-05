// interpreter related code that focusing
// on building the bytecode

#include "interpreter.h"



namespace Interpreter {

    // for now only one avaliable state
    CompilerState globalState;

    CompilerState* getFreeWorker() {

        Arena::clear(&globalState.locals);
        Arena::clear(&globalState.bytecode);

        OrderedDict::clear(&globalState.localsInfoMap);

        return &globalState;

    }

    void freeWorker(CompilerState* state) {

    }

    void initBuild() {

        constexpr int size = 1024 * 8;

        Arena::init(&globalState.locals, size);
        Arena::init(&globalState.bytecode, size);

        Arena::init(&globalState.rawData, size);

        OrderedDict::init(&globalState.localsInfoMap, size);
        globalState.localsInfoMap.flags |= OrderedDict::COPY_STRINGS;

    }



    inline int isOffsetValid(uint64_t offset) {
        return (offset + 1 != 0);
    }

    // unsigned and signed
    inline int isI32(DataTypeEnum dtype) {
        return dtype >= DT_I8 && dtype < DT_I64;
    }

    inline int isI64(DataTypeEnum dtype) {
        return dtype == DT_I64 && dtype == DT_U64;
    }

    inline int isF32(DataTypeEnum dtype) {
        return dtype == DT_F32;
    }

    inline int isF64(DataTypeEnum dtype) {
        return dtype == DT_F64;
    }

    int getDtypeOffset(DataTypeEnum dtype) {

        switch (dtype) {
            case DT_I8:  return OFF_I8;
            case DT_U8:  return OFF_U8;
            case DT_I16: return OFF_I16;
            case DT_U16: return OFF_U16;
            case DT_I32: return OFF_I32;
            case DT_U32: return OFF_U32;
            case DT_INT:
            case DT_I64: return OFF_I64;
            case DT_U64: return OFF_U64;
            case DT_POINTER: return OFF_PTR;
            default: return OFF_GENERIC;
        }

    }

    int getDtypeOffsetNoCast(DataTypeEnum dtype) {

        switch (dtype) {
            case DT_I8:  return OFF_I32 - 4;
            case DT_U8:  return OFF_U32 - 4;
            case DT_I16: return OFF_I32 - 4;
            case DT_U16: return OFF_U32 - 4;
            case DT_I32: return OFF_I32 - 4;
            case DT_U32: return OFF_U32 - 4;
            case DT_INT:
            case DT_I64: return OFF_I64 - 4;
            case DT_U64: return OFF_U64 - 4;
            default: return OFF_GENERIC - 4;
        }

    }

    inline DataType* getDtype(void* data, DataTypeEnum dtype) {

        if (dtype < DT_STRING) {
            return dataTypes + dtype;
        }

        switch (dtype) {

            case DT_STRING: {

            }

            case DT_CUSTOM: {
                TypeDefinition* def = (TypeDefinition*) data;
                return &def->dtype;
            }

            case DT_ENUM: {
                return dataTypes + ((Enumerator*) data)->dtype;
            }

            case DT_ERROR: {

            }

            case DT_FUNCTION: {

            }

            case DT_UNION: {

            }

            case DT_POINTER: {
                return dataTypes + dtype;
            }

            case DT_ARRAY: {
                return dataTypes + dtype;
            }

            default: {
                // TODO
            }

        }

        return {};

    }

    inline DataType* getDtype(Value* val) {
        return getDtype(val->any, val->dtypeEnum);
    }


    int getAlign(DataType* dtype) {

        if (dtype->align >= VM_STACK_ALIGNMENT) {
            return VM_STACK_ALIGNMENT;
        } else {
            return dtype->align;
        }

    }

    inline Err::Err pushLocal(CompilerState* state, Variable* var, uint64_t* offset) {

        DataType* dtype = getDtype(&var->cvalue);
        if (dtype->size == 0) return Err::INVALID_DATA_TYPE;

        uint8_t* body = (uint8_t*) Arena::push(
            &state->locals,
            dtype->size,
            getAlign(dtype)
        );

        *offset = state->locals.logicalPos - dtype->size;

        memset(body, 0, dtype->size);

        // store debug info
        LocalVarInfo* header = (LocalVarInfo*) alloc(alc, sizeof(LocalVarInfo));
        header->var = var;

        String key = String((char*) offset, sizeof(uint64_t));
        OrderedDict::set(&state->localsInfoMap, key, header);

        return Err::OK;

    }

    inline uint8_t* pushOpcode(CompilerState* state, Opcode opcode) {
        uint8_t* ptr = (uint8_t*) Arena::push(&state->bytecode, sizeof(Opcode), 1);
        memcpy(ptr, &opcode, sizeof(Opcode));
        state->lastOpcode = opcode;
        return ptr;
    }

    inline uint8_t* pushOperand(CompilerState* state, uint64_t val) {
        uint8_t* ptr = (uint8_t*) Arena::push(&state->bytecode, sizeof(uint64_t), 1);
        memcpy(ptr, &val, sizeof(uint64_t));
        return ptr;
    }

    void pushBoolCast(CompilerState* state, DataTypeEnum dtype) {

        if (isI32(dtype)) {
            pushOpcode(state, OC_BOOL_I32);
        } else if (isI64(dtype)) {
            pushOpcode(state, OC_BOOL_I64);
        } else if (isF32(dtype)) {
            pushOpcode(state, OC_BOOL_F32);
        } else if (isF64(dtype)) {
            pushOpcode(state, OC_BOOL_F64);
        }

    }

    void pushString(CompilerState* state, StringInitialization* init) {

        const uint64_t offset = state->rawData.logicalPos;

        char* ptr = (char*)Arena::push(&state->rawData, init->rawPtrLen, 1);
        memcpy(ptr, init->rawPtr, init->rawPtrLen);

        pushOpcode(state, OC_PUSH_PTR);
        pushOperand(state, offset);

        pushOpcode(state, OC_PUSH_I64);
        pushOperand(state, init->rawPtrLen);

    }

    void pushPushInstruction(CompilerState* state, Value* value) {

        Arena::Container* locals = &state->locals;
        Arena::Container* bytecode = &state->bytecode;

        switch (value->dtypeEnum) {

            case DT_I8: {
                pushOpcode(state, OC_PUSH_I32);

                int32_t val = (int32_t) value->i8;
                int32_t* ptr = (int32_t*) Arena::push(bytecode, 4, 1);
                *ptr = val;

                break;
            }

            case DT_I16: {
                pushOpcode(state, OC_PUSH_I32);

                int32_t val = (int32_t) value->i16;
                int32_t* ptr = (int32_t*) Arena::push(bytecode, 4, 1);
                *ptr = val;

                break;
            }

            case DT_I32: {
                pushOpcode(state, OC_PUSH_I32);

                int32_t* ptr = (int32_t*) Arena::push(bytecode, 4, 1);
                *ptr = value->i32;

                break;
            }

            case DT_I64: {
                pushOpcode(state, OC_PUSH_I64);

                int64_t* ptr = (int64_t*) Arena::push(bytecode, 8, 1);
                memcpy(ptr, &value->i64, sizeof(int64_t));

                break;
            }

            case DT_F32: {
                pushOpcode(state, OC_PUSH_F32);

                float* ptr = (float*) Arena::push(bytecode, 4, 1);
                *ptr = value->f32;

                break;
            }

            case DT_F64: {
                pushOpcode(state, OC_PUSH_F64);

                double* ptr = (double*) Arena::push(bytecode, 8, 1);
                *ptr = value->f64;

                break;
            }

            case DT_STRING: {
                // TODO: add StringInitialization to Value
                pushString(state, (StringInitialization*) value->any);
                break;
            }

            default: {
                // TODO
            }

        }

    }



    Opcode selectOperatorOpcode(UnaryExpression* uex) {

        OperatorEnum op = uex->base.opType;
        int dtypeOffset = getDtypeOffsetNoCast(uex->operand->cvalue.dtypeEnum);

        switch (op) {
            case OP_ADDITION: {
                return (Opcode) (OC_ADD_I32 + dtypeOffset);
            }

            case OP_SUBTRACTION: {
                return (Opcode) (OC_SUB_I32 + dtypeOffset);
            }

            case OP_GET_ADDRESS: {
                return (Opcode) (OC_LEA);
            }

            case OP_NEGATION: {
                return OC_NOT_BOOL;
            }

            case OP_NONE: {
                return OC_NOP;
            }

            default: {
                // TODO
            }
        }

        return OC_NOP;

    }

    Opcode selectOperatorOpcode(BinaryExpression* bex) {

        OperatorEnum op = bex->base.opType;
        int dtypeOffset = getDtypeOffsetNoCast(bex->left->cvalue.dtypeEnum);

        switch (op) {
            case OP_ADDITION: {
                return (Opcode) (OC_ADD_I32 + dtypeOffset);
            }

            case OP_SUBTRACTION: {
                return (Opcode) (OC_SUB_I32 + dtypeOffset);
            }

            case OP_MULTIPLICATION: {
                return (Opcode) (OC_MUL_I32 + dtypeOffset);
            }

            case OP_DIVISION: {
                return (Opcode) (OC_DIV_I32 + dtypeOffset);
            }

            case OP_BITWISE_AND: {
                return (Opcode) (OC_AND_I32 + dtypeOffset);
            }

            case OP_BITWISE_OR: {
                return (Opcode) (OC_OR_I32 + dtypeOffset);
            }

            case OP_BITWISE_XOR: {
                return (Opcode) (OC_XOR_I32 + dtypeOffset);
            }

            case OP_SHIFT_LEFT: {
                return (Opcode) (OC_SHL_I32 + dtypeOffset);
            }

            case OP_SHIFT_RIGHT: {
                return (Opcode) (OC_SHR_I32 + dtypeOffset);
            }

            case OP_SUBSCRIPT: {
                return OC_PTR_IDX;
            }

            default: {
                // TODO
            }
        }

        return OC_NOP;

    }

    Opcode selectSetOpcode(DataTypeEnum dtype) {

        int dtypeOffset = getDtypeOffset(dtype);
        return (Opcode) (OC_SET_I8 + dtypeOffset);

    }

    Opcode selectGetOpcode(DataTypeEnum dtype) {

        int dtypeOffset = getDtypeOffset(dtype);
        return (Opcode) (OC_GET_I8 + dtypeOffset);

    }

    Opcode selectLoadOpcode(DataTypeEnum dtype) {

        int dtypeOffset = getDtypeOffset(dtype);
        return (Opcode) (OC_LOAD_I8 + dtypeOffset);

    }

    Opcode selectStoreOpcode(DataTypeEnum dtype) {

        int dtypeOffset = getDtypeOffset(dtype);
        return (Opcode) (OC_STORE_I8 + dtypeOffset);

    }

    Opcode selectCastOpcode(DataTypeEnum src, DataTypeEnum dest) {

        if (dest == DT_I8 || dest == DT_I16) dest = DT_I32;
        if (dest == DT_U8 || dest == DT_U16) dest = DT_U32;

        switch (src) {

            case DT_I8:
            case DT_I16:
            case DT_I32: {

                if (dest == DT_U32) return OC_CAST_I32_TO_U32; // OC_NOP
                if (dest == DT_I64 || dest == DT_U64) return OC_SEXT_32_TO_64;
                if (dest == DT_F32) return OC_CAST_I32_TO_F32;
                if (dest == DT_F64) return OC_CAST_I32_TO_F64;

                break;

            }

            case DT_U8:
            case DT_U16:
            case DT_U32: {

                if (dest == DT_I32) return OC_CAST_U32_TO_I32; // OC_NOP
                if (dest == DT_I64 || dest == DT_U64) return OC_ZEXT_32_TO_64;
                if (dest == DT_F32) return OC_CAST_U32_TO_F32;
                if (dest == DT_F64) return OC_CAST_U32_TO_F64;

                break;

            }

            case DT_I64: {

                if (dest == DT_I32 || dest == DT_U32) return OC_TRUNC_64_TO_32;
                if (dest == DT_U64) return OC_CAST_I64_TO_U64;
                if (dest == DT_F32) return OC_CAST_I64_TO_F32;
                if (dest == DT_F64) return OC_CAST_I64_TO_F64;

                break;

            }
            case DT_U64: {

                if (dest == DT_I32 || dest == DT_U32) return OC_TRUNC_64_TO_32;
                if (dest == DT_I64) return OC_CAST_U64_TO_I64;
                if (dest == DT_F32) return OC_CAST_U64_TO_F32;
                if (dest == DT_F64) return OC_CAST_U64_TO_F64;

                break;

            }

            case DT_F32: {

                if (dest == DT_I32) return OC_CAST_F32_TO_I32;
                if (dest == DT_I64) return OC_CAST_F32_TO_I64;
                if (dest == DT_U32) return OC_CAST_F32_TO_U32;
                if (dest == DT_U64) return OC_CAST_F32_TO_U64;
                if (dest == DT_F64) return OC_CAST_F32_TO_F64;

                break;

            }

            case DT_F64: {

                if (dest == DT_I32) return OC_CAST_F64_TO_I32;
                if (dest == DT_I64) return OC_CAST_F64_TO_I64;
                if (dest == DT_U32) return OC_CAST_F64_TO_U32;
                if (dest == DT_U64) return OC_CAST_F64_TO_U64;
                if (dest == DT_F32) return OC_CAST_F64_TO_F32;

                break;

            }

            default: {
            }

        }

        return OC_NOP;

    }



    Err::Err compile(CompilerState* state, SyntaxNode* node);
    Err::Err compile(CompilerState* state, Expression* node);
    Err::Err compile(CompilerState* state, Variable* node);
    Err::Err compile(CompilerState* state, Function* node);

    Err::Err compile(CompilerState* state, Scope* node) {

        for (int i = 0; i < node->children.base.size; i++) {
            compile(state, *(SyntaxNode**) DArray::get(&node->children.base, i));
        }

        return Err::OK;

    }

    Err::Err compile(CompilerState* state, VariableDefinition* node) {

        Err::Err err;
        uint64_t offset;

        err = pushLocal(state, node->var, &offset);
        if (err != Err::OK) return err;

        if (!isOffsetValid(offset)) {
            return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
        }

        node->vmOffset = offset;

        if (!node->var) return Err::OK;

        compile(state, node->var);

        DataType* type = getDtype(&node->var->cvalue);
        Opcode setOpcode = selectSetOpcode(node->var->cvalue.dtypeEnum);

        pushOpcode(state, setOpcode);
        if (setOpcode == OC_SET_BLOB) {
            pushOperand(state, type->size);
        }

        pushOperand(state, offset);

        return Err::OK;

    }

    Err::Err compile(CompilerState* state, VariableAssignment* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, Variable* node) {

        if (node->expression) {
            return compile(state, node->expression);
        }

        if (node->def) {

            uint64_t offset = node->def->vmOffset;
            uint64_t dtypeSize = 0;

            Opcode op = selectGetOpcode(node->cvalue.dtypeEnum);
            pushOpcode(state, op);

            if (op == OC_GET_BLOB) {
                DataType* dtype = getDtype(&node->cvalue);
                pushOperand(state, dtype->size);
            }

            pushOperand(state, offset);

            return Err::OK;

        }

        pushPushInstruction(state, &node->cvalue);

        return Err::OK;

    }

    Err::Err compile(CompilerState* state, TypeDefinition* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, TypeInitialization* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, Union* scope) {
        return Err::OK;
    }

    Function* getInternalFunction(int idx) {
        return Internal::functions + idx;
    }

    Err::Err compileShortCircuit(CompilerState* state, BinaryExpression* bex) {
        //    &&                       ||
        // 0: <left>                0: <left>
        // 1: dup                   1: dup
        // 2: jump_if_false 5       2: jump_if_true 5
        // 3: pop                   3: pop
        // 4: <right>               4: <right>
        // 5: ...                   5: ...

        compile(state, bex->left);
        pushBoolCast(state, bex->left->cvalue.dtypeEnum);

        pushOpcode(state, OC_DUP);

        if (bex->base.opType == OP_BOOL_AND) {
            pushOpcode(state, OC_JUMP_IF_FALSE);
        } else {
            pushOpcode(state, OC_JUMP_IF_TRUE);
        }

        uint8_t* jumpOperandPtr = pushOperand(state, 0); // 0 as placeholder

        pushOpcode(state, OC_POP);

        compile(state, bex->right);
        pushBoolCast(state, bex->left->cvalue.dtypeEnum);

        uint64_t jumpTargetOffset = state->bytecode.logicalPos;
        memcpy(jumpOperandPtr, &jumpTargetOffset, sizeof(uint64_t));

        return Err::OK;

    }

    Err::Err compile(CompilerState* state, Expression* node) {

        Err::Err err;

        switch (node->type) {

            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) node;

                err = compile(state, uex->operand);
                if (err != Err::OK) return err;

                Opcode oc = selectOperatorOpcode(uex);
                if (oc == OC_NOP) break;

                if (oc == OC_NOT_BOOL) {
                    pushBoolCast(state, uex->operand->cvalue.dtypeEnum);
                }

                pushOpcode(state, oc);

                if (oc == OC_LEA) {
                    pushOperand(state, uex->operand->def->vmOffset);
                }

                break;

            }

            case EXT_BINARY: {

                BinaryExpression* bex = (BinaryExpression*) node;

                if (bex->base.opType == OP_BOOL_AND ||
                    bex->base.opType == OP_BOOL_OR) {
                    return compileShortCircuit(state, bex);
                }

                err = compile(state, bex->left);
                if (err != Err::OK) return err;

                err = compile(state, bex->right);
                if (err != Err::OK) return err;

                Opcode oc = selectOperatorOpcode(bex);
                if (oc == OC_NOP) break;

                pushOpcode(state, oc);

                if (oc == OC_PTR_IDX) {
                    Pointer* ptr = bex->left->cvalue.ptr;
                    DataType* dtype = getDtype(ptr->pointsTo, ptr->pointsToEnum);
                    pushOperand(state, dtype->size);

                    oc = selectLoadOpcode(ptr->pointsToEnum);
                    pushOpcode(state, oc);
                }

                break;

            }

            case EXT_FUNCTION_CALL: {

                FunctionCall* call = (FunctionCall*) node;
                Function* fcn = call->fcn;

                if (!isValidFunctionIdx(call->fcn->internalIdx) && !fcn->exe) {
                    err = compile(state, fcn);
                }

                for (int i = 0; i < call->inArgs.base.size; i++) {
                    Variable* arg = *(Variable**) DArray::get(&call->inArgs.base, i);
                    err = compile(state, arg);
                    if (err != Err::OK) return err;
                }

                pushOpcode(state, OC_CALL);

                // TODO: for now we push pointer, later it would be nice
                //       to provide transferable solution, or at least
                //       a way to generate such solution
                pushOperand(state, (uint64_t) fcn);

                break;

            }

            case EXT_CAST: {

                Cast* cast = (Cast*) node;

                compile(state, cast->operand);

                Opcode op = selectCastOpcode(cast->operand->cvalue.dtypeEnum, cast->target);
                if (op == OC_NOP) break;

                pushOpcode(state, op);

                break;

            }

            case EXT_STRING_INITIALIZATION: {

                pushString(state, (StringInitialization*) node);
                break;

            }

            default: {
                return Err::NOT_YET_IMPLEMENTED;
            }

        }

        return Err::OK;

    }

    Err::Err compile(CompilerState* state, ErrorSet* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, Enumerator* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, Function* node) {

        if (node->exe) return Err::OK;

        node->exe = (ExeBlock*) alloc(alc, sizeof(ExeBlock));
        Err::Err err = compile(node);
        if (err != Err::OK) return err;

        return Err::OK;

    }

    Err::Err compile(CompilerState* state, Branch* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, SwitchCase* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, WhileLoop* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, ForLoop* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, Loop* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, ReturnStatement* node) {

        if (node->var) {
            Err::Err err = compile(state, node->var);
            if (err != Err::OK) return err;
        }

        pushOpcode(state, OC_RET);

        return Err::OK;

    }

    Err::Err compile(CompilerState* state, ContinueStatement* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, BreakStatement* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, GotoStatement* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, Label* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, Namespace* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, Statement* scope) {
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, SyntaxNode* node) {

        switch (node->type) {

            case NT_SCOPE :
                return compile(state, (Scope*) node);
            case NT_VARIABLE_DEFINITION :
                return compile(state, (VariableDefinition*) node);
            case NT_VARIABLE_ASSIGNMENT :
                return compile(state, (VariableAssignment*) node);
            case NT_TYPE_DEFINITION :
                return compile(state, (TypeDefinition*) node);
            case NT_TYPE_INITIALIZATION :
                return compile(state, (TypeInitialization*) node);
            case NT_UNION :
                return compile(state, (Union*) node);
            case NT_ERROR :
                return compile(state, (ErrorSet*) node);
            case NT_ENUMERATOR :
                return compile(state, (Enumerator*) node);
            case NT_VARIABLE :
                return compile(state, (Variable*) node);
            case NT_FUNCTION :
                return compile(state, (Function*) node);
            case NT_BRANCH :
                return compile(state, (Branch*) node);
            case NT_SWITCH_CASE :
                return compile(state, (SwitchCase*) node);
            case NT_WHILE_LOOP :
                return compile(state, (WhileLoop*) node);
            case NT_FOR_LOOP :
                return compile(state, (ForLoop*) node);
            case NT_LOOP :
                return compile(state, (Loop*) node);
            case NT_RETURN_STATEMENT :
                return compile(state, (ReturnStatement*) node);
            case NT_CONTINUE_STATEMENT :
                return compile(state, (ContinueStatement*) node);
            case NT_BREAK_STATEMENT :
                return compile(state, (BreakStatement*) node);
            case NT_GOTO_STATEMENT :
                return compile(state, (GotoStatement*) node);
            case NT_LABEL :
                return compile(state, (Label*) node);
            case NT_NAMESPACE :
                return compile(state, (Namespace*) node);
            case NT_STATEMENT :
                return compile(state, (Statement*) node);

            default:
                // TODO
                return Err::CANNOT_EVALUATE_EXPRESION_AT_CMP_TIME;

        }

    }


    Err::Err compile(Function* fcn) {

        Err::Err err;

        CompilerState* state = getFreeWorker();

        for (int i = 0; i < fcn->prototype.inArgsCnt; i++) {
            VariableDefinition* def = (VariableDefinition*) DArray::get(&fcn->prototype.inArgs.base, i);
            err = compile(state, def);
            if (err != Err::OK) return err;
        }

        err = compile(state, fcn->bodyScope);
        if (err != Err::OK) return err;

        if (state->lastOpcode != OC_RET) {
            pushOpcode(state, OC_RET);
        }

        fcn->exe->bytecodeSize = state->bytecode.logicalPos;
        fcn->exe->bytecode = (uint8_t*) alloc(alc, Arena::getFlatSize(&state->bytecode), 1);
        Arena::flatCopy(&state->bytecode, fcn->exe->bytecode);

        // round up the size to ensure VM_STACK_ALIGNMENT for the contiguous block
        uint64_t finalLocalsSize = state->locals.logicalPos;
        if (finalLocalsSize > 0) {
            finalLocalsSize = (finalLocalsSize + (VM_STACK_ALIGNMENT - 1)) & ~(VM_STACK_ALIGNMENT - 1);
        }

        fcn->exe->localsSize = finalLocalsSize;
        fcn->exe->locals = (uint8_t*) alloc(alc, Arena::getFlatSize(&state->locals), state->maxAlign);
        memset(fcn->exe->locals, 0, fcn->exe->localsSize);
        Arena::flatCopy(&state->locals, fcn->exe->locals);

        fcn->exe->node = fcn;
        fcn->exe->localsInfoMap = OrderedDict::tightCopy(&state->localsInfoMap);

        freeWorker(state);
        return Err::OK;

    }

}
