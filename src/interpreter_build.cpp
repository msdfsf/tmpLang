// interpreter related code that focusing
// on building the bytecode

#include "array_list.h"
#include "data_types.h"
#include "dynamic_arena.h"
#include "error.h"
#include "globals.h"
#include "interpreter.h"
#include "operators.h"
#include "supplement/runtime.h"
#include "syntax.h"
#include "logger.h"



#include <atomic>
#include <cstdint>
#include <cstdlib>
#include <float.h>
#include <thread>



static Logger::Type logErr = { .level = Logger::ERROR, .tag = "VM" };

namespace Interpreter {

    void clear(CompilerState* state);

    // How concurrency is handled:
    // There are few worker and the main thread.
    // Firstly the function is secured via atomic state change.
    // (idle -> running) if success, function is secured and its
    // our responsibility to do the compilation.
    // Next the worker is selected. Bitwise checklist is used.
    // Each worker has its own bit corresponding with its id.
    // Worker is selected via atomic bit change.
    // If worker is selected, it compiles the function.
    // If no worker is ready, the calling thread is responsible
    // for the compilation.
    // because compiler state is pricy to duplicate or extend
    // all containers, In case that same worker has to compile
    // multiple functions they would be queued and processed
    // sequentially.
    struct Worker {
        int id;

        std::thread thread;
        std::atomic<bool> hasWork;

        CompilerState state;
        DArray::Container fcnQueue;
    };

    constexpr int workerCount = 4;
    Worker workers[workerCount];
    thread_local Worker* localWorker = NULL;

    // reserved state for masters compilation if needed
    CompilerState mastersState;

    // the 'check list' mechanism, each thread
    // will use its own bit to write to mark if
    // its ready for the next task or not.
    // 1 - free, 0 - working
    std::atomic<uint64_t> checkList = (1ULL << workerCount) - 1;

    // true: function secured
    // false: function already claimed/compiled
    bool secureFunction(Function* fcn, bool waitForExecution) {

        TaskStatus expectedStatus = TS_PENDING;
        std::atomic_ref<TaskStatus> fcnStatus(fcn->compilationStatus);

        if (!fcnStatus.compare_exchange_strong(expectedStatus, TS_RUNNING)) {
            if (waitForExecution) {
                while (expectedStatus == TS_RUNNING) {
                    fcnStatus.wait(TS_RUNNING);
                    expectedStatus = fcnStatus.load();
                }
            }
            return false;
        }

        fcn->exe = (ExeBlock*) alloc(alc, sizeof(ExeBlock));

        return true;

    }

    // returns free worker or NULL at failure
    Worker* secureWorker() {

        // for future me: memory_order_relaxed - only this operation's atomicity is guaranteed
        uint64_t checkListCopy = checkList.load(std::memory_order_relaxed);

        int workerId = -1;
        while (checkListCopy != 0) {

            // TODO: move to a function?
            for (int i = 0; i < workerCount; i++) {
                if (checkListCopy & (1ULL << i)) {
                    workerId = i;
                    break;
                }
            }

            // fetch_and returns the value of checkList
            // before the AND was applied
            const uint64_t mask = 1ULL << workerId;
            if (checkList.fetch_and(~mask) & mask) {
                break;
            }

            checkListCopy = checkList.load(std::memory_order_relaxed);

        }

        if (workerId >= 0 && workerId < workerCount) {
            return workers + workerId;
        } else {
            return NULL;
        }

    }

    Err::Err compile(Function* fcn, bool waitForExecution) {

        if (fcn->exe) return Err::OK;
        if (!secureFunction(fcn, waitForExecution)) return Err::OK;

        Worker* worker = secureWorker();

        if (worker) {

            DArray::push(&worker->fcnQueue, &fcn);
            worker->hasWork.store(true);
            worker->hasWork.notify_all();

            if (waitForExecution) {
                while (worker->hasWork.load()) {
                    worker->hasWork.wait(true);
                }
            }

        } else if (localWorker) {
            // we are worker, but we are bussy

            DArray::push(&localWorker->fcnQueue, &fcn);

        } else {
            // when master become a slave

            clear(&mastersState);
            Err::Err err = compile(&mastersState, fcn);
            if (err != Err::OK) return err;

            std::atomic_ref<TaskStatus> fcnStatus(fcn->compilationStatus);
            fcnStatus.store(TS_READY);
            fcnStatus.notify_all();

        }

        return Err::OK;

    }

    void runWorker(Worker* worker) {

        localWorker = worker;

        while (1) {

            worker->hasWork.wait(false);

            while (worker->fcnQueue.size > 0) {

                Function* fcn = *(Function**) DArray::getLast(&worker->fcnQueue);
                DArray::pop(&worker->fcnQueue);

                clear(&worker->state);
                compile(&worker->state, fcn);

                std::atomic_ref<TaskStatus> fcnStatus(fcn->compilationStatus);
                fcnStatus.store(TS_READY);
                fcnStatus.notify_all();

            }

            worker->hasWork.store(false);
            worker->hasWork.notify_one();

            checkList.fetch_or(1ULL << worker->id);
            checkList.notify_all();

        }

    }
    void init(CompilerState* state) {

        constexpr int size = 1024 * 8;

        Arena::init(&state->locals, size);
        Arena::init(&state->bytecode, size);
        Arena::init(&state->rawData, size);

        OrderedDict::init(&state->localsInfoMap, size);
        state->localsInfoMap.flags |= OrderedDict::COPY_STRINGS;

        DArray::init(&state->lines, size, sizeof(LineInfo));

        state->populateLocals = false;
        state->defaultArgsSize = 0;

        state->vecResult.isTmp = false;

    }

    void init(Worker* worker) {

        init(&worker->state);
        DArray::init(&worker->fcnQueue, 4, sizeof(Function*));
        worker->thread = std::thread(runWorker, worker);

    }

    void initBuild() {

        for (int i = 0; i < workerCount; i++) {
            init(workers + i);
        }

        init(&mastersState);

    }

    void clear(CompilerState* state) {
        Arena::clear(&state->bytecode);
        Arena::clear(&state->locals);
        Arena::clear(&state->rawData);
        DArray::clear(&state->lines);
        OrderedDict::clear(&state->localsInfoMap);
    }



    bool isFixedArray(Array* arr) {
        // in hpes that length will always be pre-unwrapped
        return arr->length->cvalue.hasValue;
    }

    // LOOK_AT : seems unnecessary
    inline bool isLineInSpan(Span* span, uint64_t line) {
        return span->start.ln <= line && span->end.ln >= line;
    }

    inline void commitLineInfo(CompilerState* state) {

        LineInfo line;
        line.span = state->currentLineSpan;
        line.ocOffsetStart = state->currentOffsetStart;
        line.ocOffsetEnd = state->bytecode.logicalPos;

        DArray::push(&state->lines, &line);

    }

    // TODO : maybe not the best naming
    inline void updateSourceLocation(CompilerState* state, Span* span) {

        if (!span) return;

        if (state->currentLineSpan.start.ln == 0) {
            // the first time into this function
            state->currentLineSpan = *span;
            state->currentOffsetStart = 0;
            return;
        }

        if (!isLineInSpan(&state->currentLineSpan, span->start.ln)) {
            // new span (push old line)
            commitLineInfo(state);
            state->currentLineSpan = *span;
            state->currentOffsetStart = state->bytecode.logicalPos;
            return;
        }

        if (span->end.ln > state->currentLineSpan.end.ln) {
            // expand span
            state->currentLineSpan.end = span->end;
        }

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
            case DT_I64: return OFF_I64;
            case DT_U64: return OFF_U64;
            case DT_ARRAY:
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
            case DT_I64: return OFF_I64 - 4;
            case DT_U64: return OFF_U64 - 4;
            case DT_ARRAY:
            case DT_POINTER: return OFF_PTR - 4;
            default: return OFF_GENERIC - 4;
        }

    }

    int getDtypeOffsetNoCastArithmetic(DataTypeEnum dtype) {

        switch (dtype) {
            case DT_I8:  return OFF_I32 - 4;
            case DT_U8:  return OFF_U32 - 4;
            case DT_I16: return OFF_I32 - 4;
            case DT_U16: return OFF_U32 - 4;
            case DT_I32: return OFF_I32 - 4;
            case DT_U32: return OFF_U32 - 4;
            case DT_I64: return OFF_I64 - 4;
            case DT_ARRAY:
            case DT_POINTER:
            case DT_U64: return OFF_U64 - 4;
            default: return OFF_GENERIC - 4;
        }

    }

    inline DataType* getDtype(void* data, DataTypeEnum dtype) {

        if (dtype <= DT_F64) {
            return dataTypes + dtype;
        }

        switch (dtype) {

            case DT_CUSTOM: {
                TypeDefinition* def = (TypeDefinition*) data;
                return &def->dtype;
            }

            case DT_UNION: {
                break;
            }

            case DT_ENUM: {
                return dataTypes + ((Enumerator*) data)->dtype;
            }

            case DT_ERROR: {
                break;
            }

            case DT_FUNCTION: {
                break;
            }

            case DT_POINTER: {
                return dataTypes + dtype;
            }

            case DT_ARRAY: {
                return dataTypes + dtype;
            }

            case DT_STRING: {

            }

            default: {
                // TODO
            }

        }

        return {};

    }

    // TODO : to global? or pointer to use Value?
    inline Value toValue(Pointer* ptr) {
        Value val;
        val.any = ptr->pointsTo;
        val.dtypeEnum = ptr->pointsToEnum;
        val.hasValue = 0;
        return val;
    }

    inline DataType* getDtype(Value* val) {
        return getDtype(val->any, val->dtypeEnum);
    }

    // TODO :
    //   The entire 'type system' feels weird.
    //   Conceptually, it feels correct to create explicit Type objects only
    //   for fundamental types (int/float), user-defined structs/unions,
    //   and enums.
    //
    //   In the case of fixed-length arrays, defining a unique Type object for each
    //   size (int[5] vs int[6]) feels wasteful and incorrect. However,
    //   avoiding this leads to a fragmented implementation where we cannot
    //   simply inspect the Type object itself but must juggle extra metadata
    //   (like AST nodes) to calculate basic properties like size.
    //
    //   I might refactor the 'type system' later, perhaps by adding cache nodes,
    //   a lookup mechanism, or something else to make the code feel better.
    //   But until then, I shall excuse myself and simply write what works to
    //   get the job done.
    inline Err::Err getDtypeInfo(Value* val, uint64_t* size, uint64_t* align) {

        Err::Err err = Err::OK;

        const DataTypeEnum dtypeEnum = val->dtypeEnum;

        if (isPrimitive(dtypeEnum) || dtypeEnum == DT_POINTER) {
            DataType* const dtype = dataTypes + dtypeEnum;
            *size = dtype->size;
            *align = dtype->align;
            return err;
        }

        if (dtypeEnum == DT_ARRAY) {

            Array* arr = val->arr;

            // if length is NULL, we are basicly dealing with
            // a runtime length, and we can interpret our dtype
            // as slice
            if (!arr->length) {
                *size = 16;
                *align = 8;
                return Err::OK;
            }

            arr->length = unwrap(arr->length);
            const uint64_t len = arr->length->cvalue.u64;

            uint64_t elementSize;
            uint64_t elementAlign;
            Value tmpVal = toValue(&arr->base);
            // TODO : cache
            err = getDtypeInfo(&tmpVal, &elementSize, &elementAlign);
            if (err != Err::OK) return err;

            *size = len * elementSize;
            *align = elementAlign;

            return err;

        }

        if (dtypeEnum == DT_CUSTOM) {

            TypeDefinition* const typeDef = val->def;
            *size = typeDef->dtype.size;
            *align = typeDef->dtype.align;

            return err;

        }

        // TODO
        return Err::NOT_YET_IMPLEMENTED;

    }

    inline uint64_t getDtypeSize(Value* val) {
        // TODO : for now this
        uint64_t size;
        uint64_t align;
        getDtypeInfo(val, &size, &align);
        return size;
    }

    inline uint64_t getDtypeAlign(Value* val) {
        return 0;
    }

    int getAlign(DataType* dtype) {
        return sizeof(vmword);
    }

    inline uint64_t addToConstPool(CompilerState* state, char* buff, uint64_t buffLen) {
        const uint64_t offset = state->rawData.logicalPos;

        char* ptr = (char*)Arena::push(&state->rawData, buffLen, 1);
        memcpy(ptr, buff, buffLen);

        return offset;
    }

    inline Err::Err pushLocal(CompilerState* state, Variable* var, uint64_t* offset) {

        uint64_t size;
        uint64_t align;
        Err::Err err = getDtypeInfo(&var->cvalue, &size, &align);
        if (err != Err::OK) return err;

        const uint64_t vmwordsCount = BYTES_TO_WORDS(size);

        *offset = state->locals.logicalPos;

        if (state->populateLocals) {
            const uint64_t allocSize = vmwordsCount * sizeof(vmword);
            uint8_t* body = (uint8_t*) Arena::push(&state->locals, allocSize, sizeof(vmword));
            memset(body, 0, allocSize);
        } else {
            state->locals.logicalPos += vmwordsCount;
        }

        // store debug info
        LocalVarInfo* header = (LocalVarInfo*) alloc(alc, sizeof(LocalVarInfo));
        header->var = var;
        header->size = size;
        header->align = align;

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

        const uint64_t offset = addToConstPool(state, init->rawStr.buff, init->rawStr.len);

        pushOpcode(state, OC_PUSH_PTR);
        pushOperand(state, offset);

        pushOpcode(state, OC_PUSH_I64);
        pushOperand(state, init->rawStr.len);

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

    Opcode selectOperatorOpcode(UnaryExpression* uex) {

        OperatorEnum op = uex->base.opType;
        switch (op) {

        case OP_ADDITION: {
            int offset = getDtypeOffsetNoCast(uex->operand->cvalue.dtypeEnum);
            return (Opcode)(OC_ADD_I32 + offset);
        }

        case OP_SUBTRACTION: {
            int offset = getDtypeOffsetNoCast(uex->operand->cvalue.dtypeEnum);
            return (Opcode)(OC_SUB_I32 + offset);
        }

        case OP_GET_ADDRESS: {
            return (Opcode)(OC_LEA);
        }

        case OP_GET_VALUE: {
            return selectLoadOpcode(uex->operand->cvalue.dtypeEnum);
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
        int dtypeOffset = getDtypeOffsetNoCastArithmetic(bex->left->cvalue.dtypeEnum);

        switch (op) {
        case OP_ADDITION: {
            return (Opcode)(OC_ADD_I32 + dtypeOffset);
        }

        case OP_SUBTRACTION: {
            return (Opcode)(OC_SUB_I32 + dtypeOffset);
        }

        case OP_MULTIPLICATION: {
            return (Opcode)(OC_MUL_I32 + dtypeOffset);
        }

        case OP_DIVISION: {
            return (Opcode)(OC_DIV_I32 + dtypeOffset);
        }

        case OP_BITWISE_AND: {
            return (Opcode)(OC_AND_I32 + dtypeOffset);
        }

        case OP_BITWISE_OR: {
            return (Opcode)(OC_OR_I32 + dtypeOffset);
        }

        case OP_BITWISE_XOR: {
            return (Opcode)(OC_XOR_I32 + dtypeOffset);
        }

        case OP_SHIFT_LEFT: {
            return (Opcode)(OC_SHL_I32 + dtypeOffset);
        }

        case OP_SHIFT_RIGHT: {
            return (Opcode)(OC_SHR_I32 + dtypeOffset);
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



    // TODO : move in meaningful place
    enum {
        IS_LVALUE = 1,
        IS_ROOT = (1 << 1),
        FORCE_ARRAY_LENGTH = (1 << 2),
        FORCE_VEC_OPCODES = (1 << 3), // TODO : ? include FORCE_ARRAY_LENGTH ?
    };

    Err::Err compile(CompilerState* state, SyntaxNode* node);
    Err::Err compile(CompilerState* state, Expression* node, Variable* target = NULL, Flags flags = 0);
    Err::Err compile(CompilerState* state, Variable* node, Variable* target = NULL, Flags flags = 0);
    Err::Err compile(CompilerState* state, Function* node);
    Err::Err compile(CompilerState* state, VariableAssignment* node);

    // usually if function is defined with any it will
    // be used anyway, so I kinda have to create this
    // runtime type tree, and if i already have it,
    // I can just reuse it in vm, so i can actualy use
    // the same function
    // TODO : grammar
    Err::Err compileAsAny(CompilerState* state, Variable* var) {

        Err::Err err = Err::OK;

        Runtime::_TypeInfo* runtimeInfo = Runtime::getType(var);
        if (!runtimeInfo) {
            return Err::NOT_YET_IMPLEMENTED;
        }

        pushOpcode(state, OC_PUSH_PTR);
        pushOperand(state, (uint64_t) runtimeInfo);

        if (isStructLike(var->cvalue.dtypeEnum)) {

            // TODO : move to a function?
            DataType* dtype = getDtype(&var->cvalue);

            uint64_t offset = state->locals.logicalPos;
            push(&state->locals, dtype->size, dtype->align);

            err = compile(state,var);
            if (err != Err::OK) return err;

            pushOpcode(state, OC_SET_BLOB);
            pushOperand(state, dtype->size);
            pushOperand(state, offset);

            pushOpcode(state, OC_LEA);
            pushOperand(state, offset);

        } else if (var->cvalue.dtypeEnum == DT_STRING) {

            StringInitialization* init = (StringInitialization*) var->expression;

            // we reserve space for slice in locals
            uint64_t offset = state->locals.logicalPos;
            uint64_t* sliceTemplate = (uint64_t*) Arena::push(&state->locals, 16, 8);

            sliceTemplate[0] = (uint64_t) init->rawStr.buff;
            sliceTemplate[1] = (uint64_t) init->rawStr.len;

            pushOpcode(state, OC_LEA);
            pushOperand(state, offset);

        } else if (var->cvalue.dtypeEnum == DT_ARRAY) {

            err = compile(state, var, NULL, FORCE_ARRAY_LENGTH | FORCE_VEC_OPCODES);
            if (err != Err::OK) return err;

            if (var->cvalue.dtypeEnum == DT_ARRAY) {
                pushOpcode(state, OC_VEC_TO_REF);
            }

        } else {

            err = compile(state, var);
            if (err != Err::OK) return err;

        }

        return err;
    }

    Err::Err compile(CompilerState* state, Scope* node) {

        for (int i = 0; i < node->children.base.size; i++) {
            compile(state, *(SyntaxNode**) DArray::get(&node->children.base, i));
        }

        return Err::OK;

    }

    Err::Err compile(CompilerState* state, VariableDefinition* node) {

        updateSourceLocation(state, node->base.span);

        Err::Err err;
        uint64_t offset;

        err = pushLocal(state, node->var, &offset);
        if (err != Err::OK) return err;

        if (!isOffsetValid(offset)) {
            return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
        }

        node->vmOffset = offset;

        // do we qualify for initialization?
        if (!node->var ||
            (!node->var->expression && !node->var->cvalue.hasValue)
        ) {
            return Err::OK;
        }

        // ? will it alwyas work ?
        VariableAssignment ass;
        ass.base.span = node->base.span;
        ass.lvar = node->var;
        ass.rvar = node->var;

        err = compile(state, &ass);
        if (!err) return err;

        /*
        compile(state, node->var, node->var);
        if (node->var->cvalue.dtypeEnum == DT_ARRAY) {
            // arrays are assigned by value
            return Err::OK;
        }

        Opcode setOpcode = selectSetOpcode(node->var->cvalue.dtypeEnum);
        pushOpcode(state, setOpcode);
        if (setOpcode == OC_SET_BLOB) {
            DataType* dtype = getDtype(&node->var->cvalue); // TODO : to wasteful for size?
            pushOperand(state, dtype->size);
        }

        pushOperand(state, offset);
        */
        return Err::OK;

    }

    Err::Err compile(CompilerState* state, VariableAssignment* node) {

        updateSourceLocation(state, node->base.span);

        Err::Err err;

        // as we may be from VariableDefinition
        Variable* lvar = (node->lvar->def) ? node->lvar : unwrap(node->lvar);
        Variable* rvar = node->rvar;//unwrap(node->rvar);

        if (lvar->cvalue.dtypeEnum == DT_ARRAY) {
            // we want to assign by value -> use of vec ops

            err = compile(state, rvar, lvar, FORCE_ARRAY_LENGTH | FORCE_VEC_OPCODES | IS_ROOT);
            if (err != Err::OK) return err;

            pushOpcode(state, OC_VEC_RESET);

            return Err::OK;

        }

        if (lvar->def) {

            err = compile(state, node->rvar);
            if (err != Err::OK) return err;

            Opcode op = selectSetOpcode(lvar->cvalue.dtypeEnum);
            pushOpcode(state, op);
            pushOperand(state, lvar->def->vmOffset);

            return Err::OK;

        }

        // here we ecpect lvalue to be a 'random'
        // epression which should result into pointer
        // on stack if normaly compiled.
        err = compile(state, lvar, NULL, 1);
        if (err != Err::OK) return err;

        err = compile(state, node->rvar, lvar);
        if (err != Err::OK) return err;

        Opcode op = selectStoreOpcode(lvar->cvalue.dtypeEnum);
        pushOpcode(state, op);

        return Err::OK;

    }

    Err::Err compile(CompilerState* state, Variable* node, Variable* target, Flags flags) {

        // TODO : kinda wasteful, maybe we create either flag or
        //  force each line-like statement to be parsed as Statement
        updateSourceLocation(state, node->base.span);

        if (node->expression) {
            return compile(state, node->expression, target, flags);
        }

        if (node->def) {

            Value* val = &node->def->var->cvalue;

            uint64_t offset = node->def->vmOffset;
            uint64_t dtypeSize = 0;

            const DataTypeEnum dtypeEnum = val->dtypeEnum;
            if (dtypeEnum == DT_ARRAY && isFixedArray(val->arr)) {

                // TODO : this is just bullshit
                uint64_t elementSize;
                uint64_t elementAlign;

                Value tmpVal;
                tmpVal.any = node->cvalue.arr->base.pointsTo;
                tmpVal.dtypeEnum = node->cvalue.arr->base.pointsToEnum;
                getDtypeInfo(&tmpVal, &elementSize, &elementAlign);


                pushOpcode(state, OC_LEA);
                pushOperand(state, offset);

                if (flags & FORCE_ARRAY_LENGTH) {
                    compile(state, val->arr->length);
                }

                return Err::OK;

            }

            Opcode op = selectGetOpcode(dtypeEnum);
            pushOpcode(state, op);

            if (op == OC_GET_BLOB) {
                DataType* dtype = getDtype(val); // TODO
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

    Err::Err compileMemberSelection(CompilerState* state, BinaryExpression* bex) {

        Variable* parent = bex->left;
        const DataTypeEnum parentType = parent->cvalue.dtypeEnum;
        // TODO

        return Err::OK;

    }

    Err::Err compileArraySize(CompilerState* state, Variable* var) {

        var = unwrap(var);

        Array* arr = var->cvalue.arr;
        arr->length = unwrap(arr->length);

        if (!arr->length) {
            Logger::log(logErr, "Array length expected!'.", var->base.span);
            return Err::UNEXPECTED_ERROR;
        }

        Err::Err err = compile(state, arr->length);
        if (err != Err::OK) return err;

        DataType* elemType = getDtype(&arr->length->cvalue);
        if (elemType->size > 1) {
            pushOpcode(state, OC_PUSH_U64);
            pushOperand(state, elemType->size);
            pushOpcode(state, OC_MUL_U64);
        }

        return Err::OK;

    }

    Err::Err compileInPlace(CompilerState* state, ArrayInitialization* init, Variable* target) {

        uint64_t elementSize;
        uint64_t elementAlign;
        Value element;
        element.any = target->cvalue.arr->base.pointsTo;
        element.dtypeEnum = target->cvalue.arr->base.pointsToEnum;
        getDtypeInfo(&element, &elementSize, &elementAlign);

        const uint64_t offset = target->def->vmOffset;

        for (int i = 0; i < init->attributes.base.size; i++) {

            pushOpcode(state, OC_LEA);
            pushOperand(state, offset);

            pushOpcode(state, OC_PUSH_U64);
            pushOperand(state, i);

            pushOpcode(state, OC_PTR_IDX);
            pushOperand(state, elementSize);

            Variable* var = *(Variable**) DArray::get(&init->attributes.base, i);
            Err::Err err = compile(state, var);
            if (err != Err::OK) return err;

            const Opcode storeOpcode = selectStoreOpcode(element.dtypeEnum);
            pushOpcode(state, storeOpcode);

        }

        return Err::OK;

    }

    // TODO : doesnt work for <val> +/- ptr, add OC_SWAP?
    inline bool tryAsPointerArithmetic(CompilerState* state, BinaryExpression* bex) {

        Variable* var = NULL;
        if (bex->left->cvalue.dtypeEnum == DT_POINTER ||
            bex->left->cvalue.dtypeEnum == DT_ARRAY) {
            var = bex->left;
        } else if (bex->right->cvalue.dtypeEnum == DT_POINTER ||
            bex->right->cvalue.dtypeEnum == DT_ARRAY) {
            var = bex->right;
        }

        if (!var) return false;

        const OperatorEnum op = bex->base.opType;
        if (op != OP_ADDITION && op != OP_SUBTRACTION) {
            return false;
        }

        uint64_t size;
        uint64_t align;
        Value val = toValue(var->cvalue.ptr);
        Err::Err err = getDtypeInfo(&val, &size, &align);
        if (err != Err::OK) return false;

        pushOpcode(state, OC_PUSH_I64);
        pushOperand(state, size);

        pushOpcode(state, OC_MUL_U64);

        Opcode oc = selectOperatorOpcode(bex);
        pushOpcode(state, oc);

        return true;
    }

    void pushDescriptor(CompilerState* state, VecDescriptor desc) {
        pushOperand(state, encodeVecDescriptor(desc));
    }

    inline bool tryVectorization(CompilerState* state, BinaryExpression* bex, VecResult lRes, VecResult rRes, Variable* target, const bool isRoot) {

        if (bex->left->cvalue.dtypeEnum != DT_ARRAY &&
            bex->right->cvalue.dtypeEnum != DT_ARRAY
        ) {
            return false;
        }

        uint64_t dest = 0;
        if (isRoot) {
            dest = target->def->vmOffset;
            state->vecResult.isTmp = false;
        } else {
            state->vecResult.isTmp = true;
        }

        VecDescriptor desc = {
            .oper = bex->base.opType,
            .flags = 
                (((uint32_t) state->vecResult.isTmp) << DE_F_DEST_SHIFT) | 
                (((uint32_t) lRes.isTmp) << DE_F_LEFT_SHIFT) |
                (((uint32_t) rRes.isTmp) << DE_F_RIGHT_SHIFT)
        };

        if (bex->left->cvalue.dtypeEnum == DT_ARRAY && bex->right->cvalue.dtypeEnum == DT_ARRAY) {
            desc.dtype = bex->left->cvalue.arr->base.pointsToEnum;
            if (bex->base.opType == OP_CONCATENATION) {
                pushOpcode(state, OC_VEC_CAT);
            } else {
                pushOpcode(state, OC_VEC_VV);
            }
        } else if (bex->left->cvalue.dtypeEnum == DT_ARRAY) {
            desc.dtype = bex->left->cvalue.arr->base.pointsToEnum;
            pushOpcode(state, OC_VEC_VS);
        } else {
            desc.dtype = bex->right->cvalue.arr->base.pointsToEnum;
            pushOpcode(state, OC_VEC_SV);
        }

        pushDescriptor(state, desc);
        pushOperand(state, dest);

        return true;

    }

    // Unary Version
    bool tryVectorization(CompilerState* state, UnaryExpression* uex, Variable* target, const bool isRoot) {

        if (uex->operand->cvalue.dtypeEnum != DT_ARRAY) return false;

        uint64_t dest = 0;
        if (isRoot) {
            dest = target->def->vmOffset;
            state->vecResult.isTmp = false;
        } else {
            state->vecResult.isTmp = true;
        }

        pushOpcode(state, OC_VEC_UNARY);

        VecDescriptor desc = {
            .dtype = uex->operand->cvalue.arr->base.pointsToEnum,
            .oper = uex->base.opType,
            .flags = ((uint32_t) state->vecResult.isTmp) << DE_F_DEST_SHIFT
        };

        pushDescriptor(state, desc);
        pushOperand(state, dest);

        return true;

    }

    Err::Err compile(CompilerState* state, Expression* node, Variable* target, Flags flags) {

        Err::Err err;

        const bool isRoot = flags & IS_ROOT;
        flags &= ~IS_ROOT;

        switch (node->type) {

            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) node;
                const bool areWeNothingburger = uex->base.opType == OP_NONE;

                if (areWeNothingburger && isRoot) flags |= IS_ROOT;

                err = compile(state, uex->operand, target, flags);
                if (err != Err::OK) return err;

                if (areWeNothingburger) break;

                if (flags & FORCE_VEC_OPCODES &&
                    tryVectorization(state, uex, target, isRoot)
                ) {
                    break;
                }

                if (uex->base.opType == OP_GET_VALUE && (flags & IS_LVALUE)) {
                    break;
                }

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

                if (bex->base.opType == OP_MEMBER_SELECTION) {
                    return compileMemberSelection(state, bex);
                }

                err = compile(state, bex->left, target, flags);
                if (err != Err::OK) return err;
                VecResult lResult = state->vecResult;

                err = compile(state, bex->right, target, flags);
                if (err != Err::OK) return err;
                VecResult rResult = state->vecResult;

                // if either side is an array - switch to vec opcodes
                if (flags & FORCE_VEC_OPCODES &&
                    tryVectorization(state, bex, lResult, rResult, target, isRoot)
                ) {
                    break;
                }

                if (tryAsPointerArithmetic(state, bex)) {
                    break;
                }

                Opcode oc = selectOperatorOpcode(bex);
                if (oc == OC_NOP) break;

                pushOpcode(state, oc);

                if (oc == OC_PTR_IDX) {
                    Pointer* ptr = bex->left->cvalue.ptr;
                    DataType* dtype = getDtype(ptr->pointsTo, ptr->pointsToEnum);
                    pushOperand(state, dtype->size);

                    if (!(flags & IS_LVALUE)) {
                        oc = selectLoadOpcode(ptr->pointsToEnum);
                        pushOpcode(state, oc);
                    }
                }

                break;

            }

            case EXT_FUNCTION_CALL: {

                FunctionCall* call = (FunctionCall*) node;
                Function* fcn = call->fcn;

                if (!isValidFunctionIdx(call->fcn->internalIdx) && !fcn->exe) {
                    err = compile(fcn);
                }

                // create empty slots for callee fp and ip
                pushOpcode(state, OC_GROW);
                pushOperand(state, 2 * sizeof(vmword));

                // predetermine if the last arg is vardic, so we
                // dont have to lookup in main loop prototype definition
                int fixedCount = fcn->prototype.inArgs.base.size;
                int varArgsCount = 0;
                bool isVariadic = false;

                if (fixedCount > 0) {
                    VariableDefinition* lastArgPrototype = *(VariableDefinition**) DArray::get(&fcn->prototype.inArgs.base, fixedCount - 1);
                    if (lastArgPrototype->var->cvalue.dtypeEnum == DT_MULTIPLE_TYPES) {
                        isVariadic = true;
                        fixedCount--;
                    }
                }

                for (int i = 0; i < fixedCount; i++) {
                    Variable* arg = *(Variable**) DArray::get(&call->inArgs.base, i);
                    err = compile(state, arg, NULL, FORCE_ARRAY_LENGTH);
                    if (err != Err::OK) return err;
                }

                bool anyVarargIsArray = false;
                if (isVariadic) {
                    for (int i = fixedCount; i < call->inArgs.base.size; i++) {
                        Variable* arg = *(Variable**) DArray::get(&call->inArgs.base, i);
                        compileAsAny(state, arg);
                        anyVarargIsArray = arg->cvalue.dtypeEnum == DT_ARRAY;
                        varArgsCount++;
                    }

                    pushOpcode(state, OC_PUSH_I64);
                    pushOperand(state, varArgsCount);
                }

                pushOpcode(state, OC_CALL);

                // TODO: for now we push pointer, later it would be nice
                //       to provide transferable solution, or at least
                //       a way to generate such solution
                pushOperand(state, (uint64_t) fcn);
                // TODO: for now this, but maybe separate opcode
                // (for the dump log)
                pushOperand(state, (uint64_t) (2 * varArgsCount));

                if (anyVarargIsArray) {
                    pushOpcode(state, OC_VEC_MEM_RESET);
                }

                break;

            }

            case EXT_CAST: {

                Cast* cast = (Cast*) node;

                if (flags & FORCE_VEC_OPCODES) {

                    compile(state, cast->operand, target, flags);

                    pushOpcode(state, OC_VEC_CAST);

                    VecDescriptor desc = {
                        .dtype = cast->target,
                        .srcDtype = cast->operand->cvalue.arr->base.pointsToEnum,
                        .flags = 0
                    };

                    if (isRoot) {
                        pushDescriptor(state, desc);
                        pushOperand(state, target->def->vmOffset);
                        state->vecResult.isTmp = false;
                    } else {
                        desc.flags = DE_F_DEST | DE_F_LEFT;
                        pushDescriptor(state, desc);
                        pushOperand(state, 0); // TODO
                        state->vecResult.isTmp = true;
                    }

                } else {

                    compile(state, cast->operand, target);

                    Opcode op = selectCastOpcode(cast->operand->cvalue.dtypeEnum, cast->target);
                    if (op == OC_NOP) break;

                    pushOpcode(state, op);

                }

                break;

            }

            case EXT_ALLOC: {

                Alloc* alc = (Alloc*) node;

                DataType* dtype = getDtype(&alc->def->var->cvalue);

                if (dtype->kind == DT_ARRAY) {
                    err = compileArraySize(state, alc->def->var);
                    if (err != Err::OK) return err;
                } else {
                    pushOpcode(state, OC_PUSH_U64);
                    pushOperand(state, dtype->size);
                }

                Function* fcn = Internal::functions + Internal::IF_ALLOC;
                pushOpcode(state, OC_CALL);
                pushOperand(state, (uint64_t) fcn);

                if (!alc->def->var->expression) break;

                // init part
                pushOpcode(state, OC_DUP); // as we expect pointer on stack
                compile(state, alc->def->var->expression);

                Opcode oc = selectStoreOpcode(dtype->kind);
                pushOpcode(state, oc);
                if (oc == OC_STORE_BLOB) {
                    pushOperand(state, dtype->size);
                }

                break;

            }

            case EXT_FREE: {
                // TODO
                break;
            }

            case EXT_GET_LENGTH: {

                GetLength* ex = (GetLength*) node;
                if (ex->arr->cvalue.arr->length) {
                    compile(state, ex->arr->cvalue.arr->length);
                } else {
                    pushOpcode(state, OC_GET_U64);
                    pushOperand(state, ex->arr->def->vmOffset + 8);
                }

                break;

            }

            case EXT_GET_SIZE: {

                GetSize* ex = (GetSize*) node;
                compile(state, ex->arr->cvalue.arr->length);
                /*
                DataType* dtype = getDtype(ex->arr->base.pointsTo, ex->arr->base.pointsToEnum);
                pushOperand(state, dtype->size);
                pushOpcode(state, OC_MUL_I64);
                */
                break;

            }

            case EXT_STRING_INITIALIZATION: {

                pushString(state, (StringInitialization*) node);
                break;

            }

            case EXT_ARRAY_INITIALIZATION: {

                ArrayInitialization* init = (ArrayInitialization*) node;

                const uint64_t elementCount = init->attributes.base.size;
                if (elementCount < 0) {
                    // TODO : can we even be there if not error in compiler
                    return Err::UNEXPECTED_ERROR;
                }

                if (init->flags & IS_CMP_TIME) {

                    Variable* first = *(Variable**)DArray::get(&init->attributes.base, 0);

                    uint64_t offset = state->rawData.logicalPos;
                    uint64_t elementSize = getDtypeSize(&first->cvalue);

                    uint8_t* rawDataPtr = (uint8_t*)Arena::push(&state->rawData, elementSize * elementSize, 1);
                    for (int i = 0; i < elementCount; i++) {
                        Variable* arg = *(Variable**)DArray::get(&init->attributes.base, i);
                        memcpy(rawDataPtr + (i * elementSize), &arg->cvalue.u64, elementSize);
                    }

                    pushOpcode(state, OC_LEA_CONST);
                    pushOperand(state, offset);
                    state->vecResult.isTmp = false;

                    if (flags & FORCE_ARRAY_LENGTH) {
                        pushOpcode(state, OC_PUSH_U64);
                        pushOperand(state, elementCount);
                    }

                    pushOpcode(state, OC_VEC_COPY);
                    VecDescriptor desc = {
                        .dtype = first->cvalue.dtypeEnum,
                    };
                    if (isRoot) {
                        pushDescriptor(state, desc);
                        pushOperand(state, target->def->vmOffset);
                    } else {
                        desc.flags = DE_F_DEST;
                        pushDescriptor(state, desc);
                        pushOperand(state, 0); // TODO
                    }

                } else {

                    uint64_t elementSize = 0;

                    for (int i = 0; i < elementCount; i++) {
                        Variable* arg = *(Variable**) DArray::get(& init->attributes.base, i);
                        compile(state, arg);

                        if (i == 0) {
                            elementSize = getDtypeSize(&arg->cvalue);
                        }

                        if (isRoot) {
                            pushOpcode(state, OC_STORE_INDEXED);
                            pushOperand(state, target->def->vmOffset);
                            pushOperand(state, (uint64_t) i);
                        } else {
                            pushOpcode(state, OC_STORE_INDEXED_TMP);
                            pushOperand(state, 0); // TODO
                            pushOperand(state, (uint64_t) i);
                        }
                    }

                    if (!isRoot) {
                        pushOpcode(state, OC_PUSH_U64);
                        pushOperand(state, elementCount);

                        pushOpcode(state, OC_VEC_ALLOC);
                        pushOperand(state, elementSize);

                        state->vecResult.isTmp = true;
                    }

                }

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

        updateSourceLocation(state, node->base.span);

        if (node->var) {
            Err::Err err = compile(state, node->var);
            if (err != Err::OK) return err;
        }

        pushOpcode(state, OC_RET);

        // TODO : not the best
        DataType* dtype = getDtype(&node->var->cvalue); // TODO
        pushOperand(state, dtype->size);

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

    Err::Err compile(CompilerState* state, Statement* node) {
        updateSourceLocation(state, node->base.span);
        compile(state, node->operand);
        return Err::OK;
    }

    Err::Err compile(CompilerState* state, Function* fcn) {

        Err::Err err;

        fcn->exe->isVariadic = false;
        for (int i = 0; i < fcn->prototype.inArgsCnt; i++) {

            VariableDefinition* def = *(VariableDefinition**) DArray::get(&fcn->prototype.inArgs.base, i);

            if (!state->populateLocals &&
                (def->var->cvalue.hasValue || def->var->expression)
            ) {
                state->populateLocals = true;
                state->fixedSize = state->locals.logicalPos;
                state->locals.logicalPos = 0;
            }

            if (def->var->cvalue.dtypeEnum == DT_MULTIPLE_TYPES) {
                fcn->exe->isVariadic = true;
            }

            err = compile(state, def);
            if (err != Err::OK) return err;

        }

        if (!state->populateLocals) {
            state->fixedSize = state->locals.logicalPos;
            state->locals.logicalPos = 0;
            state->populateLocals = true;
        }

        state->defaultArgsSize = state->locals.logicalPos;

        err = compile(state, fcn->bodyScope);
        if (err != Err::OK) return err;

        state->fixedSize += state->locals.logicalPos;

        if (state->lastOpcode != OC_RET) {
            pushOpcode(state, OC_RET);
            pushOperand(state, 0);
        }

        commitLineInfo(state);

        fcn->exe->bytecodeSize = state->bytecode.logicalPos;
        fcn->exe->bytecode = (uint8_t*) alloc(alc, Arena::getFlatSize(&state->bytecode), 1);
        Arena::flatCopy(&state->bytecode, fcn->exe->bytecode);

        fcn->exe->localsSize = BYTES_TO_WORDS(state->locals.logicalPos);
        fcn->exe->locals = (vmword*) alloc(alc, Arena::getFlatSize(&state->locals), state->maxAlign);
        memset(fcn->exe->locals, 0, fcn->exe->localsSize * sizeof(vmword));
        Arena::flatCopy(&state->locals, (uint8_t*) fcn->exe->locals);

        fcn->exe->rawDataSize = state->rawData.logicalPos;
        fcn->exe->rawData = (uint8_t*) alloc(alc, Arena::getFlatSize(&state->rawData), 1);
        Arena::flatCopy(&state->rawData, fcn->exe->rawData);

        fcn->exe->node = fcn;
        fcn->exe->localsInfoMap = OrderedDict::tightCopy(&state->localsInfoMap);

        fcn->exe->linesSize = state->lines.size;
        fcn->exe->lines = (LineInfo*) alloc(alc, fcn->exe->linesSize * sizeof(LineInfo));
        memcpy(fcn->exe->lines, state->lines.buffer, fcn->exe->linesSize * sizeof(LineInfo));

        fcn->exe->fixedSize = state->fixedSize;
        fcn->exe->defaultArgsSize = state->defaultArgsSize;

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

}
