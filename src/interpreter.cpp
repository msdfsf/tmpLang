// #pragma once

#include "interpreter.h"
#include "syntax.h"
#include "error.h"
#include "utils.h"

#include <bit>


#define TO_PVOID(var) ((void*) *((uint64_t*) &var))

namespace Interpreter {
    
    struct MemBlock {
        int offset;
        int value;
    };

    std::vector<MemBlock> mem;



    // IMAGINE WORLD POINTER DOESNT EXISTS!!!
    //
    //
    //
    //
    //
    //
    //














/*
int applyUnaryOperatorAddress(Operand* operand) {
    
    Pointer* ptr = new Pointer;
    ptr->pointsTo = operand->dtype;
    ptr->pointsToEnum = operand->cvalue.dtypeEnum;
    
    operand->cvalue.dtypeEnum = DT_POINTER;
    operand->dtype = (void*) ptr;
    
    return Err::OK;

}

int applyBinaryOperatorSubscript(Operand* a, Operand* b) {
    if (!IS_INT(b->cvalue.dtypeEnum)) return Err::INVALID_DATA_TYPE;
    return ((Pointer*) (a->dtype))->pointsToEnum; // TODO : do general solution!!!
}

int applyBinaryOperatorMemberSelection(Operand* ans, Operand* a, Operand* b) {
    
    if (b->cvalue.dtypeEnum != DT_MEMBER) return Err::INVALID_DATA_TYPE;

    Variable* var = Utils::find(((TypeDefinition*) a->dtype)->vars, ((Variable*) b)->name, ((Variable*) b)->nameLen);
    if (!var) return Err::INVALID_ATTRIBUTE_NAME;

    b->cvalue.dtypeEnum = var->cvalue.dtypeEnum; // dont know about this one
    b->dtype = var->dtype;
    ((Variable*) b)->id = var->id;
    ans->dtype = var->dtype;

    return Err::OK;

}

int applyBinaryOperatorMemberSelectionEnum(Operand* ans, Operand* a, Operand* b) {
    
    if (b->cvalue.dtypeEnum != DT_MEMBER) return Err::INVALID_DATA_TYPE;

    Variable* var = Utils::find(((Enumerator*) a->dtype)->vars, ((Variable*) b)->name, ((Variable*) b)->nameLen);
    if (!var) return Err::INVALID_ATTRIBUTE_NAME;

    ans->cvalue.dtypeEnum = var->cvalue.dtypeEnum; // dont know about this one
    ans->dtype = var->dtype;
    ans->cvalue.value = var->cvalue.value;
    ans->cvalue.hasValue = var->cvalue.hasValue;
    ans->def = var->def;
    ans->expression = NULL;
    ans->unrollExpression = 0;

    b->cvalue.dtypeEnum = var->cvalue.dtypeEnum;

    return Err::OK;

}

int applyBinaryOperatorMemberSelectionPointer(Operand* ans, Operand* a, Operand* b) {
    
    if (b->cvalue.dtypeEnum != DT_MEMBER) return Err::INVALID_DATA_TYPE;

    Pointer* ptr = (Pointer*) (a->dtype);
    if (ptr->pointsToEnum != DT_CUSTOM) return Err::INVALID_DATA_TYPE;

    TypeDefinition* td = (TypeDefinition*) (ptr->pointsTo);

    Variable* var = Utils::find(td->vars, ((Variable*) b)->name, ((Variable*) b)->nameLen);
    if (!var) return Err::INVALID_ATTRIBUTE_NAME;

    b->cvalue.dtypeEnum = var->cvalue.dtypeEnum; // dont know about this one
    b->dtype = var->dtype;
    ((Variable*) b)->id = var->id;
    ans->dtype = var->dtype;
    ((BinaryExpression*) (ans->expression))->operType = OP_DEREFERENCE_MEMBER_SELECTION;
    ((BinaryExpression*) (ans->expression))->oper = operators + OP_DEREFERENCE_MEMBER_SELECTION;

    return Err::OK;

}



// custom dypes
int applyUnaryOperatorPlusCustom(Operand* operand) {
    return Err::OK;
}


int applyUnaryOperatorMinusCustom(Operand* operand) {
    return Err::OK;
}

*/





















    // TODO : for now global
    uint64_t contextId = 1;
    uint64_t stackIdx = 1;

    // RR_RETURN has to be last, it defines offset of the 
    // index of the return statement that triggered early 
    // return in children nodes of calling node
    // hope make sense
    enum EarlyReturnReason {
        RR_BREAK = 1,
        RR_RETURN
    };


    

    inline void insertValue(Operand* op, Value value, const int idx) {
        if (op->istack.size() < idx + 1) {
            op->istack.resize(idx + 1);
        }
        op->istack[idx] = value;
    }

    // BUG : instad of push_back make sure to insert new value to aproprate index!!!
    inline void writeValue(Operand* op, Value value, int stackIdx) {
        if (stackIdx >= 2) {
            const int idx = stackIdx - 2;
            if (op->istack.size() <= idx) {
                op->istack.resize(idx + 1);
            }
            op->istack[idx] = value;
        } else {
            op->ivalue = value;
        }

    }

    inline void writeValue(Operand* op, Value value) {
        if (stackIdx >= 2) {
            const int idx = stackIdx - 2;
            if (op->istack.size() <= idx) {
                op->istack.resize(idx + 1); 
            }
            op->istack[idx] = value;
        } else {
            op->ivalue = value;
        }

    }

    inline Value* readValue(Operand* op) {
        return (stackIdx >= 2) ? &(op->istack[stackIdx - 2]) : &(op->ivalue);
    }

    inline int readHasValue(Operand* op) {
        return (stackIdx >= 2) ? op->istack[stackIdx - 2].hasValue : op->ivalue.hasValue;
    }



    // CPP CPP CPP :)

    template<DataTypeEnum>
    struct DataTypeToCppType;

    template<>
    struct DataTypeToCppType<DT_INT_8> {
        using type = int8_t;
        static constexpr auto member = &Value::i8;
    };

    template<>
    struct DataTypeToCppType<DT_INT_16> {
        using type = int16_t;
        static constexpr auto member = &Value::i16;
    };

    template<>
    struct DataTypeToCppType<DT_INT_32> {
        using type = int32_t;
        static constexpr auto member = &Value::i32;
    };

    template<>
    struct DataTypeToCppType<DT_INT_64> {
        using type = int64_t;
        static constexpr auto member = &Value::i64;
    };

    template<>
    struct DataTypeToCppType<DT_UINT_8> {
        using type = uint8_t;
        static constexpr auto member = &Value::u8;
    };

    template<>
    struct DataTypeToCppType<DT_UINT_16> {
        using type = uint16_t;
        static constexpr auto member = &Value::u16;
    };

    template<>
    struct DataTypeToCppType<DT_UINT_32> {
        using type = uint32_t;
        static constexpr auto member = &Value::u32;
    };

    template<>
    struct DataTypeToCppType<DT_UINT_64> {
        using type = uint64_t;
        static constexpr auto member = &Value::u64;
    };

    template<>
    struct DataTypeToCppType<DT_FLOAT_32> {
        using type = float;
        static constexpr auto member = &Value::f32;
    };

    template<>
    struct DataTypeToCppType<DT_FLOAT_64> {
        using type = double;
        static constexpr auto member = &Value::f64;
    };

    template<DataTypeEnum TargetType>
    inline void cast(Value* value) {

        using T = typename DataTypeToCppType<TargetType>::type;
        auto M = DataTypeToCppType<TargetType>::member;

        switch (value->dtypeEnum) {
            case DT_INT_8:      value->*M = static_cast<T>(value->i8);  break;
            case DT_INT_16:     value->*M = static_cast<T>(value->i16); break;
            case DT_INT:
            case DT_INT_32:     value->*M = static_cast<T>(value->i32); break;
            case DT_INT_64:     value->*M = static_cast<T>(value->i64); break;
            case DT_UINT_8:     value->*M = static_cast<T>(value->u8);  break;
            case DT_UINT_16:    value->*M = static_cast<T>(value->u16); break;
            case DT_UINT_32:    value->*M = static_cast<T>(value->u32); break;
            case DT_UINT_64:    value->*M = static_cast<T>(value->u64); break;
            case DT_FLOAT_32:   value->*M = static_cast<T>(value->f32); break;
            case DT_FLOAT_64:   value->*M = static_cast<T>(value->f64); break;
            default:
                return; // TODO
        
        }

        value->dtypeEnum = TargetType;

    }

    // There doesn't seem to be a clever trick to handle all casts
    // using some generic operation or bit-level hack.
    // So we're going with a straightforward double look-up approach.
    // A single look-up would be faster, but it's less readable and modular.
    // This way, we can still use specific cast functions in 'known-context'.
    void cast(Value* value, DataTypeEnum dtype) {

        switch (dtype) {
            
            case DT_INT_8 :     cast<DT_INT_8>(value); return;
            case DT_INT_16 :    cast<DT_INT_16>(value); return;
            case DT_INT : 
            case DT_INT_32 :    cast<DT_INT_32>(value); return;
            case DT_INT_64 :    cast<DT_INT_64>(value); return;
            case DT_UINT_8 :    cast<DT_UINT_8>(value); return;
            case DT_UINT_16 :   cast<DT_UINT_16>(value); return;
            case DT_UINT_32 :   cast<DT_UINT_32>(value); return;
            case DT_UINT_64 :   cast<DT_UINT_64>(value); return;
            case DT_FLOAT_32 :  cast<DT_FLOAT_32>(value); return;
            case DT_FLOAT_64 :  cast<DT_FLOAT_64>(value); return;
            case DT_STRING :    /* TODO */ return;
            case DT_POINTER :   /* TODO */ return;
            case DT_ARRAY :     /* TODO */ return;
            case DT_SLICE :     /* TODO */ return;
            case DT_CUSTOM :    /* TODO */ return;
            case DT_ENUM :      /* TODO */ return;
            case DT_MEMBER :    /* TODO */ return;
            case DT_UNION :     /* TODO */ return;
            default: return; // TODO

        }

    }



    inline void applyUnaryOperatorPlusI32(Value* value) {}
    inline void applyUnaryOperatorPlusI64(Value* value) {}
    inline void applyUnaryOperatorPlusF32(Value* value) {}
    inline void applyUnaryOperatorPlusF64(Value* value) {}



    inline void applyUnaryOperatorMinusI32(Value* value) {
        value->i32 = -value->i32;
    }

    inline void applyUnaryOperatorMinusI64(Value* value) {
        value->i64 = -value->i64;
    }

    inline void applyUnaryOperatorMinusF32(Value* value) {
        value->f32 = -value->f32;
    }

    inline void applyUnaryOperatorMinusF64(Value* value) {
        value->f64 = -value->f64;
    }



    inline void applyBinaryOperatorAdditionI32(Value* a, Value* b) {
        cast<DT_INT_32>(b);
        a->i32 = a->i32 + b->i32;
    }

    inline void applyBinaryOperatorAdditionI64(Value* a, Value* b) {
        cast<DT_INT_64>(b);
        a->i64 = a->i64 + b->i64;
    }

    inline void applyBinaryOperatorAdditionF32(Value* a, Value* b) {
        cast<DT_FLOAT_32>(b);
        a->f32 = a->f32 + b->f32;
    }

    inline void applyBinaryOperatorAdditionF64(Value* a, Value* b) {
        cast<DT_FLOAT_64>(b);
        a->f64 = a->f64 + b->f64;
    }

    inline void applyBinaryOperatorAdditionArray(Value* a, Value* b) {
    }

    inline void applyBinaryOperatorSubtractionArray(Value* a, Value* b) {
    }



    inline void applyBinaryOperatorSubtractionI32(Value* a, Value* b) {
        cast<DT_INT_32>(b);
        a->i32 = a->i32 - b->i32;
    }

    inline void applyBinaryOperatorSubtractionI64(Value* a, Value* b) {
        cast<DT_INT_64>(b);
        a->i64 = a->i64 - b->i64;
    }

    inline void applyBinaryOperatorSubtractionF32(Value* a, Value* b) {
        cast<DT_FLOAT_32>(b);
        a->f32 = a->f32 - b->f32;
    }

    inline void applyBinaryOperatorSubtractionF64(Value* a, Value* b) {
        cast<DT_FLOAT_64>(b);
        a->f64 = a->f64 - b->f64;
    }


    inline void applyBinaryOperatorMultiplicationI32(Value* a, Value* b) {
        cast<DT_INT_32>(b);
        a->i32 = a->i32 * b->i32;
    }

    inline void applyBinaryOperatorMultiplicationI64(Value* a, Value* b) {
        cast<DT_INT_64>(b);
        a->i64 = a->i64 * b->i64;
    }

    inline void applyBinaryOperatorMultiplicationF32(Value* a, Value* b) {
        cast<DT_FLOAT_32>(b);
        a->f32 = a->f32 * b->f32;
    }

    inline void applyBinaryOperatorMultiplicationF64(Value* a, Value* b) {
        cast<DT_FLOAT_64>(b);
        a->f64 = a->f64 * b->f64;
    }



    inline void applyBinaryOperatorDivisionI32(Value* a, Value* b) {
        cast<DT_INT_32>(b);
        a->i32 = a->i32 / b->i32;
    }

    inline void applyBinaryOperatorDivisionI64(Value* a, Value* b) {
        cast<DT_INT_64>(b);
        a->i64 = a->i64 / b->i64;
    }

    inline void applyBinaryOperatorDivisionF32(Value* a, Value* b) {
        cast<DT_FLOAT_32>(b);
        a->f32 = a->f32 / b->f32;
    }

    inline void applyBinaryOperatorDivisionF64(Value* a, Value* b) {
        cast<DT_FLOAT_64>(b);
        a->f64 = a->f64 / b->f64;
    }



    inline void applyBinaryOperatorModuloI32(Value* a, Value* b) {
        cast<DT_INT_32>(b);
        a->i32 = a->i32 % b->i32;
    }

    inline void applyBinaryOperatorModuloI64(Value* a, Value* b) {
        cast<DT_INT_64>(b);
        a->i64 = a->i64 % b->i64;
    }



    inline void applyBinaryOperatorEqual(Value* a, Value* b) {
        a->i64 = a->i64 == b->i64;
    }

    inline void applyBinaryOperatorNotEqual(Value* a, Value* b) {
        a->i64 = a->i64 != b->i64;
    }

    inline void applyBinaryOperatorLessThan(Value* a, Value* b) {
        a->i64 = a->i64 < b->i64;
    }

    inline void applyBinaryOperatorGreaterThan(Value* a, Value* b) {
        a->i64 = a->i64 > b->i64;
    }

    inline void applyBinaryOperatorLessThanOrEqual(Value* a, Value* b) {
        a->i64 = a->i64 <= b->i64;
    }

    inline void applyBinaryOperatorGreaterThanOrEqual(Value* a, Value* b) {
        a->i64 = a->i64 >= b->i64;
    }

    inline void applyBinaryOperatorBoolAnd(Value* a, Value* b) {
        a->i64 = a->i64 && b->i64;
    }

    inline void applyBinaryOperatorBoolOr(Value* a, Value* b) {
        a->i64 = a->i64 || b->i64;
    }



    inline void applyUnaryOperatorAddress(Value* operand) {
    }

    inline void applyBinaryOperatorSubscript(Value* a, Value* b) {
    }

    inline void applyBinaryOperatorMemberSelection(Value* a, Value* b) {
    }

    inline void applyBinaryOperatorMemberSelectionEnum(Value* a, Value* b) {
    }

    inline void applyBinaryOperatorMemberSelectionPointer(Value* a, Value* b) {
    }


    //void (*operatorFunctions[]) (Operand*, Operand*) 
    union OperatorFunction {
        void (*binary) (Value*, Value*);
        void (*unary) (Value*);
    };

    OperatorFunction operatorFunctions[] = {

        // VOID
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        // INT
        { .unary = &applyUnaryOperatorPlusI32 },
        { .unary = &applyUnaryOperatorMinusI32 },
        &applyBinaryOperatorAdditionI32,
        &applyBinaryOperatorSubtractionI32,
        &applyBinaryOperatorMultiplicationI32,
        &applyBinaryOperatorDivisionI32,
        &applyBinaryOperatorModuloI32,
        
        { .unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,
        
        &applyBinaryOperatorEqual,
        &applyBinaryOperatorNotEqual,
        &applyBinaryOperatorLessThan,
        &applyBinaryOperatorGreaterThan,
        &applyBinaryOperatorLessThanOrEqual,
        &applyBinaryOperatorGreaterThanOrEqual,
        
        &applyBinaryOperatorBoolAnd,
        &applyBinaryOperatorBoolOr,
        
        NULL,
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        // INT_8
        {.unary = &applyUnaryOperatorPlusI32 },
        {.unary = &applyUnaryOperatorMinusI32 },
        & applyBinaryOperatorAdditionI32,
        & applyBinaryOperatorSubtractionI32,
        & applyBinaryOperatorMultiplicationI32,
        & applyBinaryOperatorDivisionI32,
        & applyBinaryOperatorModuloI32,
        
        {.unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,

        & applyBinaryOperatorEqual,
        & applyBinaryOperatorNotEqual,
        & applyBinaryOperatorLessThan,
        & applyBinaryOperatorGreaterThan,
        & applyBinaryOperatorLessThanOrEqual,
        & applyBinaryOperatorGreaterThanOrEqual,
        & applyBinaryOperatorBoolAnd,
        & applyBinaryOperatorBoolOr,
        
        NULL,
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        // INT_16
        {.unary = &applyUnaryOperatorPlusI32 },
        {.unary = &applyUnaryOperatorMinusI32 },
        & applyBinaryOperatorAdditionI32,
        & applyBinaryOperatorSubtractionI32,
        & applyBinaryOperatorMultiplicationI32,
        & applyBinaryOperatorDivisionI32,
        & applyBinaryOperatorModuloI32,
        
        {.unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,
        
        & applyBinaryOperatorEqual,
        & applyBinaryOperatorNotEqual,
        & applyBinaryOperatorLessThan,
        & applyBinaryOperatorGreaterThan,
        & applyBinaryOperatorLessThanOrEqual,
        & applyBinaryOperatorGreaterThanOrEqual,
        & applyBinaryOperatorBoolAnd,
        & applyBinaryOperatorBoolOr,
        
        NULL,
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        // INT_32
        { .unary = &applyUnaryOperatorPlusI32 },
        { .unary = &applyUnaryOperatorMinusI32 },
        &applyBinaryOperatorAdditionI32,
        &applyBinaryOperatorSubtractionI32,
        &applyBinaryOperatorMultiplicationI32,
        &applyBinaryOperatorDivisionI32,
        &applyBinaryOperatorModuloI32,
        
        { .unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,
        
        &applyBinaryOperatorEqual,
        &applyBinaryOperatorNotEqual,
        &applyBinaryOperatorLessThan,
        &applyBinaryOperatorGreaterThan,
        &applyBinaryOperatorLessThanOrEqual,
        &applyBinaryOperatorGreaterThanOrEqual,
        &applyBinaryOperatorBoolAnd,
        &applyBinaryOperatorBoolOr,
        
        NULL,
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        
        // INT_64
        { .unary = &applyUnaryOperatorPlusI64 },
        { .unary = &applyUnaryOperatorMinusI64 },
        &applyBinaryOperatorAdditionI64,
        &applyBinaryOperatorSubtractionI64,
        &applyBinaryOperatorMultiplicationI64,
        &applyBinaryOperatorDivisionI64,
        &applyBinaryOperatorModuloI64,
        
        { .unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,
        
        &applyBinaryOperatorEqual,
        &applyBinaryOperatorNotEqual,
        &applyBinaryOperatorLessThan,
        &applyBinaryOperatorGreaterThan,
        &applyBinaryOperatorLessThanOrEqual,
        &applyBinaryOperatorGreaterThanOrEqual,
        &applyBinaryOperatorBoolAnd,
        &applyBinaryOperatorBoolOr,
        
        NULL,
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        // UINT_8
        { .unary = &applyUnaryOperatorPlusI32 },
        { .unary = &applyUnaryOperatorMinusI32 },
        & applyBinaryOperatorAdditionI32,
        & applyBinaryOperatorSubtractionI32,
        & applyBinaryOperatorMultiplicationI32,
        & applyBinaryOperatorDivisionI32,
        & applyBinaryOperatorModuloI32,

        { .unary = &applyUnaryOperatorAddress },
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,

        & applyBinaryOperatorEqual,
        & applyBinaryOperatorNotEqual,
        & applyBinaryOperatorLessThan,
        & applyBinaryOperatorGreaterThan,
        & applyBinaryOperatorLessThanOrEqual,
        & applyBinaryOperatorGreaterThanOrEqual,
        & applyBinaryOperatorBoolAnd,
        & applyBinaryOperatorBoolOr,

        NULL,
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        // UINT_16
        { .unary = &applyUnaryOperatorPlusI32 },
        { .unary = &applyUnaryOperatorMinusI32 },
        & applyBinaryOperatorAdditionI32,
        & applyBinaryOperatorSubtractionI32,
        & applyBinaryOperatorMultiplicationI32,
        & applyBinaryOperatorDivisionI32,
        & applyBinaryOperatorModuloI32,

        { .unary = &applyUnaryOperatorAddress },
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,

        & applyBinaryOperatorEqual,
        & applyBinaryOperatorNotEqual,
        & applyBinaryOperatorLessThan,
        & applyBinaryOperatorGreaterThan,
        & applyBinaryOperatorLessThanOrEqual,
        & applyBinaryOperatorGreaterThanOrEqual,
        & applyBinaryOperatorBoolAnd,
        & applyBinaryOperatorBoolOr,

        NULL,
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        // UINT_32
        { .unary = &applyUnaryOperatorPlusI32 },
        { .unary = &applyUnaryOperatorMinusI32 },
        & applyBinaryOperatorAdditionI32,
        & applyBinaryOperatorSubtractionI32,
        & applyBinaryOperatorMultiplicationI32,
        & applyBinaryOperatorDivisionI32,
        & applyBinaryOperatorModuloI32,

        { .unary = &applyUnaryOperatorAddress },
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,

        & applyBinaryOperatorEqual,
        & applyBinaryOperatorNotEqual,
        & applyBinaryOperatorLessThan,
        & applyBinaryOperatorGreaterThan,
        & applyBinaryOperatorLessThanOrEqual,
        & applyBinaryOperatorGreaterThanOrEqual,
        & applyBinaryOperatorBoolAnd,
        & applyBinaryOperatorBoolOr,

        NULL,
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        // UINT_64
        { .unary = &applyUnaryOperatorPlusI64 },
        { .unary = &applyUnaryOperatorMinusI64 },
        & applyBinaryOperatorAdditionI64,
        & applyBinaryOperatorSubtractionI64,
        & applyBinaryOperatorMultiplicationI64,
        & applyBinaryOperatorDivisionI64,
        & applyBinaryOperatorModuloI64,

        { .unary = &applyUnaryOperatorAddress },
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,

        & applyBinaryOperatorEqual,
        & applyBinaryOperatorNotEqual,
        & applyBinaryOperatorLessThan,
        & applyBinaryOperatorGreaterThan,
        & applyBinaryOperatorLessThanOrEqual,
        & applyBinaryOperatorGreaterThanOrEqual,
        & applyBinaryOperatorBoolAnd,
        & applyBinaryOperatorBoolOr,

        NULL,
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        
        // FLOAT_32
        { .unary = &applyUnaryOperatorPlusF32 },
        { .unary = &applyUnaryOperatorMinusF32 },
        &applyBinaryOperatorAdditionF32,
        &applyBinaryOperatorSubtractionF32,
        &applyBinaryOperatorMultiplicationF32,
        &applyBinaryOperatorDivisionF32,
        NULL,
        
        { .unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,
        
        &applyBinaryOperatorEqual,
        &applyBinaryOperatorNotEqual,
        &applyBinaryOperatorLessThan,
        &applyBinaryOperatorGreaterThan,
        &applyBinaryOperatorLessThanOrEqual,
        &applyBinaryOperatorGreaterThanOrEqual,
        &applyBinaryOperatorBoolAnd,
        &applyBinaryOperatorBoolOr,
        
        NULL,
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        
        // FLOAT_64
        { .unary = &applyUnaryOperatorPlusF64 },
        { .unary = &applyUnaryOperatorMinusF64 },
        &applyBinaryOperatorAdditionF64,
        &applyBinaryOperatorSubtractionF64,
        &applyBinaryOperatorMultiplicationF64,
        &applyBinaryOperatorDivisionF64,
        NULL,
        
        { .unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,
        
        &applyBinaryOperatorEqual,
        &applyBinaryOperatorNotEqual,
        &applyBinaryOperatorLessThan,
        &applyBinaryOperatorGreaterThan,
        &applyBinaryOperatorLessThanOrEqual,
        &applyBinaryOperatorGreaterThanOrEqual,
        &applyBinaryOperatorBoolAnd,
        &applyBinaryOperatorBoolOr,
        
        NULL,
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        
        // STRING
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        
        { .unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        
        NULL,
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        
        // POINTER
        NULL,
        NULL,
        &applyBinaryOperatorAdditionArray,
        &applyBinaryOperatorSubtractionArray,
        NULL,
        NULL,
        NULL,
        
        { .unary = &applyUnaryOperatorAddress },        
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,
        
        &applyBinaryOperatorSubscript,
        &applyBinaryOperatorMemberSelectionPointer,
        NULL,
        
        NULL,
        NULL,
        
        // ARRAY
        NULL,
        NULL,
        &applyBinaryOperatorAdditionArray,
        &applyBinaryOperatorSubtractionArray,
        NULL,
        NULL,
        NULL,
        { .unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,
        
        &applyBinaryOperatorSubscript,
        &applyBinaryOperatorMemberSelectionPointer,
        NULL,
        
        NULL,
        NULL,

        // SLICE
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        
        // MULTIPLE_TYPES
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        { .unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        
        // CUSTOM
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        { .unary = &applyUnaryOperatorAddress },
        NULL,
        
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,

        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        NULL,
        NULL,
        
        NULL,
        &applyBinaryOperatorMemberSelection,
        NULL,
        
        NULL,
        NULL,

        // UNION
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        // ERROR
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,

        // MEMBER
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        
        // ENUM
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        &applyBinaryOperatorMemberSelectionEnum,
        NULL,
        NULL,
        NULL,
    
    };




    int applyOperator(OperatorEnum oper, Value* value) {
        
        OperatorFunction fcn = operatorFunctions[value->dtypeEnum * OPERATORS_COUNT + oper]; 
        fcn.unary(value);
        return Err::OK;

    }

    int applyOperator(OperatorEnum oper, Value* valueA, Value* valueB) {
        
        OperatorFunction fcn = operatorFunctions[valueA->dtypeEnum * OPERATORS_COUNT + oper];
        fcn.binary(valueA, valueB);
        return Err::OK;

    }

    int applyOperator(OperatorEnum oper, Operand* operand) {
        
        OperatorFunction fcn = operatorFunctions[operand->ivalue.dtypeEnum * OPERATORS_COUNT + oper]; 
        fcn.unary(readValue(operand));
        return Err::OK;

    }

    int applyOperator(OperatorEnum oper, Operand* operandA, Operand* operandB) {
        
        OperatorFunction fcn = operatorFunctions[operandA->ivalue.dtypeEnum * OPERATORS_COUNT + oper];
        fcn.binary(readValue(operandA), readValue(operandB));
        return Err::OK;

    }

    inline void icopy(Operand* dest, Operand* src) {
        writeValue(dest, *readValue(src));
    }

    inline void copy(Operand* dest, Operand* src) {
        writeValue(dest, src->cvalue);
    }

    inline int evaluate(Operand* op) {

        if (op->expression == NULL) {

            if (!op->def) {
                copy(op, op);
                // writeValue(op, op->value);
                // op->idtypeEnum = op->dtypeEnum;
                return Err::OK;
            }

            Operand* defOp = op->def->var;
            
            if ((defOp->istack.size() > stackIdx - 2) && (readHasValue(defOp) == contextId)) {
                icopy(op, defOp);
            } else {
                copy(op, defOp);
            }

            return Err::OK;

        }

        Expression* ex = op->expression;
        switch (ex->type) {
            
            case EXT_WRAPPER : {
                WrapperExpression* wex = (WrapperExpression*) ex;
                if (wex->operand) evaluate(wex->operand);
                icopy(op, wex->operand);     
                break;
            }

            case EXT_UNARY : {
                UnaryExpression* uex = (UnaryExpression*) ex;
                if (uex->operand) evaluate(uex->operand);
                applyOperator(uex->operType, uex->operand);
                icopy(op, uex->operand);
                break;
            }

            case EXT_BINARY : {
                BinaryExpression* bex = (BinaryExpression*) ex;
                if (bex->operandA) evaluate(bex->operandA);
                if (bex->operandB) evaluate(bex->operandB);
                applyOperator(bex->operType, bex->operandA, bex->operandB);
                icopy(op, bex->operandA);
                break;
            }

            case EXT_TERNARY : {
                break;
            }

            case EXT_FUNCTION_CALL : {
                FunctionCall* fex = (FunctionCall*) ex;
                Function* const fcn = fex->fcn;
                for (int i = 0; i < fcn->inArgs.size(); i++) {

                    evaluate(fex->inArgs[i]);

                    Value* value = readValue(fex->inArgs[i]);
                    value->hasValue = contextId + 1;

                    //const int tmp = fex->inArgs[i]->ivalue.hasValue;
                    //fex->inArgs[i]->ivalue.hasValue = contextId + 1;
                    writeValue(fcn->inArgs[i]->var, *value, fcn->istackIdx + 1);
                    //fex->inArgs[i]->ivalue.hasValue = tmp;

                }
                execFunction(fcn, op);
                break;
            }

        }

        return Err::OK;

    }





    // All exec prefixed functions return index of last
    // node in children array of the input node on sucess
    // otherwise they return error value (which are negative)

    inline int execBranch(Branch* node);
    inline int execFunction(Function* node);
    inline int execVariableAssignment(VariableAssignment* node);
    inline int execVariableDefinition(VariableDefinition* node);
    inline int execReturnStatement(ReturnStatement* node);
    inline int execScope(Scope* node);



    inline int execBranch(Branch* node) {

        for (int i = 0; i < node->expressions.size(); i++){
            evaluate(node->expressions[i]);
            if (readValue(node->expressions[i])->i32) {
                return execScope(node->scopes[i]);
            }
        }

        if (node->scopes.size() > node->expressions.size()) {
            return execScope(node->scopes[node->scopes.size() - 1]);;
        }

        return Err::OK;

    }
    
    inline int execVariableDefinition(VariableDefinition* node) {
        evaluate(node->var);
        //copy(node->var->def->var, node->var);
        return Err::OK;
    }
    
    inline int execVariableAssignment(VariableAssignment* node) {

        evaluate(node->rvar);
        node->lvar->ivalue = node->rvar->ivalue;
        return Err::OK;

    }

    inline int execReturnStatement(ReturnStatement* node) {

        evaluate(node->var);
        return RR_RETURN + node->idx;

    }

    inline int execScope(Scope* node) {
           
        std::vector<SyntaxNode*> nodes = node->children;
        for (int i = 0; i < nodes.size(); i++) {

            SyntaxNode* const node = nodes[i];
            
            int err = Err::OK;
            switch (nodes[i]->type) {

                case NT_BRANCH :
                    err = execBranch((Branch*) node);
                    break;
                
                case NT_VARIABLE_DEFINITION : 
                    err = execVariableDefinition((VariableDefinition*) node);
                    break;

                case NT_VARIABLE_ASSIGNMENT : 
                    err = execVariableAssignment((VariableAssignment*) node);
                    break;

                case NT_RETURN_STATEMENT :
                    err = execReturnStatement((ReturnStatement*) node);
                    break;

            }

            if (err > Err::OK) return err;

        }

        return Err::OK;

    }

    int execFunction(Function* fcn, Operand* ans) {

        fcn->icnt++;
        fcn->istackIdx++;

        contextId = fcn->icnt;
        stackIdx = fcn->istackIdx;

        const int idx = execScope(fcn->bodyScope);
        if (idx < 0) return idx;
        
        if (idx >= RR_RETURN) {
            ReturnStatement* const rt = (ReturnStatement*) fcn->returns[idx - RR_RETURN];
            if (stackIdx > 2) {
                insertValue(ans, rt->var->istack[stackIdx - 2], stackIdx - 3);
            } else if (stackIdx == 2) {
                ans->ivalue = rt->var->istack[stackIdx - 2];
            } else {
                ans->cvalue = rt->var->ivalue;
            }
            // ans->dtypeEnum = rt->vars[0]->dtypeEnum;
        }

        fcn->istackIdx--;
        stackIdx = fcn->istackIdx;

        return ans->cvalue.dtypeEnum;

    }



    



}
