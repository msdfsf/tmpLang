#include "vec_kernel.h"
#include <cstdint>

// Here we generate some stuff to generate dispatch table dtype x operator
//

// --- Core function macros
//

#define GEN_VEC_FCN_B(name, dtype, op) \
    void name(void* vout, void* va, void* vb, const int len) { \
        dtype* out = (dtype*)vout; dtype* a = (dtype*)va; dtype* b = (dtype*)vb; \
        for (int i = 0; i < len; i++) out[i] = a[i] op b[i]; \
    }

#define GEN_VEC_FCN_VS(name, dtype, op) \
    void name(void* vout, void* va, uint64_t vs, const int len) { \
        dtype* out = (dtype*)vout; dtype* a = (dtype*)va; dtype s = (dtype)vs; \
        for (int i = 0; i < len; i++) out[i] = a[i] op s; \
    }

#define GEN_VEC_FCN_SV(name, dtype, op) \
    void name(void* vout, void* va, uint64_t vs, const int len) { \
        dtype* out = (dtype*)vout; dtype* a = (dtype*)va; dtype s = (dtype)vs; \
        for (int i = 0; i < len; i++) out[i] = s op a[i]; \
    }

#define GEN_VEC_FCN_U(name, dtype, op) \
    void name(void* vout, void* va, const int len) { \
        dtype* out = (dtype*)vout; dtype* a = (dtype*)va; \
        for (int i = 0; i < len; i++) out[i] = op a[i]; \
    }

#define GEN_VEC_FCN_C(name, dest, src) \
    void name(void* vout, void* va, const int len) { \
        dest* out = (dest*)vout; src* a = (src*)va; \
        for(int i = 0; i < len; i++) out[i] = (dest)a[i]; \
    }

#define GEN_VEC_FCN_F(name, dtype) \
    void name(void* vout, uint64_t vval, const int len) { \
        dtype* out = (dtype*)vout; dtype val = *((dtype*) &vval);\
        for(int i = 0; i < len; i++) out[i] = val; \
    }

// Just a helper to unite table and function generation
#define GEN_VEC_FCN_NAME(ftag, tname, oname) vec##ftag##oname##tname



// --- Expand each function by type
//

#define GEN_VEC_FCN_BY_TYPE(ftag, oname, dtype, tname, op) \
    GEN_VEC_FCN_##ftag(GEN_VEC_FCN_NAME(ftag, tname, oname), dtype, op)



// --- Expand each typed function by operator
//

#define GEN_VEC_FCN_BY_OPER_INT(ftag, oname, op) \
    GEN_VEC_FCN_BY_TYPE(ftag, oname, int8_t,   I8,  op) \
    GEN_VEC_FCN_BY_TYPE(ftag, oname, int16_t,  I16, op) \
    GEN_VEC_FCN_BY_TYPE(ftag, oname, int32_t,  I32, op) \
    GEN_VEC_FCN_BY_TYPE(ftag, oname, int64_t,  I64, op) \
    GEN_VEC_FCN_BY_TYPE(ftag, oname, uint8_t,  U8,  op) \
    GEN_VEC_FCN_BY_TYPE(ftag, oname, uint16_t, U16, op) \
    GEN_VEC_FCN_BY_TYPE(ftag, oname, uint32_t, U32, op) \
    GEN_VEC_FCN_BY_TYPE(ftag, oname, uint64_t, U64, op)

#define GEN_VEC_FCN_BY_OPER(ftag, oname, op) \
    GEN_VEC_FCN_BY_OPER_INT(ftag, oname, op) \
    GEN_VEC_FCN_BY_TYPE(ftag, oname, float,  F32, op) \
    GEN_VEC_FCN_BY_TYPE(ftag, oname, double, F64, op)

#define GEN_VEC_FCN_BY_TAG(ftag) \
    GEN_VEC_FCN_BY_OPER(ftag, Add, +) \
    GEN_VEC_FCN_BY_OPER(ftag, Sub, -) \
    GEN_VEC_FCN_BY_OPER(ftag, Mul, *) \
    GEN_VEC_FCN_BY_OPER(ftag, Div, /) \
    GEN_VEC_FCN_BY_OPER_INT(ftag, Mod, %) \
    GEN_VEC_FCN_BY_OPER_INT(ftag, And, &) \
    GEN_VEC_FCN_BY_OPER_INT(ftag, Or, |) \
    GEN_VEC_FCN_BY_OPER_INT(ftag, Xor, ^) \
    GEN_VEC_FCN_BY_OPER(ftag, BoolAnd, &&) \
    GEN_VEC_FCN_BY_OPER(ftag, BoolOr, ||) \
    GEN_VEC_FCN_BY_OPER_INT(ftag, Shr, >>) \
    GEN_VEC_FCN_BY_OPER_INT(ftag, Shl, <<) \
    GEN_VEC_FCN_BY_OPER(ftag, Eq, ==) \
    GEN_VEC_FCN_BY_OPER(ftag, Neq, !=) \
    GEN_VEC_FCN_BY_OPER(ftag, Lte, <=) \
    GEN_VEC_FCN_BY_OPER(ftag, Gte, >=) \
    GEN_VEC_FCN_BY_OPER(ftag, Lt, <) \
    GEN_VEC_FCN_BY_OPER(ftag, Gt, >)
    //GEN_VEC_FCN_BY_OPER(ftag, Dot, .) \
    //GEN_VEC_FCN_BY_OPER(ftag, Arrow, ->)



// --- For cast we have to have own case
//

#define GEN_VEC_CAST_BY_DEST(dest, name) \
    GEN_VEC_FCN_C(vecCI8##name##,  dest, int8_t)   \
    GEN_VEC_FCN_C(vecCI16##name##, dest, int16_t)  \
    GEN_VEC_FCN_C(vecCI32##name##, dest, int32_t)  \
    GEN_VEC_FCN_C(vecCI64##name##, dest, int64_t)  \
    GEN_VEC_FCN_C(vecCU8##name##,  dest, uint8_t)  \
    GEN_VEC_FCN_C(vecCU16##name##, dest, uint16_t) \
    GEN_VEC_FCN_C(vecCU32##name##, dest, uint32_t) \
    GEN_VEC_FCN_C(vecCU64##name##, dest, uint64_t) \
    GEN_VEC_FCN_C(vecCF32##name##, dest, float)    \
    GEN_VEC_FCN_C(vecCF64##name##, dest, double)



// --- Generate all functions
//

GEN_VEC_FCN_BY_TAG(B)
GEN_VEC_FCN_BY_TAG(VS)
GEN_VEC_FCN_BY_TAG(SV)

GEN_VEC_FCN_BY_OPER(U, Add, +)
GEN_VEC_FCN_BY_OPER(U, Sub, -)
//GEN_VEC_BY_OPER(U, Addr, &)
//GEN_VEC_BY_OPER(U, Value, *)
GEN_VEC_FCN_BY_OPER_INT(U, Neg, ~)
GEN_VEC_FCN_BY_OPER(U, Inc, ++)
GEN_VEC_FCN_BY_OPER(U, Dec, --)
GEN_VEC_FCN_BY_OPER(U, BoolNeg, !)

GEN_VEC_CAST_BY_DEST(int8_t,   I8)
GEN_VEC_CAST_BY_DEST(int16_t,  I16)
GEN_VEC_CAST_BY_DEST(int32_t,  I32)
GEN_VEC_CAST_BY_DEST(int64_t,  I64)
GEN_VEC_CAST_BY_DEST(uint8_t,  U8)
GEN_VEC_CAST_BY_DEST(uint16_t, U16)
GEN_VEC_CAST_BY_DEST(uint32_t, U32)
GEN_VEC_CAST_BY_DEST(uint64_t, U64)
GEN_VEC_CAST_BY_DEST(float,    F32)
GEN_VEC_CAST_BY_DEST(double,   F64)



// --- Generate tables
//

#define GEN_VEC_FCN_NAME_BY_OPER_INT(ftag, oname) \
    GEN_VEC_FCN_NAME(ftag, I8,  oname), \
    GEN_VEC_FCN_NAME(ftag, I16, oname), \
    GEN_VEC_FCN_NAME(ftag, I32, oname), \
    GEN_VEC_FCN_NAME(ftag, I64, oname), \
    GEN_VEC_FCN_NAME(ftag, U8,  oname), \
    GEN_VEC_FCN_NAME(ftag, U16, oname), \
    GEN_VEC_FCN_NAME(ftag, U32, oname), \
    GEN_VEC_FCN_NAME(ftag, U64, oname), \
    NULL, \
    NULL,

#define GEN_VEC_FCN_NAME_BY_OPER(ftag, oname) \
    GEN_VEC_FCN_NAME(ftag, I8,  oname), \
    GEN_VEC_FCN_NAME(ftag, I16, oname), \
    GEN_VEC_FCN_NAME(ftag, I32, oname), \
    GEN_VEC_FCN_NAME(ftag, I64, oname), \
    GEN_VEC_FCN_NAME(ftag, U8,  oname), \
    GEN_VEC_FCN_NAME(ftag, U16, oname), \
    GEN_VEC_FCN_NAME(ftag, U32, oname), \
    GEN_VEC_FCN_NAME(ftag, U64, oname), \
    GEN_VEC_FCN_NAME(ftag, F32, oname), \
    GEN_VEC_FCN_NAME(ftag, F64, oname),

#define GEN_VEC_FCN_NAME_BY_DEST(tname) \
    GEN_VEC_FCN_NAME(C, tname, I8),  \
    GEN_VEC_FCN_NAME(C, tname, I16), \
    GEN_VEC_FCN_NAME(C, tname, I32), \
    GEN_VEC_FCN_NAME(C, tname, I64), \
    GEN_VEC_FCN_NAME(C, tname, U8),  \
    GEN_VEC_FCN_NAME(C, tname, U16), \
    GEN_VEC_FCN_NAME(C, tname, U32), \
    GEN_VEC_FCN_NAME(C, tname, U64), \
    GEN_VEC_FCN_NAME(C, tname, F32), \
    GEN_VEC_FCN_NAME(C, tname, F64),

VecFunctionBinary vecDispatchBinary[] = {
    GEN_VEC_FCN_NAME_BY_OPER(B, Add)
    GEN_VEC_FCN_NAME_BY_OPER(B, Sub)
    GEN_VEC_FCN_NAME_BY_OPER(B, Mul)
    GEN_VEC_FCN_NAME_BY_OPER(B, Div)
    GEN_VEC_FCN_NAME_BY_OPER_INT(B, Mod)
    GEN_VEC_FCN_NAME_BY_OPER_INT(B, And)
    GEN_VEC_FCN_NAME_BY_OPER_INT(B, Or)
    GEN_VEC_FCN_NAME_BY_OPER_INT(B, Xor)
    GEN_VEC_FCN_NAME_BY_OPER(B, BoolAnd)
    GEN_VEC_FCN_NAME_BY_OPER(B, BoolOr)
    GEN_VEC_FCN_NAME_BY_OPER_INT(B, Shr)
    GEN_VEC_FCN_NAME_BY_OPER_INT(B, Shl)
    GEN_VEC_FCN_NAME_BY_OPER(B, Eq)
    GEN_VEC_FCN_NAME_BY_OPER(B, Neq)
    GEN_VEC_FCN_NAME_BY_OPER(B, Lte)
    GEN_VEC_FCN_NAME_BY_OPER(B, Gte)
    GEN_VEC_FCN_NAME_BY_OPER(B, Lt)
    GEN_VEC_FCN_NAME_BY_OPER(B, Gt)
};

VecFunctionScalar vecDispatchScalarR[] = {
    GEN_VEC_FCN_NAME_BY_OPER(VS, Add)
    GEN_VEC_FCN_NAME_BY_OPER(VS, Sub)
    GEN_VEC_FCN_NAME_BY_OPER(VS, Mul)
    GEN_VEC_FCN_NAME_BY_OPER(VS, Div)
    GEN_VEC_FCN_NAME_BY_OPER_INT(VS, Mod)
    GEN_VEC_FCN_NAME_BY_OPER_INT(VS, And)
    GEN_VEC_FCN_NAME_BY_OPER_INT(VS, Or)
    GEN_VEC_FCN_NAME_BY_OPER_INT(VS, Xor)
    GEN_VEC_FCN_NAME_BY_OPER(VS, BoolAnd)
    GEN_VEC_FCN_NAME_BY_OPER(VS, BoolOr)
    GEN_VEC_FCN_NAME_BY_OPER_INT(VS, Shr)
    GEN_VEC_FCN_NAME_BY_OPER_INT(VS, Shl)
    GEN_VEC_FCN_NAME_BY_OPER(VS, Eq)
    GEN_VEC_FCN_NAME_BY_OPER(VS, Neq)
    GEN_VEC_FCN_NAME_BY_OPER(VS, Lte)
    GEN_VEC_FCN_NAME_BY_OPER(VS, Gte)
    GEN_VEC_FCN_NAME_BY_OPER(VS, Lt)
    GEN_VEC_FCN_NAME_BY_OPER(VS, Gt)
};

VecFunctionScalar vecDispatchScalarL[] = {
    GEN_VEC_FCN_NAME_BY_OPER(SV, Add)
    GEN_VEC_FCN_NAME_BY_OPER(SV, Sub)
    GEN_VEC_FCN_NAME_BY_OPER(SV, Mul)
    GEN_VEC_FCN_NAME_BY_OPER(SV, Div)
    GEN_VEC_FCN_NAME_BY_OPER_INT(SV, Mod)
    GEN_VEC_FCN_NAME_BY_OPER_INT(SV, And)
    GEN_VEC_FCN_NAME_BY_OPER_INT(SV, Or)
    GEN_VEC_FCN_NAME_BY_OPER_INT(SV, Xor)
    GEN_VEC_FCN_NAME_BY_OPER(SV, BoolAnd)
    GEN_VEC_FCN_NAME_BY_OPER(SV, BoolOr)
    GEN_VEC_FCN_NAME_BY_OPER_INT(SV, Shr)
    GEN_VEC_FCN_NAME_BY_OPER_INT(SV, Shl)
    GEN_VEC_FCN_NAME_BY_OPER(SV, Eq)
    GEN_VEC_FCN_NAME_BY_OPER(SV, Neq)
    GEN_VEC_FCN_NAME_BY_OPER(SV, Lte)
    GEN_VEC_FCN_NAME_BY_OPER(SV, Gte)
    GEN_VEC_FCN_NAME_BY_OPER(SV, Lt)
    GEN_VEC_FCN_NAME_BY_OPER(SV, Gt)
};

VecFunctionUnary vecDispatchUnary[] = {
    GEN_VEC_FCN_NAME_BY_OPER(U, Add)
    GEN_VEC_FCN_NAME_BY_OPER(U, Sub)
    //GEN_VEC_FCN_NAME_BY_OPER(U, Addr)
    //GEN_VEC_FCN_NAME_BY_OPER(U, Value)
    GEN_VEC_FCN_NAME_BY_OPER_INT(U, Neg)
    GEN_VEC_FCN_NAME_BY_OPER(U, Inc)
    GEN_VEC_FCN_NAME_BY_OPER(U, Dec)
    GEN_VEC_FCN_NAME_BY_OPER(U, BoolNeg)
};

VecFunctionCast vecDispatchCast[] = {
    GEN_VEC_FCN_NAME_BY_DEST(I8)
    GEN_VEC_FCN_NAME_BY_DEST(I16)
    GEN_VEC_FCN_NAME_BY_DEST(I32)
    GEN_VEC_FCN_NAME_BY_DEST(I64)
    GEN_VEC_FCN_NAME_BY_DEST(U8)
    GEN_VEC_FCN_NAME_BY_DEST(U16)
    GEN_VEC_FCN_NAME_BY_DEST(U32)
    GEN_VEC_FCN_NAME_BY_DEST(U64)
    GEN_VEC_FCN_NAME_BY_DEST(F32)
    GEN_VEC_FCN_NAME_BY_DEST(F64)
};

GEN_VEC_FCN_F(vecFillI32, int32_t)
GEN_VEC_FCN_F(vecFillI64, int64_t)
GEN_VEC_FCN_F(vecFillU32, uint32_t)
GEN_VEC_FCN_F(vecFillU64, uint64_t)
GEN_VEC_FCN_F(vecFillF32, float)
GEN_VEC_FCN_F(vecFillF64, double)
VecFunctionFill vecDispatchFill[] = {
    vecFillI32,
    vecFillI64,
    vecFillU32,
    vecFillU64,
    vecFillF32,
    vecFillF64
};
