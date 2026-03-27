#include "runtime.h"
#include "stdio.h"
#include <cstdint>
#include <cstdio>
#include <cstring>



inline void printString(Runtime::_String str) {
    fwrite(str.buff, 1, str.len, stdout);
}

void printAsHex(uint64_t val) {
    #define HEX_DIGIT(v) ((v) < 10 ? '0' + (v) : 'A' + (v) - 10)

    fputs("0x", stdout);

    fputc(HEX_DIGIT((val >> 60) & 0xF), stdout);
    fputc(HEX_DIGIT((val >> 56) & 0xF), stdout);

    fputc(HEX_DIGIT((val >> 52) & 0xF), stdout);
    fputc(HEX_DIGIT((val >> 48) & 0xF), stdout);

    fputc(HEX_DIGIT((val >> 44) & 0xF), stdout);
    fputc(HEX_DIGIT((val >> 40) & 0xF), stdout);

    fputc(HEX_DIGIT((val >> 36) & 0xF), stdout);
    fputc(HEX_DIGIT((val >> 32) & 0xF), stdout);

    fputc(HEX_DIGIT((val >> 28) & 0xF), stdout);
    fputc(HEX_DIGIT((val >> 24) & 0xF), stdout);

    fputc(HEX_DIGIT((val >> 20) & 0xF), stdout);
    fputc(HEX_DIGIT((val >> 16) & 0xF), stdout);

    fputc(HEX_DIGIT((val >> 12) & 0xF), stdout);
    fputc(HEX_DIGIT((val >> 8)  & 0xF), stdout);

    fputc(HEX_DIGIT((val >> 4)  & 0xF), stdout);
    fputc(HEX_DIGIT((val >> 0)  & 0xF), stdout);

}

void printI64(uint64_t val, int sign) {

    uint8_t buff[20];
    uint8_t idx = 19;

    do {
        int64_t tmp = val;
        val /= 10;
        buff[idx] = (tmp - val * 10) + '0';
        idx--;
    } while (val);

    if (sign) {
        buff[idx] = '-';
        idx--;
    }

    fwrite(buff + idx + 1, 1, sizeof(buff) - idx - 1, stdout);

}

void printI64(int64_t val) {
    printI64(val, val < 0);
}

void printU64(uint64_t val) {
    printI64(val, 0);
}

void printString(Runtime::_ArrayInfo* str, Runtime::_Slice* slice) {

    if (str->element->kind == Type::DT_U8) {
        fwrite(slice->ptr, 1, slice->len, stdout);
    } else {
        fwrite("TODO", 1, 4, stdout);
    }

}

void printArray(Runtime::_ArrayInfo* arr, Runtime::_Slice* slice) {

    const uint64_t count = slice->len;
    const uint64_t stride = arr->element->size;

    printf("[");

    for (uint64_t i = 0; i < count; i++) {

        Runtime::_Any element;
        element.info = arr->element;

        uint8_t* elementAddr = (uint8_t*) slice->ptr + (i * stride);

        if (isPrimitive(element.info->kind)) {
            element.u = 0;
            memcpy(&element.u, elementAddr, stride);
        } else {
            element.p = elementAddr;
        }

        Runtime::printValue(element);

        if (i < count - 1) printf(", ");

    }

    printf("]");

}

void printStruct(Runtime::_StructInfo* info, uint8_t* data) {

    const uint64_t count = info->memberCount;

    printString(info->name);
    printf(" {");

    uint8_t* basePtr = data;

    for (int i=0; i < count; i++) {

        Runtime::_StructMemberInfo* memberInfo = info->members + i;

        Runtime::_Any member;
        member.info = memberInfo->type;

        if (isPrimitive(member.info->kind)) {
            member.u = *(uint64_t*) (basePtr + memberInfo->offset);
        } else {
            member.p = (basePtr + memberInfo->offset);
        }

        Runtime::printValue(member);

        if (i < count - 1) printf(";\n");

    }

    printf(" }");

}

void Runtime::printValue(_Any val) {

    switch (val.info->kind) {

        case Type::DT_I8:
        case Type::DT_I16:
        case Type::DT_I32:
        case Type::DT_I64: {
            printI64(val.i);
            break;
        }

        case Type::DT_U8:
        case Type::DT_U16:
        case Type::DT_U32:
        case Type::DT_U64: {
            printI64(val.u);
            break;
        }

        case Type::DT_CUSTOM: {
            printStruct((_StructInfo*) val.info, val.bp);
            break;
        }

        case Type::DT_POINTER: {
            printAsHex(val.u);
            break;
        }

        case Type::DT_STRING: {
            printString((_ArrayInfo*) val.info, val.s);
            break;
        }

        case Type::DT_ARRAY: {
            printArray((_ArrayInfo*) val.info, val.s);
            break;
        }

        default: {
            printf("TODO");
            break;
        }

    }

}

void Runtime::print(char* fmt, int fmtLen, int argsCnt, _Any* args) {

    int idx = 0;
    int argIdx = 0;
    int beginIdx = 0;
    for (; idx < fmtLen; idx++) {

        const char ch = fmt[idx];
        if (ch == '%') {
            fwrite(fmt + beginIdx, 1, idx - beginIdx, stdout);
            printValue(args[argIdx]);

            argIdx++;
            beginIdx = idx + 1;
        }

    }

    fwrite(fmt + beginIdx, 1, idx - beginIdx, stdout);

}
