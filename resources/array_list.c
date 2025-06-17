#include <stdlib.h>
#include <stdint.h>

#define TypedArrayList(dtype, name) \
typedef struct ArrayList##name { \
    int size; \
    int len; \
    dtype* data; \
} ArrayList##name; \
ArrayList##name* arrayListCreate##name() { \
    const int DEFAULT_INIT_SIZE = 7; \
    ArrayList##name* arr = malloc(sizeof(ArrayList##name)); \
    if (!arr) return NULL; \
    arr->size = DEFAULT_INIT_SIZE; \
    arr->len = 0; \
    arr->data = malloc(sizeof(dtype) * DEFAULT_INIT_SIZE); \
    if (!arr->data) return NULL; \
    return arr; \
} \
ArrayList##name* arrayListCreateCustom##name(const int size) { \
    ArrayList##name* arr = malloc(sizeof(ArrayList##name)); \
    if (!arr) return NULL; \
    arr->size = size; \
    arr->len = 0; \
    arr->data = malloc(sizeof(dtype) * size); \
    if (!arr->data) return NULL; \
    return arr; \
} \
int arrayListInsert##name(ArrayList##name* arr, const int idx, dtype value) { \
    if (idx >= arr->size) { \
        const int newSize = idx * 2; \
        void* tmp = realloc(arr->data, sizeof(dtype) * newSize); \
        if (!tmp) return -1; \
        if (tmp != arr->data) arr->data = tmp; \
        arr->size = newSize; \
    } \
    arr->data[idx] = value; \
    if (idx >= arr->len) arr->len = idx + 1; \
    return 0; \
} \
int arrayListAppendAlloc##name(ArrayList##name* arr, const int size) { \
    const int newLen = arr->len + size; \
    arr->len = newLen; \
    if (newLen <= arr->size) return 0; \
    const int newSize = newLen + newLen / 2; \
    void* tmp = realloc(arr->data, sizeof(dtype) * newSize); \
    if (!tmp) return -1; \
    if (tmp != arr->data) arr->data = tmp; \
    arr->size = newSize; \
    return 0; \
}

TypedArrayList(uint8_t, U8);
TypedArrayList(uint16_t, U16);
TypedArrayList(uint32_t, U32);
TypedArrayList(uint64_t, U64);


TypedArrayList(int8_t, I8);
TypedArrayList(int16_t, I16);
TypedArrayList(int32_t, I32);
TypedArrayList(int64_t, I64);

TypedArrayList(float, F32);
TypedArrayList(double, F64);

TypedArrayList(char, Char);