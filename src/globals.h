#pragma once

#include "allocator.h"
#include "string.h"
#include "file_system.h"

#include <cstdint>



// TODO
typedef float float_t;
typedef double double_t;

typedef uint64_t Flags;

typedef size_t MemberOffset;

struct Pos;
struct Span;
struct LogInfo;



char* const stageTag = NULL;

static uint8_t stageId;
struct UserFileData {
    uint8_t id;
    void* data;
};



#define getMemberOffset(type, member) ((MemberOffset) &(((type*) NULL)->member))
#define getMember(ptr, member) (((char*) (ptr)) + (member))



constexpr uint16_t toDoubleChar(char first, char second) {
    return (static_cast<uint16_t>(first) << 8) | static_cast<uint16_t>(second);
}



// TODO : better name
enum State : uint64_t {
    IS_CONST = 1 << 0,
    IS_CMP_TIME = 1 << 1,
    IS_DYNAMIC = 1 << 2,
    IS_EMBEDED = 1 << 3, // LOOK AT: wtf is this

    IS_UNARY = 1 << 4,
    IS_BINARY = 1 << 5,
    IS_TERNARY = 1 << 6,

    IS_ONE_CHAR = 1 << 7,
    IS_TWO_CHAR = 1 << 8,
    IS_THREE_CHAR = 1 << 9,
    IS_FOUR_CHAR = 1 << 10,

    IS_ARRAY = 1 << 11,
    IS_ARRAY_LIST = 1 << 12,

    IS_ALLOCATED = 1 << 13,

    IS_STRING  = 1 << 14,

    IS_ALLOCATION = 1 << 15,
    IS_LENGTH = 1 << 16,
    IS_SIZE = 1 << 17,

    IS_RENDERED = 1 << 24,

    IS_UNIQUE  = 1 << 30, // used in parser while checking for unique names in scope
};

struct Pos {
    int idx;
    int ln;
};

static const Pos INVALID_POS = { -1, -1 };
static const uint64_t NULL_FLAG = (uint64_t) (0);

struct Span {
    // store file data directly, as we may need to look up them frequently
    FileSystem::FileInfo* fileInfo;
    const char* str;
    Pos start;
    Pos end;
};

// Keeps track of a location and the original start of a parsed rule
struct SpanEx : Span {
    Pos super;
};

struct LogInfo {
    Span loc;
};


typedef uint64_t Id;
typedef String INamed;

// sometimes we need to store locations of INamed stuff that
// is not describing advanced stuff
// stuff... stuff...
struct INamedLoc {
    char* buff;
    uint64_t len;
    Span* span;
};

// LOOK AT : maybe better name
struct INamedEx {
    char* buff;
    uint64_t len;
    Span* span;
    Id id;
};

inline void init(INamedEx* name) {
    name->id = 0;
    name->len = 0;
    name->buff = NULL;
    name->span = NULL;
}


static inline int isValidFunctionIdx(int idx) {
    return idx >= 0;
}

static inline int isValidPos(Pos pos) {
    return pos.ln != INVALID_POS.ln;
}

static inline Span* getSpanStamp(Span* span) {

    Span* stamp = (Span*) alloc(alc, sizeof(Span));
    if (!stamp) return NULL;

    memcpy(stamp, span, sizeof(Span));

    return stamp;

}

static inline SpanEx markSpanStart(Span* span) {
    return SpanEx { *span, span->start };
}

static inline Span* finalizeSpan(SpanEx* lspan, Span* span) {

    lspan->start = lspan->super;
    span->end = lspan->end;

    return getSpanStamp((Span*) lspan);

}

static inline void syncStartToEnd(Span* const span) {
    span->start = { span->end.idx + 1, span->end.ln};
}

static inline void syncEnd(Span* const dest, Span* const src) {
    dest->start = src->end;
}

static inline void freeSpanStamp(Span* loc) {
    dealloc(alc, loc);
}
