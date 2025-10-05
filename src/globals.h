#pragma once

#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <filesystem> // TODO


// TODO
typedef float float_t;
typedef double double_t;



constexpr uint16_t toDoubleChar(char first, char second) {
    return (static_cast<uint16_t>(first) << 8) | static_cast<uint16_t>(second);
}

constexpr std::size_t cstrlen(const char* str) {
    std::size_t len = 0;
    while (str[len] != '\0') {
        len++;
    }
    return len;
}


struct Pos;
struct Span;
struct LogInfo;
struct File;
struct INamed;



static const char IFS_PRINTF[] = "printf";
static const char IFS_ALLOC[]  = "malloc";
static const char IFS_FREE[]   = "free";

static const char IVS_NULL[] = "null";
static const char IVS_TRUE[] = "true";
static const char IVS_FALSE[] = "false";

static const char IS_FILL = '@';



enum DataTypeEnum : int {
    DT_VOID = 0,
    DT_INT,
    DT_I8,
    DT_I16,
    DT_I32,
    DT_I64,
    DT_U8,
    DT_U16,
    DT_U32,
    DT_U64,
    DT_F32,
    DT_F64,
    DT_STRING,
    DT_POINTER,
    DT_ARRAY,
    DT_SLICE,
    DT_MULTIPLE_TYPES,
    DT_CUSTOM,
    DT_UNION,
    DT_ERROR,
    DT_MEMBER, // dont know about this one, represents the right side of member reference operator 'point . x'
    DT_ENUM,
    DT_FUNCTION, // FunctionPrototype
    DT_UNDEFINED,
    
    DT_BOOL = DT_U64,
};

static const int DATA_TYPES_COUNT = DT_UNDEFINED + 1;



// also indexes operators array
enum OperatorEnum {
    OP_NONE = -1,
    OP_UNARY_PLUS = 0,
    OP_UNARY_MINUS,
    OP_ADDITION,
    OP_SUBTRACTION,
    OP_MULTIPLICATION,
    OP_DIVISION,
    OP_MODULO,
    OP_GET_ADDRESS,
    OP_GET_VALUE,
    OP_BITWISE_AND,
    OP_BITWISE_OR,
    OP_BITWISE_XOR,
    OP_BITWISE_NEGATION,
    OP_SHIFT_RIGHT,
    OP_SHIFT_LEFT,
    OP_EQUAL,
    OP_NOT_EQUAL,
    OP_LESS_THAN,
    OP_GREATER_THAN,
    OP_LESS_THAN_OR_EQUAL,
    OP_GREATER_THAN_OR_EQUAL,
    OP_BOOL_AND,
    OP_BOOL_OR,
    OP_INCREMENT,
    OP_DECREMENT,
    OP_SUBSCRIPT,
    OP_MEMBER_SELECTION,
    OP_DEREFERENCE_MEMBER_SELECTION,
    OP_NEGATION,
    OP_CONCATENATION,
    // OP_CAST // to tie cast to dtype, not sure about it as operator, but lets see
    OP_COUNT,
    OP_INVALID
};



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
    /*
    IS_RESERVED_16  = 1 << 18,
    IS_RESERVED_17  = 1 << 19,
    IS_RESERVED_18  = 1 << 20,
    IS_RESERVED_19  = 1 << 21,
    IS_RESERVED_20  = 1 << 22,
    IS_RESERVED_21  = 1 << 23,
    IS_RESERVED_23  = 1 << 25,
    IS_RESERVED_24  = 1 << 26,
    IS_RESERVED_25  = 1 << 27,
    IS_RESERVED_26  = 1 << 28,
    IS_RESERVED_27  = 1 << 29,

    */

    IS_UNIQUE  = 1 << 30, // used in parser while checking for unique names in scope

};

struct String {
    
    char* buff;
    uint64_t len;

    inline char& operator [] (const int idx) {
        return buff[idx];
    }

    inline operator char* () {
        return buff;
    }

    inline void operator = (char* str) {
        buff = str;
    }

    inline void operator += (int offset) {
        buff += offset;
        len -= offset;
    }

    inline bool operator==(const String& str) noexcept {
        if (str.len != this->len) return false;
        return std::memcmp(this->buff, str.buff, str.len) == 0;
    }

    inline bool operator<(const String& str) noexcept {
        const int na = this->len;
        const int nb = str.len;
        const int n = std::min(na, nb);
        int cmp = 0;
        if (n > 0) cmp = std::memcmp(this->buff, str.buff, static_cast<size_t>(n));
        if (cmp != 0) return cmp < 0;
        return na < nb;
    }

    constexpr String() : buff(NULL), len(0) {}
    
    constexpr String(char* buff, uint64_t len) : buff(buff), len(len) {}

    constexpr String(const char* cstr) : buff((char*) cstr), len(cstrlen(cstr)) {}

};

inline bool operator==(const String& a, const String& b) noexcept {
    if (a.len != b.len) return false;
    return std::memcmp(a.buff, b.buff, a.len) == 0;
}

inline bool operator<(const String& a, const String& b) noexcept {
    const int na = a.len;
    const int nb = b.len;
    const int n = std::min(na, nb);
    int cmp = 0;
    if (n > 0) cmp = std::memcmp(a.buff, b.buff, static_cast<size_t>(n));
    if (cmp != 0) return cmp < 0;
    return na < nb;
}

constexpr int cstrcmp(const String strA, const String strB) {
    
    if (strA.len != strB.len) return 0;

    for (int i = 0; i < strA.len; i++) {
        if (strA.buff[i] != strB.buff[i]) {
            return 0;
        }
    }

    return 1;

}

struct File {
    std::filesystem::path absPath;
    char* absPathRaw;
    char* name;
    char* buff;
    uint64_t buffLen;
    uint64_t fpId; // file provider internal id if needed
};

struct FileId {
    uint64_t size;
    std::filesystem::file_time_type time;

    //FileId() = default;
    //FileId(const FileId&) = default;
    //FileId& operator=(const FileId&) = default;
    //FileId(FileId&&) noexcept = default;
    //FileId& operator=(FileId&&) noexcept = default;

    bool operator<(const FileId& other) const {
        return std::tie(size, time) < std::tie(other.size, other.time);
    }
};

struct Pos {
    int idx;
    int ln;
};

static const Pos INVALID_POS = { -1, -1 };
static const uint64_t NULL_FLAG = (uint64_t) (0);

struct Span {
    File* file;
    char* str;    // source buffer also here for better caching
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

struct INamed {
	    
    char* name;
    int nameLen;

    INamed() {};

    constexpr INamed(char* const nm, const int nmLn) : 
        name(nm), 
        nameLen(nmLn)
    {

    };

};

// sometimes we need to store locations of INamed stuff that 
// is not describing advanced stuff
// stuff... stuff...
struct INamedLoc : INamed {

    Span* span;

    INamedLoc() {};

    constexpr INamedLoc(char* const nm, const int nmLn, Span* span) : 
        INamed(nm, nmLn),
        span(span)
    {};

};

// LOOK AT : maybe better name
struct INamedEx : INamed {
    
    uint32_t id; // LOOK AT : change to macro for 32bit/64bit

    INamedEx() {};
    
    constexpr INamedEx(char* const nm, const int nmLn, const uint32_t id) :
        INamed(nm, nmLn),
        id(id)
    {};

};



static inline int isValidPos(Pos pos) {
    return pos.ln != INVALID_POS.ln;
}

static inline Span* getSpanStamp(Span* span) {

    Span* stamp = (Span*)malloc(sizeof(Span));
    if (!stamp) return NULL;

    stamp->str = span->str;
    stamp->file = span->file;
    stamp->start = span->start;
    stamp->end = span->end;

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
    free(loc);
}
