#pragma once

#include <cstdint>
#include <filesystem> // TODO


// TODO
typedef float float_t;
typedef double double_t;



constexpr uint16_t toDoubleChar(char first, char second) {
    return (static_cast<uint16_t>(first) << 8) | static_cast<uint16_t>(second);
}



struct Location;
struct LogInfo;
struct File;
struct INamed;



const char IFS_PRINTF[] = "printf";
const char IFS_ALLOC[]  = "malloc";
const char IFS_FREE[]   = "free";

const char IVS_NULL[] = "null";
const char IVS_TRUE[] = "true";
const char IVS_FALSE[] = "false";

const char IS_FILL = '@';



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
    int len;

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
};

struct File {
    std::filesystem::path absPath;
    char* absPathRaw;
    char* name;
    char* buff;
};

struct Location {
    File* file;
    char* str;    // source buffer also here for better caching
    
    int eidx;
    int eln;
    
    int sidx;
    int sln;
};

struct LogInfo {
    Location loc;
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

    Location* loc;

    constexpr INamedLoc(char* const nm, const int nmLn, Location* loc) : 
        INamed(nm, nmLn),
        loc(loc)
    {

    };

};

// LOOK AT : maybe better name
struct INamedEx : INamed {
    
    uint32_t id; // LOOK AT : change to macro for 32bit/64bit

    INamedEx() {};
    
    constexpr INamedEx(char* const nm, const int nmLn, const uint32_t id) :
        INamed(nm, nmLn),
        id(id)
    {

    };

};




inline void syncStartToEnd(Location* const loc) {
    loc->sidx = loc->eidx;
    loc->sln = loc->eln;
}

inline Location* getLocationStamp(Location* loc) {

    Location* stamp = (Location*) malloc(sizeof(Location));
    if (!stamp) return NULL;

    stamp->file = loc->file;
    stamp->idx = loc->idx;
    stamp->line = loc->line;

    return stamp;

}

void freeLocationStamp(Location* loc) {
    free(loc);
}
