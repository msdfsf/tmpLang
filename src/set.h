#pragma once

#include <cstdint>
namespace Set {

    enum HashMethod {
        HM_IDENTITY,    // already hash/random id
        HM_FIBONACCI,   // good for pointers/aligned memory
        HM_MURMUR3,     // for general uint64_t data
        HM_FNV1A,       // for small sets

        // Start of the section where data will be treated as
        // null-terminated strings
        HM_STRING_START,
        HM_STRING_FNV1A = HM_STRING_START,
    
        // 'key' is treated as a pointer to 12 byte int
        HM_12_FNV1A,
    };

    enum SlotType {
        ST_EMPTY,
        ST_OCCUPIED,
        ST_DELETED,
    };

    struct Slot {
        uint64_t data;
        SlotType type;
    };

    struct Container {
        Slot*    table;
        uint64_t tableSize;

        uint64_t usedSize;
        uint64_t keyOffset;

        HashMethod hashMethod;
    };

    void init(Container* set, uint64_t tableSize);
    void release(Container* set);

    // true: item was inserted; false: item already exists
    bool insert(Container* set, uint8_t* data);
    // true: item was removed; false: item does not exists
    bool remove(Container* set, uint64_t key);

    uint8_t* find(Container* set, uint64_t key);

    void clear(Container* set);

}
