#pragma once

#include <cstdint>
namespace Set {

    enum HashMethod {
        HM_IDENTITY,  // already hash/random id
        HM_FIBONACCI, // good for pointers/aligned memory
        HM_MURMUR3,   // for general uint64_t data
        HM_FNV1A      // for small sets
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
        Slot* table;
        uint64_t tableSize;

        uint64_t usedSize;

        HashMethod hashMethod;
    };

    void init(Container* set, uint64_t tableSize);

    // true: item was inserted; false: item already exists
    bool insert(Container* set, uint64_t data);
    // true: item was removed; false: item does not exists
    bool remove(Container* set, uint64_t data);

    void clear(Container* set);

}
