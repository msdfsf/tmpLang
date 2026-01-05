#pragma once

#include "array_list.h"
#include "string.h"
#include <cstdint>

namespace OrderedDict {

    enum Flags {
        COPY_STRINGS = 1,
    };

    struct Pair {
        String str;
        void* data;
    };

    struct Container {
        DArray::Container pairs;
        uint64_t flags;
        uint64_t it;
    };

    void init(Container* dict, size_t initialSize);

    void* get(Container* dict, String key);
    int set(Container* dict, String key, void* dataPtr);

    Pair* getNext(Container* dict);

    void clear(Container* dict);

    Container* tightCopy(Container* src);

}
