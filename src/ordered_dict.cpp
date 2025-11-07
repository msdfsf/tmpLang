#include "ordered_dict.h"
#include "array_list.h"
#include "allocator.h"
#include "string.h"

namespace OrderedDict {

    void init(Container* dict, size_t initialSize) {

        dict->flags = 0;
        DArray::init(&dict->pairs, initialSize, sizeof(Pair));

    }
    
    struct Slot {
        bool match;
        int idx;
    };

    Slot getSlot(Container* dict, String key) {

        int size = dict->pairs.size;
        Pair* pairs = (Pair*) dict->pairs.buffer;

        // binary search
        int left = 0;
        int right = (int) size - 1;
        int insertIdx = 0;

        while (left <= right) {

            int idx = (left + right) / 2;
            int cmp = cstracmp((pairs + idx)->str, key);

            if (cmp == 0) {
                return { .match = 1, .idx = idx };
            } else if (cmp < 0) {
                // (pairs + mid)->str < key
                left = idx + 1;
                insertIdx = left;
            } else {
                // (pairs + idx).str > key
                right = idx - 1;
                insertIdx = idx;
            }

        }

        return { .match = 0, .idx = insertIdx };

    }

    void* get(Container* dict, String key) {

        const Slot slot = getSlot(dict, key);
        return slot.match ? ((Pair*) dict->pairs.buffer) + slot.idx : NULL;

    }

    int set(Container* dict, String key, void* dataPtr) {

        const Slot slot = getSlot(dict, key);
        if (slot.match) return 0;

        DArray::shiftRight(&dict->pairs, slot.idx);

        Pair pair;
        pair.data = dataPtr;
        pair.str.len = key.len;
        if (dict->flags & COPY_STRINGS) {
            pair.str.buff = (char*) alloc(alc, key.len);
            memcpy(pair.str.buff, key.buff, key.len);
        } else {
            pair.str.buff = key.buff;
        }

        DArray::set(&dict->pairs, slot.idx, &pair);

        return 1;

    }

}
