#pragma once

#include <stdint.h>



namespace Arena {

    struct Container {

        uint64_t stackPos;
        uint64_t stackSize;
        uint64_t stackCount;

        uint8_t* headStack;
        uint8_t* tailStack;

        uint64_t lastPushSize;

    };

    void init(Container* arena, uint64_t initialSize);
    void release(Container* arena);

    void* push(Container* arena, size_t size);
    void pop(Container* arena);

    void rollback(Container* arena, void* ptr);

    void clear(Container* arena);

}
