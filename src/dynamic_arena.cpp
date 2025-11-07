#include "dynamic_arena.h"

#include <cstdint>
#include <stdlib.h>
#include <cstring>



namespace Arena {

    inline void setNextStack(uint8_t* stack, size_t size, void* nextStack) {

        uint8_t** linkPos = (uint8_t**) (stack + size);
        *linkPos = (uint8_t*) nextStack;

    }

    inline uint8_t* getNextStack(uint8_t* stack, size_t size) {

        uint8_t** linkPos = (uint8_t**) (stack + size);
        return *linkPos;

    }

    inline void setPrevStack(uint8_t* stack, size_t size, void* prevStack) {

        uint8_t** linkPos = (uint8_t**) (stack + size + sizeof(uint8_t*));
        *linkPos = (uint8_t*) prevStack;

    }

    inline uint8_t* getPrevStack(uint8_t* stack, size_t size) {

        uint8_t** linkPos = (uint8_t**) (stack + size + sizeof(uint8_t*));
        return *linkPos;

    }

    void init(Container* arena, uint64_t initialSize) {

        arena->headStack = (uint8_t*) malloc(initialSize + 2 * sizeof(void*));
        arena->tailStack = arena->headStack;
        arena->stackSize = initialSize;
        arena->stackPos = 0;
        arena->stackCount = 1;

        setNextStack(arena->headStack, arena->stackSize, NULL);
        setPrevStack(arena->headStack, arena->stackSize, NULL);

    }

    void release(Container* arena) {

        uint8_t* stack = arena->headStack;
        while (stack) {
            uint8_t* tmp = getNextStack(stack, arena->stackSize);
            free(stack);
            stack = tmp;
        }

    }

    void* push(Container* arena, size_t size) {

        if (arena->stackPos + size > arena->stackSize) {

            uint8_t* nextStack = getNextStack(arena->tailStack, arena->stackSize);
            if (!nextStack) {
                nextStack = (uint8_t*) malloc(arena->stackSize + 2 * sizeof(void*));
                setNextStack(arena->tailStack, arena->stackSize, nextStack);
                setNextStack(nextStack, arena->stackSize, NULL);
                setPrevStack(nextStack, arena->stackSize, arena->tailStack);
                arena->stackCount++;
            }

            arena->tailStack = nextStack;
            arena->stackPos = 0;

        }

        void* ptr = arena->tailStack + arena->stackPos;
        arena->stackPos += size;
        arena->lastPushSize = size;

        return ptr;

    }

    void pop(Container* arena) {

        if (arena->stackPos < arena->lastPushSize) {
            arena->stackPos = 0;
        } else {
            arena->stackPos -= arena->lastPushSize;
        }

    }

    void rollback(Container* arena, void* ptr) {

        uint8_t* stack = arena->tailStack;
        while (stack) {

            if (ptr < stack || ptr >= stack + arena->stackSize) {
                stack = getPrevStack(stack, arena->stackSize);
                continue;
            }

            int64_t diff = (uint64_t) ptr - (uint64_t) stack;
            arena->stackPos = diff;
            arena->tailStack = stack;

            return;

        }

    }

    void clear(Container* arena) {

        arena->stackPos = 0;
        arena->stackCount = 1;
        arena->lastPushSize = 0;
        arena->tailStack = arena->headStack;

    }

}
