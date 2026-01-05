#pragma once

// By default, I don’t need to manage memory in any clever way. The code
// is not very suitable for reuse as, let’s say, a library for building tools
// like an LSP. To address this, I added a simple compile-time allocation interface.
//
// I wanted to keep the ability to inline functions and avoid using macros,
// since they are not very debugger or LSP friendly. Templates were also avoided,
// because they are verbose and too C++.
//
// I considered using general handles instead of pointers, but that
// would only complicate how objects are handled in the code. So I ended up
// using the default approach with pointers. If handles are needed, they must be
// part of an outer system capable of mapping pointers.
//
// For now, this is the provided solution. I am not quite happy or satisfied with it,
// but at least I can be sure that it works.
//
// If a custom allocator is needed, define _CUSTOM_ALLOCATOR_
// and provide implementations for the alloc/dealloc functions.
//
// Note: This is the default allocator used for general cases.
// It exists to be included where importing the 'AST' symbols
// and their allocator would be wasteful.
//

#include <ranges>
#if !defined(_CUSTOM_ALLOCATOR_)

    #include "dynamic_arena.h"

    constexpr size_t ALLOC_INIT_BUFFER_SIZE = 1024 * 1024 * 32; // 32MB

    inline Arena::Container _allocatorMem;
    inline Arena::Container* alc; // TODO: not sure about name, but as in Cpp
                                            // there has to be a cast, using something
                                            // long makes call too messy
                                            // maybe just use thread local var and call
                                            // it a day...

    inline void initAlloc(Arena::Container* allocator) {
        Arena::init(allocator, ALLOC_INIT_BUFFER_SIZE);
    }

    inline void releaseAlloc(Arena::Container* allocator) {
        Arena::release(allocator);
    }

    inline void* alloc(Arena::Container* allocator, size_t size) {
        return Arena::push(allocator, size);
    }

    inline void* alloc(Arena::Container* allocator, size_t size, size_t align) {
        return Arena::push(allocator, size, align);
    }

    inline void dealloc(Arena::Container* allocator, void* ptr) {
        // ))
        return Arena::rollback(allocator, ptr);
    }

    // start a transaction
    inline uint64_t beginScope (Arena::Container* allocator) {
        return 0; //TODO
    }

    // free everything allocated since 'begin'
    inline void rewindScope (Arena::Container* allocator, uint64_t handle) {
        // TODO
    }

    // keep the data, drop the 'begin' and all asociated data
    inline void commitScope (Arena::Container* allocator, uint64_t handle) {
        // TODO
    }

#endif
