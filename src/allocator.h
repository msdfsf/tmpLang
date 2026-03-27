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

#if !defined(_CUSTOM_ALLOCATOR_)

    #include "dynamic_arena.h"

    #ifndef ALLOC_INIT_BUFFER_SIZE
        // 32 MB
        #define ALLOC_INIT_BUFFER_SIZE (1024 * 1024 * 32)
    #endif

    typedef Arena::Container* AllocatorHandle;
    // TODO: not sure about name, but as in Cpp
    // there has to be a cast, using something
    // long makes call too messy
    inline thread_local AllocatorHandle alc = NULL;



    inline void initAlloc(AllocatorHandle allocator) {
        Arena::init(allocator, ALLOC_INIT_BUFFER_SIZE);
    }

    inline void releaseAlloc(AllocatorHandle allocator) {
        Arena::release(allocator);
    }



    inline void* alloc(AllocatorHandle allocator, size_t size) {
        return Arena::push(allocator, size);
    }

    inline void* alloc(AllocatorHandle allocator, size_t size, size_t align) {
        return Arena::push(allocator, size, align);
    }

    inline void dealloc(AllocatorHandle allocator, void* ptr) {
        // ))
        return Arena::rollback(allocator, ptr);
    }

#else

    typedef void* AllocatorHandle;
    extern thread_local AllocatorHandle alc;

    extern void* alloc   (AllocatorHandle allocator, size_t size);
    extern void* alloc   (AllocatorHandle allocator, size_t size, size_t align);
    extern void  dealloc (AllocatorHandle allocator, void* ptr);

#endif
