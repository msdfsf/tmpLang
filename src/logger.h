#pragma once

#include <stdint.h>
#include "globals.h"

namespace Logger {

    enum Level {
        PLAIN   = 0x1,
        HINT    = 0x2,
        INFO    = 0x4,
        WARNING = 0x8,
        ERROR   = 0xF
    };

    struct Type {
        Level level = PLAIN;
        const char* const tag = NULL;
    };

    // use Level enum to define bits
    extern uint32_t verbosity;

    // each thread has its own bit which corresponds with its  id
    // if set, log function will not do anything
    // TODO : idea is that Location will also carry info about thread
    //        with such info as id and log destination
    //        for now we dont care
    extern uint64_t mute;

    void log(const Type type, const char* const message, Span* loc, ...);
    void log(const Type type, const char* const message);

}
