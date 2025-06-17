#pragma once

#include <stdint.h>
//#include <stdarg.h>
#include "globals.h"

namespace Logger {

    enum Level {
        PLAIN   = 0x1,
        HINT    = 0x2,
        INFO    = 0x4,
        WARNING = 0x8,
        ERROR   = 0xF
    };

    // better name
    // loc->idx -1 has to point to start closure '(', '{', '<', '[', anything else is not supported for now
    enum UnderlineEnd {
        STATEMENT   = -1,
        LINE        = -2,
        CLOSURE     = -3
    };

    // use Level enum to define bits
    extern uint32_t verbosity;

    // each thread has its own bit which corresponds with its  id
    // if set, log function will not do anything
    // TODO : idea is that Location will also carry info about thread
    //        with such info as id and log destination
    //        for now we dont care
    extern uint64_t mute;

    void log(const uint32_t type, const char* const message, Location* loc, const int len = 0, ...);
    void log(const uint32_t type, const char* const message);

}
