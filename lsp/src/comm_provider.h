#pragma once
#include "json.h"

namespace CommProvider {

    enum Err {
        OK = 0,
        ERR_CLOSED              = -1,
        ERR_TIMEOUT             = -2,
        ERR_IO                  = -3,
        ERR_PARSE               = -4,
        ERR_MALLOC              = -5,
        ERR_NOT_READY           = -6,
        ERR_FILE_NOT_FOUND      = -7,
        ERR_PERMISSION_DENIED   = -8,
        ERR_NOT_YET_IMPLEMENTED = -9,
        ERR_UNKNOWN             = -10,
    };

    enum CommType {
        CT_STD,
        CT_TCP,
        CT_FILE,
    };

    struct TCPInfo {
        const char* addr;
        int port;
    };

    struct FileInfo {
        FILE* handle;
        const char* path;
    };

    struct StdInfo {};

    struct Info {
        CommType type;
        union {
            TCPInfo tcp;
            FileInfo file;
            StdInfo std;
        };
    };

    struct Message {
        JsonString body;
    };

    Err read(Info* info, Message* msg);
    Err write(Info* info, JsonString js);

    const char* str(Err code);

};
