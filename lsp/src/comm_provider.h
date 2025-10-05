#pragma once
#include "json.h"

namespace CommProvider {
    
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
        Json::Value body;
    };

    int read(Info* info, Message* msg);

    int write(Info* info, Json::Value js);
    int write(Info* info, String body);

};