#include "comm_provider.h"
#include "json.h"
#include <string>



bool allocString(JsonString* str) {
    str->data = (char*) malloc(str->len);
    return str->data;
}

void freeString(JsonString* str) {
    free(str->data);
}

/*
int32_t multiCharToI32(const char* str) {
    int val = 0;
    for (int i = 0; i < 4; i++) {
        val <<= 8;
        val |= (unsigned char) str[i];
    }
    return val;
}

int cmpStr(const char* strA, String strB) {
    for (int i = 0; i < strB.len; i++) {
        if (strA[i] != strB.buff[i]) return i;

    }
    return strB.len;
}
*/

CommProvider::Err CommProvider::read(Info* info, Message* msg) {

    // Content-Length: <length>\r\n
    // ... other headers ...
    // \r\n
    // <content>

    if (!info || !msg) return Err::ERR_UNKNOWN;

    JsonString body;
    std::string head;

    FILE* stream = NULL;

    switch (info->type) {

        case CT_STD: {
            stream = stdin;
            break;
        }

        case CT_FILE: {

            if (!info->file.handle) {
                info->file.handle = fopen(info->file.path, "rb");
                if (!info->file.handle) {
                    return Err::ERR_FILE_NOT_FOUND; // TODO : add better error
                }
            }

            stream = info->file.handle;

            break;

        }

        case CT_TCP: {
            fprintf(stderr, "TCP read not yet implemented\n");
            return Err::ERR_NOT_YET_IMPLEMENTED;
        }

    }


    char ch;
    int32_t buff = 0;
    while ((ch = fgetc(stream)) != EOF) {
        head += ch;
        buff = (buff << 8) | ch;
        if (buff == 0x0D0A0D0A) break;
    }

    // as there are only two headers, we will do it in place
    // for now, and maybe forever, we only care about Content-Length
    int idx = 0;
    while (idx < head.size()) {

        int end = head.find('\n', idx);

        if (head.compare(idx, 16, "Content-Length: ") == 0) {
            idx += 16;
            body.len = std::stoi(head.substr(idx, end - idx));
        } else if (head.compare(idx, 14, "Content-Type: ") == 0) {
            idx += 14;
        }

        idx = end + 1;

    }

    if (body.len == 0) return Err::ERR_PARSE;
    if (!allocString(&body)) return Err::ERR_MALLOC;

    fread(body.data, 1, body.len, stream);
    // fprintf(stderr, "%.*s\n", body.len, body.data);

    msg->body = body;
    return Err::OK;

}

CommProvider::Err CommProvider::write(CommProvider::Info* info, JsonString body) {

    FILE* stream = NULL;
    switch (info->type) {

        case CT_STD: {
            stream = stdout;
            break;
        }

        case CT_FILE: {

            if (!info->file.handle) {
                info->file.handle = fopen(info->file.path, "rb");
                if (!info->file.handle) {
                    return Err::ERR_FILE_NOT_FOUND; // TODO : add better error
                }
            }

            stream = info->file.handle;

            break;

        }

        case CT_TCP: {
            fprintf(stderr, "TCP read not yet implemented\n");
            return Err::ERR_NOT_YET_IMPLEMENTED;
        }

    }

    if (fprintf(stream, "Content-Length: %zu\r\n\r\n", (size_t) body.len) < 0) {
        fprintf(stderr, "fprintf failed to write header\n");
        return Err::ERR_IO;
    }

    size_t wrote = fwrite(body.data, 1, (size_t) body.len, stream);
    if (wrote != (size_t)body.len) {
        fprintf(stderr, "fwrite failed to deliver %zu bytes, only %zu delivered\n", (size_t) body.len, wrote);
        return Err::ERR_IO;
    }

    if (fflush(stream) != 0) {
        perror("fflush");
        return Err::ERR_IO;
    }

    fprintf(stderr, "Content-Length: %ld\r\n\r\n%.*s", body.len, body.len, body.data);

    return Err::OK;

}

const char* CommProvider::str(CommProvider::Err code) {
    switch (code) {
        case OK:                    return "OK";
        case ERR_CLOSED:            return "ERR_CLOSED";
        case ERR_TIMEOUT:           return "ERR_TIMEOUT";
        case ERR_PARSE:             return "ERR_PARSE";
        case ERR_NOT_READY:         return "ERR_NOT_READY";
        case ERR_FILE_NOT_FOUND:    return "ERR_FILE_NOT_FOUND";
        case ERR_PERMISSION_DENIED: return "ERR_PERMISSION_DENIED";
        case ERR_UNKNOWN:           return "ERR_UNKNOWN";
        default:                    return "INVALID_ERR_CODE";
    }
}
