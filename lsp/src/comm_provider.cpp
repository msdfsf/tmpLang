#include "comm_provider.h"
#include "../../src/error.h"

int allocString(String* str) {

    str->buff = (char*) malloc(str->len);
    return str->buff ? Err::OK : Err::MALLOC;

}

void freeString(String* str) {
    free(str->buff);
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

int CommProvider::read(Info* info, Message* msg) {

    // Content-Length: <length>\r\n
    // ... other headers ...
    // \r\n
    // <content>

    if (!info || !msg) return Err::INVALID_ARGUMENTS;
    
    String body;
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
                    return Err::FILE_DOES_NOT_EXISTS; // TODO : add better error
                }
            }
            
            stream = info->file.handle;
            
            break;
        
        }

        case CT_TCP: {
            fprintf(stderr, "TCP read not yet implemented\n");
            return Err::NOT_YET_IMPLEMENTED;
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

    if (body.len == 0) return Err::UNEXPECTED_END_OF_FILE;
    if (allocString(&body) < 0) return Err::MALLOC;

    fread(body.buff, 1, body.len, stream);
    fprintf(stderr, "%.*s\n", body.len, body.buff);

    Json::Value js;
    const int err = Json::parse(body, &js);
    freeString(&body);
    if (err < 0) return err;

    msg->body = js;
    return Err::OK;

}


int CommProvider::write(CommProvider::Info* info, Json::Value body) {
    return CommProvider::write(info, Json::dump(&body));
}

int CommProvider::write(CommProvider::Info* info, String body) {

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
                    return Err::FILE_DOES_NOT_EXISTS; // TODO : add better error
                }
            }
            
            stream = info->file.handle;
            
            break;
        
        }

        case CT_TCP: {
            fprintf(stderr, "TCP read not yet implemented\n");
            return Err::NOT_YET_IMPLEMENTED;
        }

    }

    if (fprintf(stream, "Content-Length: %zu\r\n\r\n", (size_t) body.len) < 0) {
        fprintf(stderr, "fprintf failed to write header\n");
        return Err::IO_ERROR;
    }

    size_t wrote = fwrite(body.buff, 1, (size_t) body.len, stream);
    if (wrote != (size_t)body.len) {
        fprintf(stderr, "fwrite failed to deliver %zu bytes, only %zu delivered\n", (size_t) body.len, wrote);
        return Err::IO_ERROR;
    }

    if (fflush(stream) != 0) {
        perror("fflush");
        return Err::IO_ERROR;
    }

    fprintf(stderr, "Content-Length: %ld\r\n\r\n%.*s", body.len, body.len, body.buff);
    
    return Err::OK;

}