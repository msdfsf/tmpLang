#include "lsp.h"

#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>

FileSystem::Handle hnd;

struct LineOffsets {
    uint32_t* data;
    uint32_t  size;
    uint32_t  capacity;
};

struct FileData {
    String      data;
    uint64_t    version;
    LineOffsets lineOffsets;
};

static int hexVal(char c) {

    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
    if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
    return -1;

}

// Convert file:// URI -> filesystem path reusing the input buffer
// returns the new length of the path
int uriToPathInplace(String* uri) {

    const char prefix[] = "file://";
    const int prefixLen = sizeof(prefix) - 1;
    int idx = 0;

    if (uri->len >= prefixLen &&
        strncmp(uri->buff, prefix, prefixLen) == 0
    ) {
        idx = prefixLen;
    }

    int outIdx = 0;
    while (idx < uri->len) {

        if (uri->buff[idx] == '%' &&
            idx + 2 < uri->len &&
            std::isxdigit((unsigned char) uri->buff[idx + 1]) &&
            std::isxdigit((unsigned char) uri->buff[idx + 2])
        ) {

            int high = hexVal(uri->buff[idx + 1]);
            int low = hexVal(uri->buff[idx + 2]);
            if (high >= 0 && low >= 0) {
                uri->buff[outIdx] = (char)((high << 4) | low);
                outIdx++;
                idx += 3;
                continue;
            }
        }

        char ch = uri->buff[idx++];
        #ifdef _WIN32
            if (ch == '/') ch = '\\';
        #endif
        uri->buff[outIdx] = ch;
        outIdx++;

    }

    uri->buff[outIdx] = '\0';
    uri->len = outIdx;

    return outIdx;

}

constexpr double GROW_COEF = 1.5;

// allocates sufficient initial buffer for given string
// and copies data
String makeFileString(String str) {
    String out;
    out.len = str.len * GROW_COEF;
    out.buff = (char*) malloc(out.len);
    return out;
}

void deleteFileString(String str) {
    free(str.buff);
    str.len = 0;
}

// TODO
void panic() {
    fprintf(stderr, "Malloc! Malloc! Maaaaalloc!\n");
    exit(1);
}

void pushLineOffset(LineOffsets* lines, uint32_t offset) {
    if (lines->size >= lines->capacity) {
        lines->capacity *= 1.5;
        lines->data = (uint32_t*) realloc(lines->data, lines->capacity * sizeof(uint32_t));
        if (!lines->data) panic();
    }
    lines->data[lines->size] = offset;
    lines->size++;
}

// Creates and computes line offsets
void createLineOffsets(FileData* data) {
    LineOffsets* lines = &data->lineOffsets;

    lines->capacity = 1024;
    lines->data = (uint32_t*) malloc(lines->capacity * sizeof(uint32_t));
    lines->size = 0;

    for (int i = 0; i < data->data.len; i++) {
        const char ch = data->data[i];
        if (ch == '\n') pushLineOffset(lines, i);
    }
}

// As we are receiving file 'id' in form of
// uri, we will just treat its null terminated
// string directly as handler
FileSystem::Handle toFileHandler(String rawUri) {
    uriToPathInplace(&rawUri);
    return rawUri.buff;
}

Lsp::Err::Kind
Lsp::handle(Lsp::TextDocument::DidOpen* method) {

    if (!method || !method->textDocument) {
        return Err::report({
            Err::INVALID_PARAMS, { nullptr, 0 }
        }, nullptr);
    }

    Lsp::T::TextDocumentItem* item = method->textDocument;

    uriToPathInplace(&item->uri);
    hnd = FileSystem::load(item->uri);

    FileSystem::FileInfo* info = FileSystem::getFileInfo(hnd);
    FileData* data = (FileData*) info->userData;

    if (!data) {
        data = (FileData*) malloc(sizeof(FileData));
    }

    deleteFileString(data->data);
    data->data = makeFileString(item->uri);

    createLineOffsets(data);

    return Err::OK;

}

Lsp::Err::Kind
Lsp::handle(Lsp::TextDocument::DidClose *method) {

    if (!method || !method->textDocument) {
        return Err::report({
            Err::INVALID_PARAMS, { nullptr, 0 }
        }, nullptr);
    }

    FileSystem::unload(method->textDocument->uri.buff);
    return Err::OK;

}

Lsp::Err::Kind
Lsp::handle(TextDocument::DidChange* method) {

    if (!method || !method->textDocument) {
        return Err::report({
            Err::INVALID_PARAMS, { nullptr, 0 }
        }, nullptr);
    }

    Lsp::T::Int version = method->textDocument->version;
    String uri = method->textDocument->uri;
    // TODO : Here we went to sleep...

    return Err::OK;

}
