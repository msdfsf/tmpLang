#include "lsp.h"

#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>



FileSystem::Handle hnd;

struct LineOffsets {
    uint32_t* data;
    uint32_t  size;
    uint32_t  capacity;
};

struct FileString : String {
    size_t capacity;
};

struct FileData {
    FileString  data;
    uint32_t    version;
    LineOffsets lineOffsets;
};

constexpr double             GROW_COEF      = 1.5;
constexpr FileSystem::Origin ORIGIN         = FileSystem::Origins::USER_START;
constexpr uint32_t           INVALID_OFFSET = UINT32_MAX;



void print(const FileData* data);



// Helpers
// ===

// allocates sufficient initial buffer for given string
// and copies data
FileString makeFileString(String str) {
    FileString out;
    out.capacity = str.len * GROW_COEF;
    out.buff = (char*) malloc(out.capacity);
    memcpy(out.buff, str.buff, str.len);
    out.len = str.len;
    return out;
}

void releaseFileString(FileString str) {
    free(str.buff);
    str.len = 0;
}

void resizeFileString(FileString* str, size_t newSize) {
    if (newSize <= str->capacity) {
        str->len = newSize;
        return;
    }

    str->buff = (char*) realloc(str->buff, newSize);
    if (!str->buff) {
        Lsp::panic({ Lsp::Err::ALLOC, { 0 } });
    }
    str->len = newSize;
}

void pushLineOffset(LineOffsets* lines, uint32_t offset) {
    if (lines->size >= lines->capacity) {
        lines->capacity *= 1.5;
        lines->data = (uint32_t*) realloc(lines->data, lines->capacity * sizeof(uint32_t));
        if (!lines->data) Lsp::panic({ Lsp::Err::ALLOC, {0} });
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

void releaseLineOffset(LineOffsets* offsets) {
    free(offsets->data);
    offsets->capacity = 0;
    offsets->size = 0;
}

void resizeLineOffsets(LineOffsets* offsets, size_t newSize) {
    if (newSize <= offsets->capacity) {
        offsets->size = newSize;
        return;
    }

    offsets->data = (uint32_t*) realloc(offsets->data, newSize);
    if (!offsets->data) {
        Lsp::panic({ Lsp::Err::ALLOC, { 0 } });
    }
    offsets->size = newSize;
}

uint32_t countLines(String str) {
    uint32_t cnt = 0;
    for (int i = 0; i < str.len; i++) {
        if (str[i] == '\n') cnt++;
    }
    return cnt;
}

// While temporary buffer could be used to collect new offsets.
// It seems that predetermine line count in the changed span is
// faster and whats more important simpler.
void updateLineOffsets(
    FileData* data,
    uint32_t  startLine,
    uint32_t  endLine,
    uint32_t  startOffset,
    String    newText,
    int64_t   diffOffset
) {
    LineOffsets* const offsets = &data->lineOffsets;

    for (uint32_t i = endLine + 1; i < offsets->size; i++) {
        offsets->data[i] += diffOffset;
    }

    uint32_t newLinesCount = countLines(newText);

    int64_t  lineDiff = newLinesCount - (endLine - startLine);
    uint32_t tailSize = data->lineOffsets.size - endLine - 1;
    uint32_t oldSize  = data->lineOffsets.size;

    if (lineDiff > 0) resizeLineOffsets(offsets, oldSize + lineDiff);
    memmove(data->lineOffsets.data + startLine + 1 + newLinesCount,
            data->lineOffsets.data + endLine + 1,
            tailSize * sizeof(uint32_t));
    if (lineDiff < 0) resizeLineOffsets(offsets, oldSize + lineDiff);

    uint32_t lineIdx = startLine + 1;
    for (uint32_t i = 0; i < newText.len; i++) {
        if (newText.buff[i] == '\n') {
            uint32_t absoluteOffset = startOffset + i + 1;
            data->lineOffsets.data[lineIdx] = absoluteOffset;
            lineIdx++;
        }
    }
}

void updateLineOffsets(FileData* data, String text) {
    data->lineOffsets.size = 0;
    for (int i = 0; i < text.len; i++) {
        const char ch = data->data[i];
        if (ch == '\n') pushLineOffset(&data->lineOffsets, i);
    }
}

FileData* makeFileData() {
    FileData* data = (FileData*) calloc(1, sizeof(FileData));
    if (!data) Lsp::panic({ Lsp::Err::ALLOC, { 0 } });

    data->data = { 0 };
    data->lineOffsets = { 0 };

    return data;
}

void releaseFileData(FileData* data) {
    releaseFileString(data->data);
    releaseLineOffset(&data->lineOffsets);
    free(data);
}

// Converts a position to a byte offset, clamping out-of-bounds values
uint64_t getOffset(FileData* data, Lsp::T::Position pos) {
    if (pos.line >= data->lineOffsets.size) {
        return data->data.len;
    }

    size_t lineStart = data->lineOffsets.data[pos.line];
    size_t lineEnd   = pos.line + 1 < data->lineOffsets.size
                        ? data->lineOffsets.data[pos.line + 1]
                        : data->data.len;

    size_t lineLength = lineEnd - lineStart;
    if (pos.character > lineLength) {
        return lineEnd;
    }

    return lineStart + pos.character;
}

// Converts a position to a byte offset, returning INVALID_OFFSET if out of bounds
uint64_t getStrictOffset(FileData* data, Lsp::T::Position pos) {
    if (pos.line >= data->lineOffsets.size) {
        return INVALID_OFFSET;
    }

    size_t lineStart = data->lineOffsets.data[pos.line];
    size_t lineEnd   = pos.line + 1 < data->lineOffsets.size
                        ? data->lineOffsets.data[pos.line + 1]
                        : data->data.len;

    size_t lineLength = lineEnd - lineStart;
    if (pos.character > lineLength) {
        return INVALID_OFFSET;
    }

    return lineStart + pos.character;
}

Lsp::Err::Kind
applyChanges(
    FileData* data,
    Lsp::T::Slice<Lsp::T::TextDocumentContentChangeEvent*> changes
) {
    for (int i = 0; i < changes.size; i++) {
        auto* change = changes.data[i];

        if (change->range) {
            uint32_t sOffset = getStrictOffset(data, *change->range->start);
            uint32_t eOffset = getStrictOffset(data, *change->range->end);
            if (sOffset == INVALID_OFFSET || eOffset == INVALID_OFFSET) {
                return Lsp::Err::SYNC;
            }

            int64_t diff = change->text.len - (eOffset - sOffset);
            uint32_t oldTotalLen = data->data.len;
            uint32_t newTotalLen = oldTotalLen + diff;

            if (diff > 0) resizeFileString(&data->data, newTotalLen);
            memmove(data->data + sOffset + change->text.len,
                    data->data + eOffset,
                    oldTotalLen - eOffset);
            memcpy(data->data + sOffset, change->text.buff, change->text.len);
            if (diff < 0) resizeFileString(&data->data, newTotalLen);

            updateLineOffsets(
                data,
                change->range->start->line,
                change->range->end->line,
                sOffset,
                change->text,
                diff
            );
        } else {
            resizeFileString(&data->data, change->text.len);
            memcpy(data->data, change->text, change->text.len);
            updateLineOffsets(data, change->text);
        }
    }

    return Lsp::Err::OK;
}



// Basic
// ===

void Lsp::init() {
    DArray::init(&Lsp::stack, 1024, sizeof(void*));
    FileSystem::init();
}

void Lsp::release() {
    DArray::release(&Lsp::stack);
    FileSystem::release();
}



// TextDocument handles
// ===

Lsp::Err::Kind
Lsp::handle(Lsp::TextDocument::DidOpen* method) {

    if (!method || !method->textDocument) {
        return report({ Err::INVALID_PARAMS, { 0 } });
    }

    Lsp::T::TextDocumentItem* item = method->textDocument;

    hnd = FileSystem::load(item->uri, ORIGIN);
    if (!hnd) return Err::LOAD_FILE;

    FileSystem::FileInfo* info = FileSystem::getFileInfo(hnd);
    FileData* data = (FileData*) info->userData;

    if (!data) {
        data = makeFileData();
        info->userData = data;
    }

    releaseFileString(data->data);
    data->data = makeFileString(item->text);
    data->version = item->version;

    createLineOffsets(data);

    print(data);
    return Err::OK;

}

Lsp::Err::Kind
Lsp::handle(Lsp::TextDocument::DidClose *method) {

    if (!method || !method->textDocument) {
        return report({ Err::INVALID_PARAMS, { 0 } });
    }

    Lsp::T::TextDocumentIdentifier* id = method->textDocument;

    FileSystem::Path path;
    FileSystem::uriToPath(id->uri, &path);

    void* hnd = FileSystem::getHandle({ path.buffer, path.bufferLen });
    if (!hnd) return Err::OK;

    FileSystem::FileInfo* info = FileSystem::getFileInfo(hnd);
    FileData* data = (FileData*) info->userData;
    print(data);

    if (info->userData) {
        releaseFileData(data);
        info->userData = NULL;
    }

    FileSystem::unload(hnd, ORIGIN);
    return Err::OK;

}

Lsp::Err::Kind
Lsp::handle(TextDocument::DidChange* method) {

    if (!method || !method->textDocument) {
        return report({ Err::INVALID_PARAMS, { 0 } });
    }

    int version = method->textDocument->version;

    FileSystem::Path path;
    FileSystem::uriToPath(method->textDocument->uri, &path);

    void* hnd = FileSystem::getHandle({ path.buffer, path.bufferLen });
    if (!hnd) return Err::OK;

    FileSystem::FileInfo* info = FileSystem::getFileInfo(hnd);
    FileData* data = (FileData*) info->userData;

    if (method->textDocument->version <= data->version) {
        return Err::OK;
    }
    data->version = version;

    applyChanges(data, method->contentChanges);

    print(data);
    return Err::OK;

}



// Logger
// ===
// TODO : reuse somehow compilers logger?

#include "../../src/ansi_colors.h"
#define LSP_TAG "LSP-Vi"

static void printTag() {
    fprintf(stderr, AC_BOLD "[LSP-Vi]" AC_RESET);
}

static void printFileName(String name) {
    if (name.buff && name.len > 0) {
        fprintf(stderr, AC_BRIGHT_BLACK "[%.*s]" AC_RESET, (int) name.len, name.buff);
    }
}

static void printTag(Lsp::Inf::Kind kind) {
    switch (kind) {
        case Lsp::Inf::DEBUG:
            fprintf(stderr, AC_BOLD_CYAN "[DEBUG]" AC_RESET);
            break;
        case Lsp::Inf::INFO:
        default:
            fprintf(stderr, AC_BOLD_GREEN "[INFO]" AC_RESET);
            break;
    }
}

Lsp::Inf::Kind
Lsp::report(Lsp::Inf::Info inf, const char* format, ...) {
    printTag();
    printTag(inf.kind);

    va_list argList;
    va_start(argList, format);
    vfprintf(stderr, format, argList);
    va_end(argList);

    printFileName(inf.file);
    fprintf(stderr, "\n");

    return inf.kind;
}

Lsp::Err::Kind
Lsp::report(Err::Info err, ...) {
    if (err.kind == Err::OK) return Err::OK;

    printTag();
    fprintf(stderr, AC_BOLD_RED "[ERROR]" AC_RESET);
    fprintf(stderr, ": ");

    va_list argList;
    va_start(argList, err);
    vfprintf(stderr, Err::str(err.kind), argList);
    va_end(argList);

    printFileName(err.file);
    fprintf(stderr, "\n");

    return err.kind;
}

void Lsp::panic(Err::Info err) {
    report(err);
    std::abort();
}



// Debug functions
// ===

constexpr int MAX_TEXT_PREVIEW   = 1024;
constexpr int MAX_OFFSET_PREVIEW = 16;

void print(const FileData* data) {
    if (!data) {
        printf(AC_ERROR "\nFileData: [NULL]" AC_RESET "\n");
        return;
    }

    printf(AC_SECTION "\n=== FileData (LSP State) ===" AC_RESET "\n");
    printf("  " AC_VAR "Version:      " AC_RESET AC_NUMBER "%u" AC_RESET "\n", data->version);

    printf("  " AC_VAR "Content:      " AC_RESET AC_NUMBER "%zu" AC_RESET AC_SEPARATOR "/" AC_RESET AC_NUMBER "%zu" AC_RESET " bytes (Used/Cap)\n",
           data->data.len, data->data.capacity);

    printf("  " AC_VAR "Line Offsets: " AC_RESET AC_NUMBER "%u" AC_RESET AC_SEPARATOR "/" AC_RESET AC_NUMBER "%u" AC_RESET " lines (Used/Cap)\n",
           data->lineOffsets.size, data->lineOffsets.capacity);

    // Visualizing the offset array
    printf("  " AC_VAR "Offset Map:   " AC_RESET AC_SEPARATOR "[" AC_RESET);
    uint32_t previewCount = data->lineOffsets.size > MAX_OFFSET_PREVIEW ? MAX_OFFSET_PREVIEW : data->lineOffsets.size;
    for (uint32_t i = 0; i < previewCount; i++) {
        printf(AC_NUMBER "%u" AC_RESET "%s",
               data->lineOffsets.data[i],
               (i == previewCount - 1) ? "" : AC_SEPARATOR ", " AC_RESET);
    }
    if (data->lineOffsets.size > MAX_OFFSET_PREVIEW) printf(AC_SEPARATOR ", ..." AC_RESET);
    printf(AC_SEPARATOR "]" AC_RESET "\n");

    // Preview the actual text content
    if (data->data.buff) {
        int previewLen = data->data.len > MAX_TEXT_PREVIEW ? MAX_TEXT_PREVIEW : (int)data->data.len;
        printf("  " AC_VAR "Text Preview:\n" AC_RESET "---\n" AC_TYPE "%.*s" AC_RESET AC_SEPARATOR "%s" AC_RESET "\n---\n",
               previewLen, data->data.buff,
               data->data.len > MAX_TEXT_PREVIEW ? AC_BRIGHT_BLACK "..." : "");
    }

    printf("\n");
}
