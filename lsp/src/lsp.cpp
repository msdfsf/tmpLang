#include "lsp.h"
#include "../../src/task_system.h"
#include "lsp_render.h"

#include <atomic>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>



constexpr double             GROW_COEF      = 1.5;
constexpr FileSystem::Origin ORIGIN         = FileSystem::Origins::USER_START;
constexpr uint32_t           INVALID_OFFSET = UINT32_MAX;

void print(const Lsp::FileData* data);



// Custom compiler allocator
// ===

#include "../../src/allocator.h"

// We just set alc each time before file parsing
// to specific arena we want to use.
thread_local AllocatorHandle alc = NULL;

void* alloc(AllocatorHandle allocator, size_t size) {
    return Arena::push((Arena::Container*) allocator, size);
}

void* alloc(AllocatorHandle allocator, size_t size, size_t align) {
    return Arena::push((Arena::Container*) allocator, size, align);
}

void dealloc(AllocatorHandle allocator, void* ptr) {
    return Arena::rollback((Arena::Container*) allocator, ptr);
}

void clear(AllocatorHandle allocator) {
    return Arena::clear((Arena::Container*) allocator);
}



thread_local AllocatorHandle nalc = NULL;

inline void* nalloc(AllocatorHandle allocator, AllocType type) {
    return alloc(allocator, nodeTypeSize[type]);
}

inline void* nalloc(AllocatorHandle allocator, AllocType type, size_t count) {
    return alloc(allocator, nodeTypeSize[type] * count);
}

void ndealloc(AllocatorHandle allocator, void* ptr) {
    return dealloc(allocator, ptr);
}



// Helpers
// ===

// allocates sufficient initial buffer for given string
// and copies data
Lsp::FileString makeFileString(String str) {
    Lsp::FileString out;
    out.capacity = str.len * GROW_COEF;
    out.buff = (char*) malloc(out.capacity);
    memcpy(out.buff, str.buff, str.len);
    out.len = str.len;
    return out;
}

void releaseFileString(Lsp::FileString str) {
    free(str.buff);
    str.len = 0;
}

void resizeFileString(Lsp::FileString* str, size_t newSize) {
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

void pushLineOffset(Lsp::LineOffsets* lines, uint32_t offset) {
    if (lines->size >= lines->capacity) {
        lines->capacity *= 1.5;
        lines->data = (uint32_t*) realloc(lines->data, lines->capacity * sizeof(uint32_t));
        if (!lines->data) Lsp::panic({ Lsp::Err::ALLOC, {0} });
    }
    lines->data[lines->size] = offset;
    lines->size++;
}

// Creates and computes line offsets
void createLineOffsets(Lsp::FileData* data) {
    Lsp::LineOffsets* lines = &data->lineOffsets;

    lines->capacity = 1024;
    lines->data = (uint32_t*) malloc(lines->capacity * sizeof(uint32_t));
    lines->size = 0;

    pushLineOffset(lines, 0);
    for (int i = 0; i < data->data.len; i++) {
        const char ch = data->data[i];
        if (ch == '\n') pushLineOffset(lines, i);
    }
}

void releaseLineOffset(Lsp::LineOffsets* offsets) {
    free(offsets->data);
    offsets->capacity = 0;
    offsets->size = 0;
}

void resizeLineOffsets(Lsp::LineOffsets* offsets, size_t newSize) {
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
    Lsp::FileData* data,
    uint32_t  startLine,
    uint32_t  endLine,
    uint32_t  startOffset,
    String    newText,
    int64_t   diffOffset
) {
    Lsp::LineOffsets* const offsets = &data->lineOffsets;

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

void updateLineOffsets(Lsp::FileData* data, String text) {
    data->lineOffsets.size = 0;

    pushLineOffset(&data->lineOffsets, 0);
    for (int i = 0; i < text.len; i++) {
        const char ch = data->data[i];
        if (ch == '\n') pushLineOffset(&data->lineOffsets, i);
    }
}

Lsp::FileData* makeFileData() {
    Lsp::FileData* data = (Lsp::FileData*) calloc(1, sizeof(Lsp::FileData));
    if (!data) Lsp::panic({ Lsp::Err::ALLOC, { 0 } });

    data->data = { 0 };
    data->lineOffsets = { 0 };

    int arenaCount = sizeof(Lsp::FileData::arenas) / sizeof(Arena::Container);
    for (int i = 0; i < arenaCount; i++) {
        Arena::init(data->arenas + i, 1024 * 1024 * 32);

        data->unit[i].ast = alloc<AstContext>(&Lsp::State::allocator);
        data->unit[i].reg = alloc<AstRegistry>(&Lsp::State::allocator);

        Ast::init(data->unit[i].ast);
        Ast::init(data->unit[i].reg);
    }

    return data;
}

void releaseFileData(Lsp::FileData* data) {
    releaseFileString(data->data);
    releaseLineOffset(&data->lineOffsets);
    free(data);
}

// Converts a position to a byte offset, clamping out-of-bounds values
uint64_t getOffset(Lsp::FileData* data, Lsp::T::Position pos) {
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
uint64_t getStrictOffset(Lsp::FileData* data, Lsp::T::Position pos) {
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
    Lsp::FileData* data,
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
    Ast::init();
    FileSystem::init();
    TaskSystem::init(4);
}

void Lsp::release() {
    DArray::release(&Lsp::stack);
    FileSystem::release();
}



void ensureFileAstUpdated(FileSystem::Handle fhnd) {
    TaskSystem::beginGroup();
    TaskSystem::dispatchParse(fhnd);
    TaskSystem::wait();
}

// TextDocument handles
// ===

Lsp::Err::Kind
Lsp::handle(Lsp::TextDocument::DidOpen* method) {

    if (!method || !method->textDocument) {
        return report({ Err::INVALID_PARAMS, { 0 } });
    }

    Lsp::T::TextDocumentItem* item = method->textDocument;

    FileSystem::Handle hnd = FileSystem::load(item->uri, ORIGIN);
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

    info->status = FileSystem::FS_DIRTY;
    ensureFileAstUpdated(hnd);

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

template<typename T, typename F>
SyntaxNode* handleQuery(Lsp::FileData* fdata, T* method, F fcn) {

    int idx;
    while (true) {
        // Check-in
        idx = fdata->committedIdx.load(std::memory_order_acquire);
        fdata->readerCount[idx].fetch_add(1, std::memory_order_relaxed);

        // But theoretically there is a chance, that buffers was switched and new
        // work already started as readerCount was not updated in time. We have to
        // validate.
        if (idx == fdata->committedIdx.load(std::memory_order_acquire)) {
            break;
        }

        // Unlucky. Check-out and try again
        int remaining = fdata->readerCount[idx].fetch_sub(1, std::memory_order_release);
        if (remaining == 1) fdata->readerCount[idx].notify_one();
    }

    SyntaxNode* result = fcn(fdata, method);

    // Check-out
    if (fdata->readerCount[idx].fetch_sub(1, std::memory_order_release) == 1) {
        fdata->readerCount[idx].notify_one();
    }

    return result;

}

Pos toPos(Lsp::FileData* fdata, Lsp::T::Position* lspPos) {
    if (fdata->lineOffsets.size <= lspPos->line) {
        return { -1, -1 };
    }

    return {
        .idx = (int) (fdata->lineOffsets.data[lspPos->line] + lspPos->character),
        .ln = (int) lspPos->line
    };
}

Lsp::T::Position* toLspPosition(Lsp::FileData* fdata, Pos pos) {
   using namespace Lsp;
   T::Position* position = alloc<T::Position>(&Lsp::State::allocator);

   const int line = pos.ln - 1;

   position->line = line;
   if (fdata->lineOffsets.size <= line) {
       position->character = 0;
   } else {
       position->character = pos.idx - fdata->lineOffsets.data[line];
   }

   return position;
}

Lsp::T::Range* toLspRange(Lsp::FileData* fdata, Span* span) {
    using namespace Lsp;
    T::Range* range = alloc<T::Range>(&Lsp::State::allocator);

    range->start = toLspPosition(fdata, span->start);
    range->end = toLspPosition(fdata, span->end);

    return range;
}

bool isInside(Span* span, Pos pos) {
    return (span->start.idx <= pos.idx) && (span->end.idx >= pos.idx);
}

SyntaxNode* findMostRelevantNode(SyntaxNode* root, Pos pos);

SyntaxNode* findMostRelevantNode(Expression* root, Pos pos) {
    if (!root) return NULL;

    switch (root->type) {
        case EXT_BINARY: {
            BinaryExpression* bex = (BinaryExpression*) root;

            SyntaxNode* node;
            node = findMostRelevantNode((SyntaxNode*) bex->left, pos);
            if (node) return node;

            node = findMostRelevantNode((SyntaxNode*) bex->right, pos);
            if (node) return node;

            break;
        }

        case EXT_FUNCTION_CALL: {
            FunctionCall* call = (FunctionCall*) root;

            if (isInside(call->name.span, pos)) {
                return call->fcn ? (SyntaxNode*) call->fcn : (SyntaxNode*) root;
            }

            for (uint32_t i = 0; i < call->inArgCount; i++) {
                SyntaxNode* node = findMostRelevantNode((SyntaxNode*) call->inArgs[i], pos);
                if (node) return node;
            }

            break;
        }

        case EXT_UNARY: {
            UnaryExpression* uex = (UnaryExpression*) root;

            SyntaxNode* node = findMostRelevantNode((SyntaxNode*) uex->operand, pos);
            if (node) return node;

            break;
        }

        case EXT_TERNARY: {
            TernaryExpression* tex = (TernaryExpression*) root;

            // TODO

            break;
        }

        case EXT_TYPE_INITIALIZATION: {
            TypeInitialization* init = (TypeInitialization*) root;

            for (uint32_t i = 0; i < init->attributeCount; i++) {
                SyntaxNode* node = findMostRelevantNode((SyntaxNode*) init->attributes[i], pos);
                if (node) return node;
            }

            break;
        }

        case EXT_ARRAY_INITIALIZATION: {
            ArrayInitialization* init = (ArrayInitialization*) root;

            for (uint32_t i = 0; i < init->attributeCount; i++) {
                SyntaxNode* node = findMostRelevantNode((SyntaxNode*) init->attributes[i], pos);
                if (node) return node;
            }

            break;
        }

        case EXT_CATCH: {
            Catch* ex = (Catch*) root;

            // TODO
            break;
        }

        case EXT_ALLOC: {
            Alloc* ex = (Alloc*) root;

            SyntaxNode* node = findMostRelevantNode((SyntaxNode*) ex->def, pos);
            if (node) return node;

            break;
        }

        case EXT_FREE: {
            Free* ex = (Free*) root;

            SyntaxNode* node = findMostRelevantNode((SyntaxNode*) ex->var, pos);
            if (node) return node;

            break;
        }

        case EXT_SLICE: {
            Slice* slice = (Slice*) root;

            SyntaxNode* node;

            node = findMostRelevantNode((SyntaxNode*) slice->arr, pos);
            if (node) return node;

            node = findMostRelevantNode((SyntaxNode*) slice->bidx, pos);
            if (node) return node;

            node = findMostRelevantNode((SyntaxNode*) slice->eidx, pos);
            if (node) return node;

            break;
        }

        case EXT_GET_LENGTH: {
            GetLength* len = (GetLength*) root;

            SyntaxNode* node = findMostRelevantNode((SyntaxNode*) len->arr, pos);
            if (node) return node;

            break;
        }

        case EXT_GET_SIZE: {
            GetSize* sz = (GetSize*) root;

            SyntaxNode* node = findMostRelevantNode((SyntaxNode*) sz->arr, pos);
            if (node) return node;

            break;
        }
    }

    return NULL;
}

// For now we assume that all nodes are always in order
SyntaxNode* findMostRelevantNode(SyntaxNode* root, Pos pos) {
    if (!root || !isInside(root->span, pos)) return NULL;

    switch (root->type) {
        case NT_SCOPE: {
            Scope* scope = (Scope*) root;

            for (int i = 0; i < scope->childrenCount; i++) {
                SyntaxNode* child = scope->children[i];
                SyntaxNode* node = findMostRelevantNode(child, pos);
                if (node) return node;
            }

            break;
        }

        case NT_FUNCTION: {
            Function* fcn = (Function*) root;

            for (uint32_t i = 0; i < fcn->prototype.inArgCount; i++) {
                SyntaxNode* node = findMostRelevantNode((SyntaxNode*) fcn->prototype.inArgs[i], pos);
                if (node) return node;
            }

            SyntaxNode* node = findMostRelevantNode((SyntaxNode*) fcn->bodyScope, pos);
            if (node) return node;

            break;
        }

        case NT_VARIABLE_DEFINITION: {
            VariableDefinition* def = (VariableDefinition*) root;

            SyntaxNode* node = findMostRelevantNode((SyntaxNode*) def->var, pos);
            if (node) return node;

            break;
        }

        case NT_BRANCH: {
            Branch* branch = (Branch*) root;

            for (uint32_t i = 0; i < branch->expressionCount; i++) {
                SyntaxNode* node = findMostRelevantNode((SyntaxNode*) branch->expressions[i], pos);
                if (node) node;
            }

            for (uint32_t i = 0; i < branch->scopeCount; i++) {
                SyntaxNode* node = findMostRelevantNode((SyntaxNode*) branch->scopes[i], pos);
                if (node) return node;
            }

            break;
        }

        case NT_TYPE_DEFINITION: {
            TypeDefinition* type = (TypeDefinition*) root;

            for (uint32_t i = 0; i < type->varCount; i++) {
                SyntaxNode* node = findMostRelevantNode((SyntaxNode*) type->vars[i], pos);
                if (node) return node;
            }

            break;
        }

        case NT_STATEMENT: {
            Statement* stmt = (Statement*) root;

            SyntaxNode* node = findMostRelevantNode((SyntaxNode*) stmt->operand, pos);
            if (node) return node;

            break;
        }

        case NT_VARIABLE: {
            Variable* var = (Variable*) root;

            SyntaxNode* node = (SyntaxNode*) findMostRelevantNode(var->expression, pos);
            if (node) return node;

            break;

        }

        case NT_ENUMERATOR: {
            Enumerator* type = (Enumerator*) root;

            for (uint32_t i = 0; i < type->varCount; i++) {
                SyntaxNode* node = findMostRelevantNode((SyntaxNode*) type->vars[i], pos);
                if (node) return node;
            }

            break;
        }

        case NT_FOR_LOOP: {
            ForLoop* loop = (ForLoop*) loop;

            // TODO

            break;
        }
    }

    return root;
}

Lsp::T::Hover*
Lsp::handle(Lsp::TextDocument::Hover* method) {

    FileSystem::Handle fhnd = FileSystem::getHandle(method->textDocument->uri);
    if (!fhnd) return NULL;

    FileData* fdata = (FileData*) FileSystem::getUserData(fhnd);

    SyntaxNode* node = handleQuery(fdata, method, [] (auto fdata, auto method) {
        SyntaxNode* node = findMostRelevantNode(
            (SyntaxNode*) fdata->unit->ast->root,
            toPos(fdata, method->position)
        );

        return node;
    });

    if (!node) return NULL;

    T::Hover* result = alloc<T::Hover>(&State::allocator);
    result->range = toLspRange(fdata, node->span);
    result->contents = alloc<T::MarkupContent>(&State::allocator);
    result->contents->kind = Lsp::T::MARKUP_KIND_MARKDOWN;
    result->contents->value = Lsp::Render::hover(node);

    return result;

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

void print(const Lsp::FileData* data) {
    if (!data) {
        fprintf(stderr, AC_ERROR "\nFileData: [NULL]" AC_RESET "\n");
        return;
    }

    fprintf(stderr, AC_SECTION "\n=== FileData (LSP State) ===" AC_RESET "\n");
    fprintf(stderr, "  " AC_VAR "Version:      " AC_RESET AC_NUMBER "%u" AC_RESET "\n", data->version);

    fprintf(stderr, "  " AC_VAR "Content:      " AC_RESET AC_NUMBER "%zu" AC_RESET AC_SEPARATOR "/" AC_RESET AC_NUMBER "%zu" AC_RESET " bytes (Used/Cap)\n",
           data->data.len, data->data.capacity);

    fprintf(stderr, "  " AC_VAR "Line Offsets: " AC_RESET AC_NUMBER "%u" AC_RESET AC_SEPARATOR "/" AC_RESET AC_NUMBER "%u" AC_RESET " lines (Used/Cap)\n",
           data->lineOffsets.size, data->lineOffsets.capacity);

    // Visualizing the offset array
    fprintf(stderr, "  " AC_VAR "Offset Map:   " AC_RESET AC_SEPARATOR "[" AC_RESET);
    uint32_t previewCount = data->lineOffsets.size > MAX_OFFSET_PREVIEW ? MAX_OFFSET_PREVIEW : data->lineOffsets.size;
    for (uint32_t i = 0; i < previewCount; i++) {
        fprintf(stderr, AC_NUMBER "%u" AC_RESET "%s",
               data->lineOffsets.data[i],
               (i == previewCount - 1) ? "" : AC_SEPARATOR ", " AC_RESET);
    }
    if (data->lineOffsets.size > MAX_OFFSET_PREVIEW) fprintf(stderr, AC_SEPARATOR ", ..." AC_RESET);
    fprintf(stderr, AC_SEPARATOR "]" AC_RESET "\n");

    // Preview the actual text content
    if (data->data.buff) {
        int previewLen = data->data.len > MAX_TEXT_PREVIEW ? MAX_TEXT_PREVIEW : (int)data->data.len;
        fprintf(stderr, "  " AC_VAR "Text Preview:\n" AC_RESET "---\n" AC_TYPE "%.*s" AC_RESET AC_SEPARATOR "%s" AC_RESET "\n---\n",
               previewLen, data->data.buff,
               data->data.len > MAX_TEXT_PREVIEW ? AC_BRIGHT_BLACK "..." : "");
    }

    fprintf(stderr, "\n");
}
