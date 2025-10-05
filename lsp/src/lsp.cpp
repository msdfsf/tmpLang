#include "lsp.h"
#include "../../src/globals.h"
#include "../../src/error.h"
#include "../../src/file_driver.h"
#include "../../src/parser.h"
#include "../../src/syntax.h"
#include "translator.h"
#include "json.h"



Lsp::RequestMethod Lsp::toRequestMethod(Json::Value val) {

    if (val.type != Json::JS_STRING) return RM_UNKNOWN;

    auto it = methodMap.find(std::string(val.string.buff, val.string.len));
    if (it != methodMap.end()) return it->second;
    
    return RM_UNKNOWN;

}

Lsp::FileInfo* Lsp::getFileInfo(uint64_t id) {    
    return openedFilesLookup[id];
}

int insertFileInfo(Lsp::FileInfo* const info) {
    const uint64_t id = Lsp::openedFilesLookup.size();
    info->file.fpId = id;
    Lsp::openedFilesLookup.push_back(info);
    return Err::OK;
}

// replace slice in 'array' std::vector denoted by 'start' and 'end' idxs with 'slice' std::vector
void replaceSlice(
    std::vector<uint64_t>& array,
    int start,
    int end,
    const std::vector<uint64_t>& slice
) {

    end += 1;

    const int oldCnt = end - start;
    const int newCnt = slice.size();

    if (oldCnt > newCnt) {
        array.erase(array.begin() + start + newCnt, array.begin() + start + oldCnt);
    } else {
        const int addup = newCnt - oldCnt;
        array.resize(array.size() + addup);
        std::copy(array.begin() + end, array.end() - addup, array.begin() + end + addup);
    }

    std::copy(slice.begin(), slice.end(), array.begin() + start);

}

int toMyRange(Span* myRange, Json::Value range) {

    if (range.type != Json::JS_OBJECT) return Err::INVALID_ATTRIBUTE_NAME;

    Json::Value start = Json::value(*range.object, "start");
    Json::Value end = Json::value(*range.object, "end");
    if (start.type != Json::JS_OBJECT || end.type != Json::JS_OBJECT) {
        return Err::INVALID_ATTRIBUTE_NAME;
    }

    Json::Value startLine = Json::value(*start.object, "line");
    Json::Value startChar = Json::value(*start.object, "character");
    Json::Value endLine = Json::value(*end.object, "line");
    Json::Value endChar = Json::value(*end.object, "character");
    if (startLine.type != Json::JS_NUMBER || startChar.type != Json::JS_NUMBER ||
        endLine.type != Json::JS_NUMBER || endChar.type != Json::JS_NUMBER) {
        return Err::INVALID_ATTRIBUTE_NAME;
    }

    myRange->start.ln = (int) startLine.number;
    myRange->start.idx = (int) startChar.number;
    myRange->end.ln = (int) endLine.number;
    myRange->end.idx = (int) endChar.number;

    return Err::OK;

}

int computeLineOffsets(std::vector<uint64_t>& lineOffsets, String text) {

    lineOffsets.clear();
    lineOffsets.push_back(0);

    for (uint64_t idx = 0; idx < text.len; idx++) {
        if (text[idx] == '\n') {
            lineOffsets.push_back(idx + 1);
        }
    }

    return Err::OK;

}

int computeLineOffsets(Lsp::FileInfo* const info) {

    return computeLineOffsets(info->lineOffsets, { info->file.buff, info->file.buffLen });

}

int updateLineOffsets(Lsp::FileInfo* const info, Span* const span, String newText) {

    std::vector<uint64_t> newLineOffsets;
    computeLineOffsets(newLineOffsets, newText);

    const int oldLnCnt = span->end.ln - span->start.ln;
    const int newLnCnt = newLineOffsets.size() - 1;

    uint64_t startOffset = info->lineOffsets[span->start.ln] + span->start.idx;
    uint64_t endOffset = info->lineOffsets[span->end.ln] + span->end.idx;
    int64_t delta = newText.len - (endOffset - startOffset);

    for (int i = 0; i < newLineOffsets.size(); i++) {
        newLineOffsets[i] += startOffset;
    }

    for (int i = span->end.ln + 1; i < info->lineOffsets.size(); i++) {
        info->lineOffsets[i] += delta;
    }

    replaceSlice(info->lineOffsets, span->start.ln + 1, span->end.ln, newLineOffsets);
    
    return Err::OK;

}



int Lsp::TextDocument::didOpen(String uri, String text) {

    char* uriBuff = (char*) malloc(uri.len + 1);
    if (!uriBuff) return Err::MALLOC;
    memcpy(uriBuff, uri.buff, uri.len);
    uriBuff[uri.len] = '\0';

    char* fileBuff = (char*) malloc(text.len + 1);
    if (!fileBuff) {
        free(uriBuff);
        return Err::MALLOC;
    }
    memcpy(fileBuff, text.buff, text.len);
    fileBuff[text.len] = '\0';
    
    String id = { uriBuff, uri.len };

    FileInfo* info = new FileInfo();

    info->id = id;
    info->file.buff = fileBuff;
    info->file.buffLen = text.len;
    
    computeLineOffsets(info);

    openedFiles[id] = info;

    return Err::OK;

}

int Lsp::TextDocument::didClose(String uri) {

    auto it = openedFiles.find(uri);
    if (it == openedFiles.end()) {
        return Err::FILE_DOES_NOT_EXISTS;
    }

    FileInfo* info = it->second;
    free(info->file.buff);
    free(info->id.buff);
    delete info;

    openedFiles.erase(it);

    return Err::OK;

}

int Lsp::TextDocument::didChange(String uri, Json::Array changes) {

    auto it = openedFiles.find(uri);
    if (it == openedFiles.end()) {
        return Err::FILE_DOES_NOT_EXISTS;
    }

    FileInfo* info = it->second;

    for (int i = 0; i < changes.items.size(); i++) {

        Json::Value item = *changes.items[i];
        if (item.type != Json::JS_OBJECT) Err::INVALID_ATTRIBUTE_NAME;

        Json::Value text = Json::value(*item.object, "text");
        if (text.type != Json::JS_STRING) Err::INVALID_ATTRIBUTE_NAME;

        Json::Value range = Json::value(*item.object, "range");
        if (range.type == Json::JS_INVALID) {

            info->file.buffLen = text.string.len;
            info->file.buff = (char*) realloc(info->file.buff, text.string.len + 1);
            if (!info->file.buff) return Err::MALLOC;
            memcpy(info->file.buff, text.string.buff, text.string.len);
            info->file.buff[text.string.len] = '\0';

            return computeLineOffsets(info);
        
        }

        if (range.type != Json::JS_OBJECT) return Err::INVALID_ATTRIBUTE_NAME;

        Span span;
        int err = toMyRange(&span, range);
        if (err < 0) return err;

        const uint64_t start = info->lineOffsets[span.start.ln] + span.start.idx;
        const uint64_t end = info->lineOffsets[span.end.ln] + span.end.idx;
        const uint64_t buffLen = info->file.buffLen - (end - start) + text.string.len;
        
        char* newBuff = (char*) realloc(info->file.buff, buffLen + 1);
        if (!newBuff) return Err::MALLOC;
        info->file.buff = newBuff;
        info->file.buffLen = buffLen;

        memmove(newBuff + start + text.string.len, newBuff + end, info->file.buffLen - end);
        memcpy(newBuff + start, text.string.buff, text.string.len);
    
        updateLineOffsets(info, &span, { text.string.buff, text.string.len });

    }

    return Err::OK;

}

// we dont need to validate anything, just list of symbols
int Lsp::TextDocument::documentSymbol(const char* uri, CommProvider::Info* commInfo) {

    auto it = openedFiles.find(uri);
    if (it == openedFiles.end()) {
        fprintf(stderr, "File not opened: %s\n", uri);
        return Err::FILE_DOES_NOT_EXISTS;
    }

    FileInfo* info = it->second;

    File fileWrapper;
    fileWrapper.buff = info->file.buff;
    fileWrapper.buffLen = info->file.buffLen;
    fileWrapper.fpId = info->file.fpId;

    Span span;
    span.file = &fileWrapper;
    span.start = {0, 1};
    span.end = {0, 1};
    span.str = fileWrapper.buff;

    fprintf(stderr, "File buffer:\n%.*s\n", (int) fileWrapper.buffLen, fileWrapper.buff);

    Scope scope;
    Lex::Token lastToken = Parser::parseScope(&span, &scope, SC_GLOBAL, 0);
    fprintf(stderr, "Last token kind: %d detail: %d\n", lastToken.kind, lastToken.detail);
    if (lastToken.encoded < 0) return lastToken.encoded;

    fprintf(stderr, "Parsed successfully\n");

    LspRenderer::clearBuffer();
    LspRenderer::render(&scope, NULL);

    freeNodeRecursively(&scope);

    CommProvider::write(commInfo, LspRenderer::getBuffer());

    return Err::OK;

}