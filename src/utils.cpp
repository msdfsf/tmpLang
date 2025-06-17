// #pragma once

#include "utils.h"
#include "globals.h"
#include "logger.h"
#include "error.h"

#include <ctype.h>
#include <filesystem>
#include <cstring>

#ifdef _WIN32
    #define _AMD64_
    #include <libloaderapi.h>
#else
    #include <limits.h>
    #include <unistd.h>
#endif

namespace Utils {

    // hopefully path is not a big one to copy
    std::filesystem::path getExePath() {
        
        std::filesystem::path path;

        #ifdef _WIN32
            char buffer[MAX_PATH];
            DWORD result = GetModuleFileNameA(NULL, buffer, MAX_PATH);
            if (result > 0) {
                path = std::filesystem::path(buffer).parent_path();
            } else {
                Logger::log(Logger::ERROR, "SYSTEM: Failed to get executable path! (GetModuleFileNameA failed)");
                return {};
            }
        #else
            char buffer[PATH_MAX];
            ssize_t len = readlink("/proc/self/exe", buffer, sizeof(buffer) - 1);
            if (len != -1) {
                buffer[len] = '\0';
                path = std::filesystem::path(buffer).parent_path(); // Get the folder of the executable
            } else {
                Logger::log(Logger::ERROR, "Failed to get executable path! (readlink failed)");
                return {};
            }
        #endif

        return path;
    
    }
    
    // ASCII string
    long str2int(char* const str, const int len, int* endIdx) {

        int i = 0;
        int num = 0;
        while(i < len && isdigit(str[i])) {

            const char ch = str[i];
            if (!isdigit(ch)) break;

            num = num * 10 + (ch - '0');

            i++;

        }

        *endIdx = i;
        return num;

    }

    // buff contains original string at the very beginning
    // buff has to be long enough to fill the result
    // idx and len indicate string that has to be replaaced with rstr in buff
    void replaceString(String buff, String rstr, const int idx, const int len) {

        const int offset = idx + rstr.len;
        for (int i = len - 1; i >= idx; i++) {
            buff[i + offset] = buff[i];
        }

        for (int i = idx; i < offset; i++) {
            buff[i] = rstr[i - idx];
        }

    }

    // null-terminated
    int skipWhiteSpaces(char *const str, int *const idx) {

        int i = *idx;
        int lines = 0;
        while (1) {

            const char ch = str[i];

            if (ch > 32) {
                // assuming valid char
                *idx = i;
                return lines;
            } else if (ch == EOL) {
                lines++;
            } else if (ch == EOS) {
                *idx = i;
                return -1;
            }

            i++;
        
        }
    
    }

    // returns Err::UNEXPECTED_END_OF_FILE if end of file is reached, otherwise Err::OK
    int skipWhiteSpaces(char *const str, Location* loc) {

        int i = loc->idx;
        int lines = 0;
        while (1) {

            const char ch = str[i];

            if (ch > 32) {
                // assuming valid char
                loc->idx = i;
                loc->line += lines;
                return Err::OK;
            } else if (ch == EOL) {
                lines++;
            } else if (ch == EOS) {
                loc->idx = i;
                return Err::UNEXPECTED_END_OF_FILE; // maybe special error?
            }

            i++;
        
        }
    
    }

    int skipTillWhiteSpace(char *const str) {

        int i = 0;
        while (str[i] > 32) {
            i++;
        }

        return i;

    }

    // returns array of utf8 chars
    // where lenOut returns length of array and byteOut size of the filed in bytes
    // so, bytesOut can be 1, 2, 3, 4
    // copyWhenAscii is bool which determine if in case of pure ascii str
    // new string should be allocated and data copied.
    // https://en.wikipedia.org/wiki/UTF-8
    // expects valid utf8 string with start at the begining of code point
    char* encodeUtf8(const char* const str, const int strLen, int* lenOut, int* bytesOut, int copyWhenAscii) {

        const int mask1 = 0b01111111;
        const int mask2 = 0b11011111;
        const int mask3 = 0b11101111;

        int len = 0;
        int bytes = 1;
        for (int i = 0; i < strLen; i++) {

            const unsigned char ch = str[i];
            if (ch <= mask1) {

            } else if (ch <= mask2) {
                i++;
                bytes = 2;
            } else if (ch <= mask3) {
                i += 2;
                bytes = 3;
            } else {
                i += 3;
                bytes = 4;
            }

            len++;

        }


        char* arr = copyWhenAscii ? (char*) malloc(bytes * len) : NULL;
        switch (bytes) {

            case 1: {
                if (copyWhenAscii) memcpy(arr, str, len);
                else arr = (char*) str;
                break;
            }

            case 2: {

                uint16_t* arr16 = (uint16_t*)arr;

                for (int i = 0; i < strLen; i++) {

                    const unsigned char ch = str[i];
                    if (ch <= mask1) {
                        *arr16 = ch;
                    }
                    else if (ch <= mask2) {
                        *arr16 = *((uint16_t*)(str + i));
                        i++;
                    }

                    arr16++;

                }

                break;

            }

            case 3:
            case 4: {

                uint32_t* arr32 = (uint32_t*)arr;

                for (int i = 0; i < strLen; i++) {

                    const unsigned char ch = str[i];
                    if (ch <= mask1) {
                        *arr32 = ch;
                    }
                    else if (ch <= mask2) {
                        *arr32 = *((uint16_t*)(str + i));
                        i++;
                    }
                    else if (ch <= mask3) {
                        *arr32 = 0;
                        memcpy(arr32, str + i, 3);
                        i += 2;
                    }
                    else {
                        memcpy(arr32, str + i, 4);
                        i += 3;
                    }

                    arr32++;

                }

                break;

            }

            default:
                break;

        }

        *lenOut = len;
        *bytesOut = bytes;
        
        return arr;

    }

    // LOOK_AT : better name?
    //           optimize?
    int skipWhiteSpacesAndComments(char *const str, Location* loc) {

        int i = loc->idx;
        int lines = 0;
        while (1) {

            const char ch = str[i];
            
            if (ch == '/') {

                const char nextCh = str[i + 1];
                if (nextCh == '/') {
                    // line comment

                    i += 2;
                    while (1) {

                        const char ch = str[i];
                        if (ch == EOL || ch == EOS) break;
                        i++;
                    
                    }

                    if (str[i] == EOS) {
                        continue;
                    }

                    // i++;
                    lines++;
                
                } else if (nextCh == SCOPE_BEGIN) {
                    // block comment

                    int startIdx = i;
                    int startLines = lines;
                    int toClose = 1;

                    i += 2;
                    while (1) {

                        const char ch = str[i];

                        if (ch == '\n') {
                            lines++;
                        } else if (ch == '/') {

                            i++;
                            const char nextCh = str[i];

                            if (nextCh == SCOPE_BEGIN) {
                                toClose++;
                            } else if (nextCh == SCOPE_END) {
                                toClose--;
                                if (toClose <= 0) break;
                            }
                        
                        } else if (ch == EOS) {

                            loc->idx = i;
                            loc->line += lines;

                            if (toClose != 0) {
                                Location tmp;
                                tmp.idx = startIdx;
                                tmp.line = startLines;
                                tmp.file = loc->file;
                                Logger::log(Logger::ERROR, ERR_STR(Err::UNTERMINATED_COMMENT), &tmp, 2);
                                return Err::UNTERMINATED_COMMENT;
                            }
                            
                            return Err::UNEXPECTED_END_OF_FILE;
                        
                        }

                        i++;
                    
                    }

                    // i++;
                
                } else {

                    loc->idx = i;
                    loc->line += lines;
                    return Err::OK;
                
                }
            
            } else if (ch > 32) {
                // assuming valid char
                loc->idx = i;
                loc->line += lines;
                return Err::OK;
            } else if (ch == EOL) {
                lines++;
            } else if (ch == EOS) {
                Logger::log(Logger::ERROR, "Unexpected end of file! Showing the start of the relevant section.", loc, 1);
                loc->idx = i;
                loc->line += lines;
                return Err::UNEXPECTED_END_OF_FILE; // maybe special error?
            }

            i++;

        }

    }

    int cmpOneChar(Operator *op, uint32_t word) {
        return op->word == word & 0xFF;
    }

    int cmpTwoChars(Operator *op, char* const str) {

        const uint16_t wordA = op->word;
        const uint16_t wordB = *((uint16_t *) str);

        if (wordA < 256)
            return wordA == wordB & 0xFF;
        return wordA == wordB;
    
    }

    int cmp(const char* strA, char* strB, const int lenB) {
        
        int i = 0;
        while (i < lenB) {
            if (strA[i] != strB[i]) return 0;
            i++;
        }
        
        return strA[i] == '\0';

    }

    int findWordEnd(char *const str) {

        for (int i = 0;; i++) {
            const char ch = str[i];
            if (ch <= ' ' || ch == ';') {
                return i;
            }
        }
    
    }

    int findWordEnd(char *const str, const char chA, const char chB) {

        for (int i = 0;; i++) {
            const char ch = str[i];
            if (ch == chA || ch == chB || ch <= ' ' || ch == ';') {
                return i;
            }
        }
    
    }

    // TODO : rename to just findWordEnd
    int findWordEndOrChar(char* const str, const char ch) {

        for (int i = 0;; i++) {
            const char sch = str[i];
            if (sch == ch || sch <= ' ' || sch == ';') {
                return i;
            }
        }
    
    }

    int findVarEnd(char* const str) {

        for (int i = 0;; i++) {
            const char ch = str[i];
            // ((ch < '0' || ch > 'z') || (ch > '9' && ch < 'A') || (ch != '_' && (ch > 'Z' && ch < 'a')))
            if (!((ch >= '0' && ch <= '9') || ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'))) {
                return i;
            }
        }
    
    }

    int findNumberEnd(char* const str) {

        for (int i = 0;; i++) {

            const char ch = str[i];
            if (!(ch >= '0' && ch <= '9')) {
                return i;
            }
        
        }
    
    }

    int findLineStart(char* const str, const int idx, int* tabCount) {

        int tbCnt = 0; 
        for (int i = idx - 1; i >= 0; i--) {
            
            const char ch = str[i];
            
            if (ch == EOL) {
                *tabCount = tbCnt;
                return i + 1;
            }
            
            if (ch == '\t') tbCnt++;
        
        }

        *tabCount = tbCnt;
        return 0;

    }

    int findLineEnd(char* const str, const int idx) {

        for (int i = idx; ; i++) {
            const char ch = str[i];
            if (ch == EOL || ch == EOS) return i - 1;
        }

    }

    int skipWhiteSpacesBack(char* const str, const int idx) {

        for (int i = idx - 1; i >= 0; i--) {
            if (str[i] == EOL) return i + 1;
        }

        return 0;

    }

    char* findChar(char* str, const char ch) {
        
        while (1) {
            
            const char strCh = *str;
            
            if (strCh == EOF) return NULL;
            if (strCh == ch) return str;
            
            str++;

        }
    
    }

    int findClosureEnd(char* const str, const int endCh) {

        int i = 1;
        int toClose = 1;
        const int bgCh = str[endCh];
        while (1) {

            const int ch = str[i];
            if (ch == bgCh) {
                toClose++;
            } else if (ch == endCh) {
                if (toClose == 1) return i;
                toClose--;
            } else if (ch == EOF) {
                return 0;
            }

            i++;

        }

    }

    inline uint32_t reverse(uint32_t word) {
        uint32_t rev = 0;
        rev |= (word & 0xFF) << 24;
        rev |= ((word >> 8) & 0xFF) << 16;
        rev |= ((word >> 16) & 0xFF) << 8;
        rev |= ((word >> 24) & 0xFF);
        return rev;
    }

    int match(INamed* const a, INamed* const b) {

        if (a->nameLen != b->nameLen) return 0;

        char* const aName = a->name;
        char* const bName = b->name;
        const int len = b->nameLen;

        for (int i = 0; i < len; i++) {
            if (aName[i] != bName[i]) return 0;
        }

        return 1;
        
    }

    int match(INamed* const a, const char* const b) {

        for (int i = 0; i < a->nameLen; i++) {

            const char chA = a->name[i];
            const char chB = b[i];
            if (chB == '\0' || chA != chB) return 0;

        }

        return 1;

    }

    // fname has to be NULL terminated
    // returns offset from the start of fname where dir ends
    int stripDir(char* fname) {

        int lastOffset = 0;
        int i = 0;
        while (1) {
            
            const char strCh = fname[i];
            
            if (strCh == EOS) return lastOffset;
            if (strCh == '\\' || strCh == '/') lastOffset = i + 1;
            
            i++;

        }

    }

    VariableDefinition* createEmptyVariableDefinition() {

        VariableDefinition* def = new VariableDefinition();
        def->flags = 0;
        def->lastPtr = NULL;
        def->dtype = NULL;

        def->var = new Variable();
        def->var->cvalue.any = NULL;
        def->var->cvalue.dtypeEnum = DT_VOID;
        def->var->cvalue.hasValue = 0;

        def->var->def = def;

        return def;

    }

    void copy(Variable* dest, Variable* src) {

        Location* loc = dest->loc;
        Expression* ex = dest->expression;
        // Variable* aloc = dest->allocSize;
        // std::vector<ScopeName*> sc = dest->scopeNames;
        memcpy(dest, src, sizeof(Variable));
        dest->loc = loc;
        dest->expression = ex;
        // dest->allocSize = aloc;
        // dest->scopeNames = sc;
    
    }
    
    inline void offsetParentIdx(std::vector<SyntaxNode*> vec, const int offset) {
        for (int i = 0; i < vec.size(); i++) {
            vec[i]->parentIdx += offset;
        }
    }

    Namespace* getCopy(Namespace* nspace) {

        Namespace* newNspace = new Namespace();
        memcpy(newNspace, nspace, sizeof(Namespace));

        return newNspace;

    }

    Function* getCopy(Function* fcn) {

        Function* newFcn = new Function();
        memcpy(newFcn, fcn, sizeof(Function));

        return newFcn;

    }

    void copy(Scope* scA, Scope* scB) {
        
        Scope* parent = scB->scope;

        scA->type = scB->type;
        scA->scope = scB->scope;
        scA->loc = scB->loc;
        scA->parentIdx = scB->parentIdx;
        scA->snFlags = scB->snFlags;

        scA->children = scB->children;
        
        scA->fcn = scB->fcn;

        scA->defSearch = scB->defSearch;
        scA->defs = scB->defs;
        scA->fcns = scB->fcns;
        scA->unions = scB->unions;
        scA->labels = scB->labels;
        scA->customErrors = scB->customErrors;
        scA->customDataTypes = scB->customDataTypes;
        scA->enums = scB->enums;
        scA->namespaces = scB->namespaces;
        scA->gotos = scB->gotos;

    }
    
    #define appendVectors(a, b) (a).insert(std::begin(a), std::begin(b), std::end(b))

    // appends scB to scA
    void appendScope(Scope* scA, Scope* scB) {
        // #define appendVectors(a, b) (a).insert(std::end(a), std::begin(b), std::end(b))
        appendVectors(scA->children, scB->children);
        appendVectors(scA->codeBlocks, scB->codeBlocks);
        appendVectors(scA->customDataTypes, scB->customDataTypes);
        appendVectors(scA->enums, scB->enums);
        appendVectors(scA->fcns, scB->fcns);
        appendVectors(scA->foreignFunctions, scB->foreignFunctions);
        appendVectors(scA->langDefs, scB->langDefs);
        appendVectors(scA->namespaces, scB->namespaces);

        //appendVectors(scA->defSearch, scB->defSearch);
        //offsetParentIdx(scB->defSearch, scA->defSearch.size());
    }

    // appends scB  infront of scA
    void appendPrefixScope(Scope* scA, Scope* scB) {
        // #define appendVectors(a, b) (a).insert(std::begin(a), std::begin(b), std::end(b))
        appendVectors(scA->children, scB->children);
        appendVectors(scA->codeBlocks, scB->codeBlocks);
        appendVectors(scA->customDataTypes, scB->customDataTypes);
        appendVectors(scA->enums, scB->enums);
        //appendVectors(scA->vars, scB->vars);
        appendVectors(scA->defs, scB->defs);
        appendVectors(scA->fcns, scB->fcns);
        appendVectors(scA->foreignFunctions, scB->foreignFunctions);
        appendVectors(scA->langDefs, scB->langDefs);
        appendVectors(scA->namespaces, scB->namespaces);

        //appendVectors(scB->defSearch, scA->defSearch);
        //offsetParentIdx(scA->defSearch, scB->defSearch.size());
    }

}
