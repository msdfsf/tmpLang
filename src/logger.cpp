#include "logger.h"
#include "ansi_colors.h"
#include "globals.h"
#include "utils.h"
//#include "globals.h"

#include <cstdarg>
#include <stdio.h>
#include <stdarg.h>

constexpr int DEFAULT_MESSAGE_LENGTH = 256;
constexpr int TAB_SIZE = 4;

namespace Logger {

    uint32_t verbosity = PLAIN | HINT | INFO | WARNING | ERROR;
    uint64_t mute = 0;


    int getEndClosure(const char ch) {
        switch (ch) {
            case '(' : return ')';
            case '<' : return '>';
            case '[' : return ']';
            case '{' : return '}';
            default: return ')';
        }
    }

    // TODO : format argument {
    //  digitsAlign,
    //  color,
    //  backgroundColor
    // }
    int printSpanStrict(FILE* stream, Span* span) {

        const char* str = span->str;
        const uint64_t size = span->end.idx - span->start.idx;
        const uint64_t endIdx = span->end.idx;

        const int maxLineDigits = Utils::countDigits(span->end.ln);

        uint64_t idx = span->start.idx;
        uint64_t prevIdx = idx;
        uint64_t lineNum = span->start.ln;
        while (idx < endIdx) {
            const char ch = str[idx];
            if (ch == '\n') {
                printf(AC_BOLD_CYAN "%*llu | ", maxLineDigits, lineNum);
                fwrite(str + prevIdx, 1, idx - prevIdx, stream);
                lineNum++;
                prevIdx = idx;
            }
            idx++;
        }

        if (idx - prevIdx > 0) {
            printf(AC_BOLD_CYAN "%*llu | ", maxLineDigits, lineNum);
            fwrite(str + prevIdx, 1, idx - prevIdx, stream);
        }

        fprintf(stream, AC_RESET "\n");

        return maxLineDigits;

    }

    // messy
    void vlog (const Type type, const char* const message, Span* span, va_list args) {

        if (mute || !(verbosity & type.level)) return;

        char* body = NULL;

        int idx = 0;
        int tabCount = 0;
        int lnStartIdx = 0;
        int lnEndIdx = 0;
        int lnLength = 0;
        int underlineLen = 0;

        if (span) {
            // TODO : buggy when \0
            body = (char*) span->str;
            idx = span->start.idx;
            lnStartIdx = Utils::findLineStart(body, idx, &tabCount);
            lnEndIdx = Utils::findLineEnd(body, idx);
            lnLength = lnEndIdx - lnStartIdx + 1;
            underlineLen = span->end.idx - span->start.idx + 1;
        }

        const char* underlineEscColor = "";
        switch (type.level) {

            case INFO : {
                printf("INFO");
                break;
            }

            case WARNING : {
                underlineEscColor = AC_WARNING;
                printf(AC_WARNING "\nWARNING" AC_RESET);
                break;
            }

            case ERROR : {
                underlineEscColor = AC_ERROR;
                printf(AC_ERROR "\nERROR" AC_RESET);
                // printf("(%i, %i) : ", span->line, idx - lnStartIdx + 1);
                break;
            }

            default :
                break;

        }

        if (type.tag) {
            printf("[%s]", type.tag);
        }

        if (span) {
            printf("(%i, %i) : ", span->end.ln, idx - lnStartIdx + 1);
        } else {
            if (type.level != PLAIN) printf(" : ");
        }

        vprintf(message, args);

        if (!span) return;

        putchar('\n');

        // enough?
        char numbuff[32];
        sprintf(numbuff, "%i | ", span->end.ln);

        printf("%s%.*s\n", numbuff, lnLength, body + lnStartIdx);

        // awful
        const int tabOffset = tabCount * (TAB_SIZE - 1);
        int i = lnStartIdx;
        for (int i = 0; i < strlen(numbuff); i++) putchar(' ');
        for (; i < lnStartIdx + tabCount; i++) putchar('\t');
        for (; i < idx; i++) putchar(' ');
        printf(underlineEscColor);
        for (; i < idx + underlineLen; i++) putchar('^');
        printf(AC_RESET);
        for (; i < lnEndIdx; i++) putchar(' ');

        putchar('\n');

        printf(" in file: %.*s\n", span->fileInfo->name.len, span->fileInfo->name.buff);

        putchar('\n');

    }

    void log(const Type type, const char* const message, Span* span, ...) {
        va_list args;
        va_start(args, span);

        vlog(type, message, span, args);

        va_end(args);
    }

    void log(Type type, const char* const message) {

        if (mute) return;
        if (!(verbosity & type.level)) return;

        const char* underlineEscColor = "";
        switch (type.level) {

            case HINT : {
                break;
            }

            case INFO : {
                printf("INFO : ");
                break;
            }

            case WARNING : {
                printf(AC_WARNING "WARNING : " AC_RESET);
                break;
            }

            case ERROR : {
                printf(AC_ERROR "\nERROR " AC_RESET);
                break;
            }

            default :
                break;

        }

        printf(message);

    }

    [[noreturn]] void panic(const char* const message, Span* span, ...) {
        va_list args;
        va_start(args, span);

        Type type = {.level = ERROR, .tag = "PANIC"};
        vlog(type, message, span, args);

        va_end(args);
        exit(1);
    }

    [[noreturn]] void panic(const char* const message) {
        Type type = {.level = ERROR, .tag = "PANIC"};
        log(type, message);
    }

}
