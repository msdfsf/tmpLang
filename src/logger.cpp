// #pragma once

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "logger.h"
#include "globals.h"
#include "Utils.h"
#include "lexer.h"

#define DEFAULT_MESSAGE_LENGTH 256
#define TAB_SIZE 4

#define RED_ESC "\033[1;31m"
#define YELLOW_ESC "\033[1;33m"
#define COLOR_RESET_ESC "\033[0m"

#define ERR_ESC RED_ESC
#define WARNING_ESC YELLOW_ESC

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

    // messy
    void log(const uint32_t type, const char* const message, Span* span, ...) {

        if (mute) return;
        if (!(verbosity & type)) return;

        char* body = NULL;

        int idx = 0;
        int tabCount = 0;
        int lnStartIdx = 0;
        int lnEndIdx = 0;
        int lnLength = 0;
        int underlineLen = 0;

        if (span) {
            // TODO : buggy when \0
            body = span->file->buff;
            idx = span->start.idx;
            lnStartIdx = Utils::findLineStart(body, idx, &tabCount);
            lnEndIdx = Utils::findLineEnd(body, idx);
            lnLength = lnEndIdx - lnStartIdx + 1;
            underlineLen = span->end.idx - span->start.idx + 1;
        }

        const char* underlineEscColor = "";
        switch (type) {

            case INFO : {
                printf("INFO");
                break;
            }

            case WARNING : {
                underlineEscColor = WARNING_ESC;
                printf(WARNING_ESC "\nWARNING" COLOR_RESET_ESC);
                break;
            }

            case ERROR : {
                underlineEscColor = ERR_ESC;
                printf(ERR_ESC "\nERROR" COLOR_RESET_ESC);
                // printf("(%i, %i) : ", span->line, idx - lnStartIdx + 1);
                break;
            }
        
            default :
                break;
        
        }

        if (span) {
            printf("(%i, %i) : ", span->end.ln, idx - lnStartIdx + 1);
        } else {
            if (type != PLAIN) printf(" : ");
        }

        va_list args;
        va_start(args, span);
        vprintf(message, args);
        va_end (args);

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
        printf(COLOR_RESET_ESC);
        for (; i < lnEndIdx; i++) putchar(' ');

        putchar('\n');
        
        printf(" in file: %s\n", span->file->name);
        
        putchar('\n');

    }

    void log(const uint32_t type, const char* const message) {

        if (mute) return;
        if (!(verbosity & type)) return;

        const char* underlineEscColor = "";
        switch (type) {

            case HINT : {
                break;
            }

            case INFO : {
                printf("INFO : ");
                break;
            }

            case WARNING : {
                printf("WARNING : ");
                break;
            }

            case ERROR : {
                printf(RED_ESC "\nERROR " COLOR_RESET_ESC);
                break;
            }
        
            default :
                break;
        
        }

        printf(message);

    }

}
