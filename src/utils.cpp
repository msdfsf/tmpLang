#include "utils.h"

namespace Utils {

    inline int min(int a, int b) {
        return (a < b) ? a : b;
    }

    inline uint32_t reverse(uint32_t word) {
        uint32_t rev = 0;
        rev |= (word & 0xFF) << 24;
        rev |= ((word >> 8) & 0xFF) << 16;
        rev |= ((word >> 16) & 0xFF) << 8;
        rev |= ((word >> 24) & 0xFF);
        return rev;
    }

    int findLineStart(const char* str, int idx, int* tabCount) {

        int count = 0;
        while (idx > 0 && str[idx - 1] != '\n') {
            if (str[idx] == '\t') count++;
            idx--;
        }

        if (tabCount) *tabCount = count;
        return idx;

    }

    int findLineEnd(const char* str, int idx) {

        while (str[idx] != '\0') {
            if (str[idx] == '\n') return idx;
            idx++;
        }

        return idx;

    }

}
