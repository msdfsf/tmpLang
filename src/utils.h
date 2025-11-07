#pragma once
#include <cstdint>

namespace Utils {

    inline uint32_t reverse(uint32_t word);

    int findLineStart(const char *body, int idx, int *tabCount);
    int findLineEnd(const char* str, int idx);

}
