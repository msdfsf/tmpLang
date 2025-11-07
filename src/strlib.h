#pragma once

#include "string.h"

namespace Strings {

    long toInt(String str, int* endIdx);
    void replace(String str, String rstr, const int idx, const int len);

    char* encodeUtf8(const char* const str, const int strLen, int* lenOut, int* bytesOut, int copyWhenAscii);

    inline constexpr int compare(const String strA, const String strB) {

        if (strA.len != strB.len) return 0;

        for (int i = 0; i < strA.len; i++) {
            if (strA.buff[i] != strB.buff[i]) {
                return 0;
            }
        }

        return 1;

    }

    inline constexpr int compare(const String* strA, const String strB) {

        if (strA->len != strB.len) return 0;

        for (int i = 0; i < strA->len; i++) {
            if (strA->buff[i] != strB.buff[i]) {
                return 0;
            }
        }

        return 1;

    }

    inline constexpr int compare(const String* strA, const String* strB) {

        if (strA->len != strB->len) return 0;

        for (int i = 0; i < strA->len; i++) {
            if (strA->buff[i] != strB->buff[i]) {
                return 0;
            }
        }

        return 1;

    }


    // Alphabetically compares strings
    // Returns negative value if strA comes before strB
    // Returns positive value if strA comes after strB
    // Returns 0 if strings are equal
    constexpr int acompare(const String strA, const String strB) {

        int minLen = (strA.len < strB.len) ? strA.len : strB.len;

        for (int i = 0; i < minLen; i++) {
            const int diff = (unsigned char) strA.buff[i] - (unsigned char) strB.buff[i];
            if (diff != 0) {
                return diff;
            }
        }

        return strA.len - strB.len;

    }

}
