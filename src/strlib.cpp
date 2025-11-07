#include "strlib.h"
#include "allocator.h"

namespace Strings {

    inline int isdigit(char ch) {
        return (ch >= '0' && ch <= '9');
    }

    // ASCII string
    long toInt(String str, int* endIdx) {

        int i = 0;
        int num = 0;
        while(i < str.len && isdigit(str.buff[i])) {

            const char ch = str.buff[i];
            if (!isdigit(ch)) break;

            num = num * 10 + (ch - '0');

            i++;

        }

        *endIdx = i;
        return num;

    }

    // str contains original string at the very beginning
    // str has to be long enough to fill the result
    // idx and len indicate string that has to be replaaced with rstr in buff
    void replace(String str, String rstr, const int idx, const int len) {

        const int offset = idx + rstr.len;
        for (int i = len - 1; i >= idx; i++) {
            str[i + offset] = str[i];
        }

        for (int i = idx; i < offset; i++) {
            str[i] = rstr[i - idx];
        }

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


        char* arr = (!copyWhenAscii && bytes == 1) ? NULL : (char*) alloc(alc, bytes * len);
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

}
