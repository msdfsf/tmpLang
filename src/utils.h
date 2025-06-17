#pragma once
#include <ctype.h>

// #include "globals.h
#include "syntax.h"
#include <vector>
#include <filesystem>

namespace Utils {

    // ASCII string
    long str2int(char* const str, const int len, int* endIdx);

    void replaceString(String buff, String rstr, const int idx, const int len);

    int skipWhiteSpaces(char* const str, int* const idx);
    int skipWhiteSpaces(char* const str, Location* loc);
    int skipWhiteSpacesAndComments(char* const str, Location* loc);
    int skipWhiteSpacesAndComments(char* const str, int* const idx);
    int skipTillWhiteSpace(char* const str);
    int skipWhiteSpacesBack(char* const str, const int idx);

    int cmp(const char* strA, char* strB, const int lenB);

    int cmpOneChar(Operator* op, uint32_t word);
    int cmpTwoChars(Operator* op, char* const str);

    int findWordEnd(char *const str);
    int findWordEnd(char *const str, const char chA, const char chB);
    int findWordEndOrChar(char *const str, const char ch);
    int findVarEnd(char *const str);
    int findNumberEnd(char *const str);
    char* findChar(char* str, const char ch);

    int findLineStart(char* const str, const int idx, int* tabCount);
    int findLineEnd(char* const str, const int idx);
    int findClosureEnd(char* const str, const int endCh);

    int match(INamed* a, INamed* b);
    int match(INamed* const a, const char* const b);

    std::filesystem::path getExePath();

    int stripDir(char* fname);

    inline uint32_t reverse(uint32_t word);

    char* encodeUtf8(const char* const str, const int strLen, int* lenOut, int* maxCharSizeOut, int copyWhenAscii = 0);

    template<typename T>
    void pushFornt(std::vector<T> &vec, T obj) {
        vec.insert(vec.begin(), obj);
    }

    template <typename T, typename O>
    void push(std::vector<T> &vec, O* obj) {
        obj->parentIdx = vec.size();
        vec.push_back(obj);
    }

    template <typename T, typename O, typename M>
    T* findPreviousOccurrences(Scope* scope, O* obj, M member) {

        char* const name = obj->name;
        const int nameLen = obj->nameLen;
        int idx = obj->idx;

        while (scope) {

            std::vector<T*> items = scope->*member;

            for (int i = 0; i < idx; i++) {

                if (nameLen != items[i]->nameLen) continue;

                char* const itemName = items[i]->name;
                const int itemNameLen = items[i]->nameLen;

                int j = 0;
                for (; j < itemNameLen; j++) {
                    if (itemName[j] != name[j]) break;
                }

                if (j != itemNameLen) continue;
                else return items[i];
            
            }

            scope = scope->scope;
            idx = scope->parentIdx;
        
        }

        return NULL;
    
    }

    template <typename T, typename M>
    T* find(Scope* scope, char* const name, const int nameLen, M member) {

        while (scope) {

            std::vector<T*> items = scope->*member;

            for (int i = 0; i < (int) items.size(); i++) {

                if (nameLen != items[i]->nameLen) continue;

                char* const itemName = items[i]->name;
                const int itemNameLen = items[i]->nameLen;

                int j = 0;
                for (; j < itemNameLen; j++) {
                    if (itemName[j] != name[j]) break;
                }

                if (j != itemNameLen) continue;
                else return items[i];
            
            }

            scope = scope->scope;
        
        }

        return NULL;
    
    }

    template <typename T, typename M, typename I>
    T* find(I itBegin, I itEnd, char* const name, const int nameLen, M T::*member) {

        for (I it = itBegin; it != itEnd; it++) {

            M item = (*it)->*member;
            if (nameLen != item->nameLen) continue;

            char* const itemName = item->name;
            const int itemNameLen = item->nameLen;

            int j = 0;
            for (; j < itemNameLen; j++) {
                if (itemName[j] != name[j]) break;
            }

            if (j != itemNameLen) continue;
            else return *it;
        
        }
        
        return NULL;
    
    }

    template <typename T>
    T* find(std::vector<T*> &vec, char* const name, const int nameLen) {
        
        for (int i = 0; i < (int) vec.size(); i++) {

            if (nameLen != vec[i]->nameLen) continue;

            char* const itemName = vec[i]->name;
            const int itemNameLen = vec[i]->nameLen;

            int j = 0;
            for (; j < itemNameLen; j++) {
                if (itemName[j] != name[j]) break;
            }

            if (j != itemNameLen) continue;
            else return vec[i];
        
        }

        return NULL;

    }

    template <typename T>
    T find(T arr[], int arrLen, char* const name, const int nameLen) {
        
        for (int i = 0; i < (int) arrLen; i++) {

            if (nameLen != arr[i]->nameLen) continue;

            char* const itemName = arr[i]->name;
            const int itemNameLen = arr[i]->nameLen;

            int j = 0;
            for (; j < itemNameLen; j++) {
                if (itemName[j] != name[j]) break;
            }

            if (j != itemNameLen) continue;
            else return arr[i];
        
        }

        return NULL;

    }

    template <typename T>
    int findIdx(std::vector<T*> &vec, char* const name, const int nameLen) {
        
        for (int i = 0; i < (int) vec.size(); i++) {

            if (nameLen != vec[i]->nameLen) continue;

            char* const itemName = vec[i]->name;
            const int itemNameLen = vec[i]->nameLen;

            int j = 0;
            for (; j < itemNameLen; j++) {
                if (itemName[j] != name[j]) break;
            }

            if (j != itemNameLen) continue;
            else return i;
        
        }

        return -1;

    }

    template <typename T>
    int contain(T* arr, const int len, const T item) {

        for (int i = 0; i < len; i++) {
            if (item == arr[i]) return 1;
        }

        return 0;
    
    }

    constexpr int len(const char* str) {
        int len = 0;
        while (str[len] != '\0') len++;
        return len;
    }

}
