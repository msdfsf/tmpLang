#include "file_driver.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <filesystem>

namespace FileDriver {

    #define MAX_FILE_NAME_LENGTH 256

    //#define POSIX 0
    //#define WINDOWS 1

    // 0 on success
    #ifdef _WIN32
        #include <direct.h>
        #include <errno.h>
        int newDir(char* const path) {
            if (_mkdir(path)) {
                return (errno != EEXIST);
            }
            return 0;
        }
    #else
        #include <sys/stat.h>
        int newDir(char* const path) {
            return mkdir(path, 0777);
        }
    #endif

    FILE* openFile(char* name, int nameLen, const char* const mode) {

        char buffer[256];
        for (int i = 0; i < nameLen; i++) {
            buffer[i] = name[i];
        }
        buffer[nameLen] = '\0';

        FILE* file = fopen(buffer, mode);

        return file;

    }

    FILE* openFile(char* name, int nameLen, char* dirName, const char* const mode) {

        newDir(dirName);

        char buffer[MAX_FILE_NAME_LENGTH];

        int i = 0;
        for (; i < MAX_FILE_NAME_LENGTH; i++) {
            const char ch = dirName[i];
            if (ch != '\0') buffer[i] = ch;
            else break;
        }

        if (i + nameLen >= MAX_FILE_NAME_LENGTH - 1) {
            return NULL;
        }

        buffer[i] = '/';
        i++;

        const int offset = i;
        for (; i < offset + nameLen; i++) {
            buffer[i] = name[i - offset];
        }

        return openFile(buffer, i, mode);

    }

    int readFile(char* name, char** buffer) {

        // TODO: NULL CHECK

        FILE* file = fopen(name, "rb");
        if (!file) return -1;

        fseek(file, 0, SEEK_END);
		const int fileSize = ftell(file);
		fseek(file, 0, SEEK_SET);

        *buffer = (char*) malloc(fileSize + 1);
        fread(*buffer, 1, fileSize + 1, file);

        (*buffer)[fileSize] = '\0';

        fclose(file);
        return fileSize;

    }

    int createDirectory(char* const path) {
        return !std::filesystem::create_directory(path);
    }

}
