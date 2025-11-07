#pragma once

#include "stdio.h"



namespace FileDriver {

    int newDir(char* name);

    FILE* openFile(char* name, int nameLen, const char* const mode);
    FILE* openFile(char* name, int nameLen, char* dirName, const char* const mode);

    int readFile(char* name, char** buffer);
    int writeFile(FILE* file, char* buffer, int buffLen);

    int createDirectory(char* const path);
    
}
