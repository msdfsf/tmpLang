#pragma once

#include "file_system.h"
#include "syntax.h"

struct Translator {

    FILE* mainFile;
    int debugInfo;

    void (*init)                        (char* const dirName);
    void (*printNode)                   (FILE* file, int level, SyntaxNode* const node, Variable* lvalue);
    void (*printExpression)             (FILE* file, int level, Expression* const node, Variable* lvalue);
    void (*printForeignCode)            ();
    void (*exit)                        ();

};
