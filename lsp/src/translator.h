#pragma once
#include "../../src/syntax.h"

namespace LspRenderer {

    String getBuffer();
    void clearBuffer();
    void render(SyntaxNode* const node, Variable* lvalue);

}