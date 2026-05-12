#pragma once
#include "../../src/string.h"
#include "../../src/syntax.h"

// note: Lsp::State::allocator is used
namespace Lsp::Render {

    String hover(SyntaxNode* node);

};
