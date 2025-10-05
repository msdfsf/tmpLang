#pragma once

#include <vector>

#include "globals.h"
#include "lexer.h"
#include "syntax.h"

namespace Parser {

    int parse(char* const flname);

    Lex::Token parseScope(Span* const span, Scope* scope, const ScopeType global = SC_COMMON, const char endWithStatement = 0);
    
}