#pragma once

#include "array_list.h"
#include "globals.h"
#include "lexer.h"
#include "syntax.h"
#include "error.h"
#include <cstdint>

namespace Parser {

    enum {
        USE_KEYWORD_AS_END = 0x1,
        EMPTY_EXPRESSION_ALLOWED = 0x2,
        ALLOW_QUALIFIER = 0x4,
        INCLUDE_TO_TREE = 0x8,
        INCLUDE_TO_SCOPE = 0x10,
        ALLOW_UNEXPECTED_END = 0x12,
    };

    enum ScopeEnd {
        SE_DEFAULT = 0,
        SE_STATEMENT = 1,
    };

    struct FullToken {
        Lex::Token token;
        Lex::TokenValue value;
    };

    struct End {
        Lex::TokenKind a;
        Lex::TokenKind b;
    };

    void init();
    Err::Err parse(char* const flname);

    Lex::Token parseScope(Span* const span, Scope* scope, const ScopeType global = SC_COMMON, const ScopeEnd = SE_DEFAULT);
    Lex::Token parseForeignScope(Span* const span, Scope* scope);
    Lex::Token parseLabel(Span* const span, Scope* scope);
    Lex::Token parseKeywordStatement(Span* const span, Scope* scope, const Keyword keyword, Flags flags);
    Lex::Token parseDirective(Span* const span, Scope* scope, const Lex::Directive directive, Flags flags);
    Lex::Token parsePrintLiteral(Span* const span, Scope* const scope, Lex::TokenValue* const startTokenValue);
    Lex::Token parseVariableAssignment(Span* const span, Scope* scope, const Pos startPos, const End endToken);

    Lex::Token parseVariableDefinition(Span* const span, Scope* scope, FullToken prevToken, const End endToken, Flags flags, VariableDefinition** out);

    // TODO : maybe just remove 'known' version to simplify things
    Lex::Token parseDataType(Span* const span, Scope* scope, FullToken prevToken, Flags flags, VariableDefinition* def, Lex::TokenValue* outLastValue);
    Lex::Token parseKnownDataType(Span* const span, Scope* scope, DataTypeEnum dtype, String dtypeName, VariableDefinition* def, Lex::TokenValue* outLastValue);

    Lex::Token parseDataTypeDecorators(Span* const span, Scope* scope, Variable* var, Flags flags, Lex::TokenValue* outLastValue, Pointer** outLastPointer);

    Lex::Token parseBareStatement(Span* span, Scope* scope, const Pos startPos, const End endToken);

    Lex::Token parseLanguageTag(Span* span, String* tag);
    Lex::Token parseFunctionPointer(Span* const span, Scope* scope, FunctionPrototype** fcnOut);
    Lex::Token parseImport(Span* const span, Scope* sc);
    Lex::Token parseDefinitionAssignment(Span* const span, Scope* scope, FullToken prevToken, VariableDefinition* const def, const End endToken, Flags flags);
    Lex::Token parseTypeInitialization(Span* const span, Scope* scope, TypeInitialization** outTypeInit);
    Lex::Token parseRValue(Span* const span, Scope* scope, Variable* outVar, const End endToken);
    Lex::Token parseIfStatement(Span* const span, Scope* const scope);
    Lex::Token parseSwitchStatement(Span* const span, Scope* const scope);
    Lex::Token parseForLoop(Span* const span, Scope* const scope);
    Lex::Token parseFunction(Span* const span, Scope* const scope, Flags param);
    Lex::Token parseWhileLoop(Span* const span, Scope* const scope);
    Lex::Token parseGotoStatement(Span* const span, Scope* const scope);
    Lex::Token parseReturnStatement(Span* const span, Scope* const scope);
    Lex::Token parseContinueStatement(Span* const span, Scope* const scope);
    Lex::Token parseBreakStatement(Span* const span, Scope* const scope);
    Lex::Token parseForEachLoop(Span* const span, Scope* const scope);
    Lex::Token parseNamespace(Span* const span, Scope* const scope);
    Lex::Token parseEnumDefinition(Span* const span, Scope* const scope);
    Lex::Token parseError(Span* const span, Scope* const scope);
    Lex::Token parseFreeStatement(Span* const span, Scope* const scope);
    Lex::Token parseTypeDefinition(Span* const span, Scope* const scope);
    Lex::Token parseArrayInitialization(Span* span, Scope* scope, ArrayInitialization** initOut);
    Lex::Token parseAllocStatement(Span* const span, Scope* const scope);
    Lex::Token parseCatch(Span* const span, Variable* var);
    Lex::Token parseList(Span* const span, Scope* scope, Lex::TokenKind separator, Lex::TokenKind end, DArray::Container* args);

    Lex::Token parseExpression(Span* const span, Scope* scope, Variable** outVariable, const Pos startPos, const End endToken, const Flags param = 0, const int defIdx = -1);
    Lex::Token parseExpression(Span* const span, Variable* var, const Pos startPos, const End endToken, const Flags param = 0, const int defIdx = -1);

}
