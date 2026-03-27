#pragma once

#include "array_list.h"
#include "data_types.h"
#include "globals.h"
#include "lexer.h"
#include "syntax.h"
#include "diagnostic.h"
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

    struct ParseContext {
        AstContext* ast;

        // The 'global' span of the file being parsed
        Span*       fileSpan;

        // State - change as we move through the tree
        Scope*      currentScope;    // The scope currently being filled
        Function*   currentFunction; // NULL if at global level
        SyntaxNode* currentLoop;

        // Stacks and Buffers
        DArray::Container nodeStack;
        DArray::Container defStack;
        Arena::Container  errBuff;

        // Counters and Indices
        int      idxInScope; // Track definition index in current scope
        uint32_t arrId;
        uint32_t varId;
        uint64_t errId;
        uint64_t defId;
    };



    // Main
    ParseContext* init(AstContext* ctx); // Call per thread, calls Lex::init()
    Err::Err      parse(ParseContext* ctx, char* const flname);
    // Manages everything automatically, call if you dont want to
    // handle threads/contexts by yourself and just want to parse
    // file to AstContext
    Err::Err      parse(char* const flname, AstContext** outAstCtx);

    // Scopes and Blocks
    Lex::Token parseScope(ParseContext* ctx, Span* span, const ScopeType type, const ScopeEnd end);
    Lex::Token parseNamespace(ParseContext* ctx, Span* span);
    Lex::Token parseForeignScope(ParseContext* ctx, Span* span);

    // Statements
    Lex::Token parseKeywordStatement(ParseContext* ctx, Span* span, const Keyword kw, Flags flags);
    Lex::Token parseBareStatement(ParseContext* ctx, Span* span, const Pos startPos, const End end);
    Lex::Token parseFreeStatement(ParseContext* ctx, Span* span);
    Lex::Token parseAllocStatement(ParseContext* ctx, Span* span);
    Lex::Token parsePrintLiteral(ParseContext* ctx, Span* span, Lex::TokenValue* startTokenVal);

    // Variables and Definitions
    Lex::Token parseLabel(ParseContext* ctx, Span* span);
    Lex::Token parseVariableAssignment(ParseContext* ctx, Span* span, const Pos startPos, const End end);
    Lex::Token parseVariableDefinition(ParseContext* ctx, Span* span, FullToken prev, const End end, Flags flags, VariableDefinition** out);
    Lex::Token parseDefinitionAssignment(ParseContext* ctx, Span* span, FullToken prev, VariableDefinition* def, const End end, Flags flags);

    // Types
    Lex::Token parseDataType(ParseContext* ctx, Span* span, FullToken prev, Flags flags, VariableDefinition* def, Lex::TokenValue* outVal);
    Lex::Token parseKnownDataType(ParseContext* ctx, Span* span, Type::Kind kind, String name, VariableDefinition* def, Lex::TokenValue* outVal);
    Lex::Token parseDataTypeDecorators(ParseContext* ctx, Span* span, Variable* var, Flags flags, Lex::TokenValue* outVal, Pointer** outPtr);
    Lex::Token parseEnumDefinition(ParseContext* ctx, Span* span);
    Lex::Token parseTypeDefinition(ParseContext* ctx, Span* span);
    Lex::Token parseTypeInitialization(ParseContext* ctx, Span* span, TypeInitialization** out);
    Lex::Token parseArrayInitialization(ParseContext* ctx, Span* span, ArrayInitialization** out);

    // Control Flow
    Lex::Token parseIfStatement(ParseContext* ctx, Span* span);
    Lex::Token parseSwitchStatement(ParseContext* ctx, Span* span);
    Lex::Token parseForLoop(ParseContext* ctx, Span* span);
    Lex::Token parseWhileLoop(ParseContext* ctx, Span* span);
    Lex::Token parseForEachLoop(ParseContext* ctx, Span* span);
    Lex::Token parseGotoStatement(ParseContext* ctx, Span* span);
    Lex::Token parseReturnStatement(ParseContext* ctx, Span* span);
    Lex::Token parseContinueStatement(ParseContext* ctx, Span* span);
    Lex::Token parseBreakStatement(ParseContext* ctx, Span* span);

    // Functions
    Lex::Token parseFunction(ParseContext* ctx, Span* span, Flags flags);
    Lex::Token parseFunctionPointer(ParseContext* ctx, Span* span, FunctionPrototype** out);

    // Expression / R-Value
    Lex::Token parseExpression(ParseContext* ctx, Span* span, Variable** outVar, const Pos startPos, const End end, const Flags flags = NULL_FLAG);
    Lex::Token parseExpression(ParseContext* ctx, Span* span, Variable* var, const Pos startPos, const End end, const Flags flags = NULL_FLAG);
    Lex::Token parseRValue(ParseContext* ctx, Span* span, Variable* outVar, const End end);
    Lex::Token parseCatch(ParseContext* ctx, Span* span, Variable* var);

    // Misc
    Lex::Token parseList(ParseContext* ctx, Span* span, Lex::TokenKind separator, Lex::TokenKind end);
    Lex::Token parseError(ParseContext* ctx, Span* span);
    Lex::Token parseImport(ParseContext* ctx, Span* span);
    Lex::Token parseDirective(ParseContext* ctx, Span* span, const Lex::Directive dir, Flags flags);
    Lex::Token parseLanguageTag(ParseContext* ctx, Span* span, String* tag);

}
