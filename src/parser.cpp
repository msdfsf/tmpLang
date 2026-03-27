// === Rules, conventions and overview ===
// ====                               ====
//
// During recursive parsing, a global thread-local buffer is used to temporarily
// collect these children. Once the node is completely parsed, the exact required
// memory is allocated, and the collected data is permanently committed to the AST.
//
// This also applies to cached definition nodes used for fast symbol resolution.
// Which may use its own buffer to not mess up contiguity.

// Each 'parse' function should return the last token it encountered, or an error.
// Each 'parse' function processes the input stream starting with the first token
// already consumed.
//   (We read one or more tokens first, then decide what to do. In most cases,
//    a single token is enough to decide, so we simply skip it and pass along
//    extra information about it, instead of rollback. This keeps the stack
//    smaller and prevents processing the same token twice. In case of multiple
//    tokens, we just roll back after the first one.)
//
// Each 'parse' function takes the ParseContext as the first argument and the
// parent Span as the second argument.
//   - The ParseContext tracks ambient state (current Scope, Function, stacks,
//     and IDs). Any function that modifies this state (e.g., entering a new
//     scope) must save the old state locally and restore it before returning.
//   - To ensure state restoration and span finalization, functions should
//     (if needed) use a 'defer:' label as a single exit point.
//   - The parent span's end position should be updated to match the local span
//     when the function exits.
//   - The start position of a span that the function does not own must not be modified.
//   - If previous token information is needed, it should be passed as the third
//     argument. (The type for this is not fixed and should be chosen based on needs.)
//   - If information about ending tokens is needed, it should be passed as the
//     fourth argument. If previous-token info is not required, the third argument
//     is used instead.
//   - All other arguments can be case-specific and do not need to follow any rules.
//   - To simplify function calls and signatures, it is recommended to use a
//    'flag argument' parameter to carry any additional info via flags if possible.
//
// AST nodes never use dynamic collections internally. Any node with a variable
// number of children (e.g., Scopes) must allocate memory for all of them at
// once in a single, contiguous block.
//
// During recursive parsing, buffers inside the ParseContext (nodeStack, defStack)
// are used to temporarily collect these children. Once the node is completely
// parsed, the exact required memory is allocated from the AstContext, and the
// collected data is permanently committed to the AST.
//
// This also applies to cached definition nodes used for fast symbol resolution,
// which may use its own buffer to not mess up contiguity.



#include "parser.h"

#include <cstddef>
#include <cstdint>
#include <cstring>

#include "array_list.h"
#include "data_types.h"
#include "config.h"
#include "globals.h"
#include "keywords.h"
#include "lexer.h"
#include "operators.h"
#include "string.h"
#include "syntax.h"
#include "dynamic_arena.h"
#include "file_system.h"
#include "logger.h"
#include "diagnostic.h"



namespace Parser {

    const char* const logTag = "parser";
    const Logger::Type logErr = { .level = Logger::ERROR, .tag = logTag };
    const Logger::Type logWrn = { .level = Logger::WARNING, .tag = logTag };
    const Logger::Type logHnt = { .level = Logger::HINT, .tag = logTag };



    // === Scratch buffers
    //

    typedef uint32_t StackMark;

    // returns idx
    StackMark markStack(DArray::Container* stack) {
        return stack->size;
    }

    SyntaxNode** commitStack(DArray::Container* stack, StackMark mark, uint32_t* outCount) {
        const uint32_t count = stack->size - mark;
        *outCount = count;

        if (count == 0) return NULL;

        const uint32_t bytes = count * sizeof(SyntaxNode*);
        SyntaxNode** arr = (SyntaxNode**) alloc(alc, bytes);
        memcpy(arr, ((SyntaxNode**) stack->buffer) + mark, bytes);

        stack->size = mark;

        return arr;
    }



    // === Error related stuff
    //

    inline void errorBuffInit(ParseContext* ctx) {
        Arena::init(&ctx->errBuff, 128);
    }

    inline void errorBuffClear(ParseContext* ctx) {
        Arena::clear(&ctx->errBuff);
    }

    inline void errorBuffPush(ParseContext* ctx, const char* const str) {
        const size_t strSize = cstrlen(str);
        char* buff = (char*) Arena::push(&ctx->errBuff, strSize);
        memcpy(buff + 1, str, strSize);
    }

    inline void tokenToErrorBuff(ParseContext* ctx, Lex::TokenKind token) {

        const char* const str = Lex::toStr(token);
        const size_t strSize = cstrlen(str);

        char* buff = (char*) Arena::push(&ctx->errBuff, 2 + strSize);

        buff[0] = '\'';
        memcpy(buff + 1, str, strSize);
        buff[strSize + 1] = '\'';

    }

    // returns 1 if any token was added to the buffer
    inline int endTokenToErrorBuff(ParseContext* ctx, End end) {

        if (end.a != Lex::TK_NONE) {
            tokenToErrorBuff(ctx, end.a);
        }

        if (end.b != Lex::TK_NONE) {
            if (end.a != Lex::TK_NONE) {
                char* buff = (char*) Arena::push(&ctx->errBuff, 2);
                buff[0] = ',';
                buff[1] = ' ';
            }
            tokenToErrorBuff(ctx, end.b);
        }

        return (end.a != Lex::TK_NONE || end.b != Lex::TK_NONE);

    }

    inline int isEndToken(Lex::Token token, const End& end) {
        return token.kind == end.a || token.kind == end.b;
    }

    /*
    inline void insertVariableDefinition(Scope* scope, VariableDefinition* def) {
        DArray::push(&scope->children, &def);
        DArray::push(&scope->defs, &def->var);
    }
    */

    inline void nullValue(Variable* var) {
        var->value = {};
        var->expression = NULL;
    }

    inline void offsetParentIdx(DArray::Container* vec, const int offset);
    int isArrayLHS(char* const str, Span* span);





    uint32_t varId = 0;
    // to assign each array/string initialization an id, so
    // render can easily create separate variable for them
    uint32_t arrId = 0;

    uint64_t errId = 1;

    uint64_t defId = 0;

    inline void assignId(Variable* var) {
        varId++;
        var->name.id = varId;
    }



    inline void setDefinitionIdx(ParseContext* ctx, SyntaxNode* node) {
        node->definitionIdx = ctx->idxInScope;
    }


/*
    inline Err::Err insertDefSearch(INamed* name, SyntaxNode* node, Span* span) {

        String str = String(name->buff, name->len);
        if (!OrderedDict::set(&node->scope->defSearch, str, node)) {
            Logger::log(logErr, Err::str(Err::SYMBOL_ALREADY_DEFINED), span);
            return Err::SYMBOL_ALREADY_DEFINED;
        }

        // node->parentIdx = node->scope->children.base.size;

        return Err::OK;

    }
*/



    // import related stuff


    // to represent tree of imports
    // so we can check for circular imports and log user full path
    struct ImportNodeChunk;
    struct ImportNode {
        FileSystem::Handle file;
        Namespace* fileScope;
        ImportStatement* import;
        ImportNode* parent;
        ImportNodeChunk* children;
        ImportNodeChunk* childrenLast;
    };

    struct ImportNodeChunk {
        ImportNodeChunk* link;
        ImportNode node;
    };

    ImportNode importRoot;
    ImportNode* importCurrent = NULL;

    ImportNode* initImportNode() {

        ImportNode* import = (ImportNode*) alloc(alc, sizeof(ImportNode));
        import->file = FileSystem::null;
        import->fileScope = NULL;
        import->import = NULL;
        import->parent = NULL;
        import->children = NULL;
        import->childrenLast = NULL;

        return import;

    }

    int doesImportExistInPath(ImportNode* pathNode, ImportNode* checkNode) {

        ImportNode* node = pathNode;
        while (node) {
            if (node->file == checkNode->file) {
                return 1;
            }
            node = node->parent;
        }

        return 0;

    }

    void logImportPath(ImportNode* node) {

        if (node->parent) {
            logImportPath(node->parent);
        }

        if (node->import) {
            Logger::log({ .level = Logger::PLAIN }, " -> %.*s", NULL, node->import->fname.len, node->import->fname.buff);
        } else {
            Logger::log({ .level = Logger::PLAIN }, " import path: <MAIN_FILE>");
        }

    }

    void pushImportChunk(ImportNode* node, ImportNode* children) {

        ImportNodeChunk* newChunk = (ImportNodeChunk*) nalloc(nalc, AT_BYTE, sizeof(ImportNodeChunk));
        newChunk->node = *children;
        newChunk->link = NULL;

        if (node->children) {
            node->childrenLast->link = newChunk;
        } else {
            node->children = newChunk;
        }

        node->childrenLast = newChunk;

    }



    ParseContext* init(AstContext* astCtx) {
        ParseContext* ctx = (ParseContext*) alloc(alc, sizeof(ParseContext));
        Lex::init();
        errorBuffInit(ctx);
        DArray::init(&ctx->nodeStack, 1024, sizeof(void*));
        DArray::init(&ctx->defStack, 1024, sizeof(void*));
        ctx->ast = astCtx;
        return ctx;
    }

    Err::Err parseFile(ParseContext* ctx, const FileSystem::Handle flhnd, ImportNode* import) {

        FileSystem::Path* path = FileSystem::computeRelativePath(flhnd, SyntaxNode::dir);
        if (!path) {
            Logger::log(logWrn, "Maximum file path size exceeded while computing relative path!");
        }

        Span span;
        span.fileInfo = FileSystem::getFileInfo(flhnd);
        span.str = FileSystem::getBuffer(flhnd);
        span.start = { 0, 1 };
        span.end = { -1, 1 }; // in overflow we trust

        importCurrent = import;

        ctx->fileSpan = &span;
        ctx->currentScope = (Scope*) import->fileScope;
        ctx->ast->root = ctx->currentScope; // TODO

        Lex::Token token = parseScope(ctx, &span, SC_GLOBAL, SE_DEFAULT);
        return (Err::Err) token.encoded;

    }

    Err::Err processImport(ParseContext* ctx, ImportNode* currentNode, String fpath) {

        // 1) process all children imports in current file
        ImportNodeChunk* chunk = currentNode->children;
        while (chunk) {

            ImportNode* const importNode = &chunk->node;
            ImportStatement* import = importNode->import;

            FileSystem::Handle flhnd = FileSystem::load(fpath, import->fname);
            if (importRoot.file == FileSystem::null) {
                Logger::log(logErr, "File %.*s not found in path %.*s!", NULL, fpath.len, fpath.buff, import->fname.len, import->fname.buff);
                return Err::FILE_DOES_NOT_EXISTS;
            }

            Namespace* fscope = (Namespace*) FileSystem::getUserData(flhnd);

            if (!fscope) {

                importNode->fileScope = Ast::Node::makeNamespace();
                FileSystem::setUserData(flhnd, importNode->fileScope);

                const Err::Err err = parseFile(ctx, flhnd, importNode);
                if (err != Err::OK) return err;

            } else {

                importNode->fileScope = fscope;

            }

            importNode->file = flhnd;
            ((SyntaxNode*) importNode->fileScope)->type = NT_SCOPE;

            if (doesImportExistInPath(currentNode, importNode)) {
                Logger::log(logErr, Err::str(Err::CIRCULAR_IMPORT), import->base.span);
                logImportPath(importNode);
                return Err::CIRCULAR_IMPORT;
            }

            Namespace* root = importNode->parent->fileScope ?
                importNode->parent->fileScope : (Namespace*) SyntaxNode::root;
            switch (import->keyword) {

                case KW_VOID : {

                    // import foo from file

                    int symbolType = -1;
                    SyntaxNode* symbol = NULL;

                    Namespace* nsc = (Namespace*) (importNode->fileScope);
                    for (int i = 0; i < nsc->scope.childrenCount; i++) {

                        SyntaxNode* node = ((SyntaxNode*) nsc->scope.children) + i;
                        if (node->type == NT_NAMESPACE) {

                            Namespace* nspace = (Namespace*) node;
                            if (nspace->name.len == import->param.len && strncmp(nspace->name.buff, import->param.buff, import->param.len) == 0) {
                                symbolType = NT_NAMESPACE;
                                symbol = (SyntaxNode*) nspace;
                                break;
                            }

                        } else if (node->type == NT_FUNCTION) {

                            Function* fcn = (Function*) node;
                            if (fcn->name.len == import->param.len && strncmp(fcn->name.buff, import->param.buff, import->param.len) == 0) {
                                symbolType = NT_FUNCTION;
                                symbol = (SyntaxNode*) fcn;
                                break;
                            }

                        }

                    }

                    if (symbolType < 0) {
                        Logger::log(logErr, "Aprepritee symbol not found! Note, only namespaces and functions can be exported.", import->base.span);
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    switch (symbolType) {

                        case NT_NAMESPACE : {

                            Namespace* sc = Ast::Node::copy((Namespace*) symbol);

                            sc->scope = root->scope;
                            sc->scope.base.definitionIdx = 0;

                            //DArray::pushFront(&root->scope.children, &sc);
                            //DArray::pushFront(&root->scope.namespaces.base, &sc);

                            break;

                        }

                        case NT_FUNCTION : {

                            Function* fcn = Ast::Node::copy((Function*) symbol);

                            fcn->base.scope = (Scope*) root;
                            fcn->base.definitionIdx = 0;

                            //DArray::pushFront(&root->scope.children, &fcn);
                            //DArray::pushFront(&root->scope.fcns.base, &fcn);

                            break;

                        }

                        default:
                            Logger::log(logErr, "Aprepritee symbol not found! Note, only namespaces and functions can be exported.", import->base.span);
                            return Err::UNEXPECTED_SYMBOL;

                    }

                    break;

                }

                case KW_SCOPE : {

                    Scope* sc = (Scope*) (importNode->fileScope);

                    sc->base.scope = (Scope*) root;
                    sc->base.type = NT_SCOPE;
                    sc->base.definitionIdx = 0;

                    //DArray::pushFront(&root->scope.children.base, &sc);

                    break;

                }

                case KW_FCN : {

                    Function* fcn = Ast::Node::makeFunction();

                    fcn->bodyScope = (Scope*) (importNode->fileScope);
                    fcn->name.buff = import->param;
                    fcn->name.len = import->param.len;
                    fcn->base.scope = (Scope*) root;
                    fcn->base.type = NT_FUNCTION;
                    fcn->prototype.outArg = Ast::Node::makeVariableDefinition();
                    fcn->prototype.outArg->var->value.typeKind = Type::DT_VOID;
                    fcn->base.definitionIdx = 0;
                    fcn->base.flags = 0;

                    fcn->name.id = varId;
                    varId++;

                    // as we importing function, order doesn't matter, so we can push it back
                    //DArray::push(&root->scope.children.base, &fcn);
                    //DArray::push(&root->scope.fcns.base, &fcn);

                    break;

                }

                case KW_NAMESPACE : {

                    Namespace* sc = (Namespace*) (importNode->fileScope);

                    sc->name.buff = import->param;
                    sc->name.len = import->param.len;
                    sc->scope = root->scope;
                    sc->scope.base.type = NT_NAMESPACE;
                    sc->scope.base.definitionIdx = 0;
                    sc->scope.base.flags = 0;

                    //DArray::pushFront(&root->scope.children.base, &sc);
                    //DArray::pushFront(&root->scope.namespaces.base, &sc);

                    // in case of namespace we dont need to update parentIdx for searchDefs
                    // but may be wrong..

                    break;

                }

                default : {

                    Logger::log(logErr, "Unsupported import keyword!", import->base.span);
                    return Err::UNEXPECTED_SYMBOL;

                }

            }

            chunk = chunk->link;

        }

        // 2) process new imports of each children
        chunk = currentNode->children;
        while (chunk) {
            const Err::Err err = processImport(ctx, &chunk->node, fpath);
            if (err != Err::OK) return err;
            chunk = chunk->link;
        }

        return Err::OK;

    }

    Err::Err parse(ParseContext* ctx, char* const flname) {

        Err::Err err;

        SyntaxNode::root = Ast::Node::makeScope();
        SyntaxNode::root->base.scope = NULL;
        SyntaxNode::root->base.definitionIdx = 0;

        importRoot.file = FileSystem::load(String(flname));
        if (importRoot.file == FileSystem::null) {
            Logger::log(logErr, "File %s not found!", NULL, flname);
            return Err::FILE_DOES_NOT_EXISTS;
        }

        SyntaxNode::dir = FileSystem::getDirectory(importRoot.file);

        importRoot.fileScope = (Namespace*) SyntaxNode::root;
        importRoot.import = NULL;
        importRoot.parent = NULL;

        ctx->ast->usedFunctionMask = 0;

        err = parseFile(ctx, importRoot.file, &importRoot);
        if (err != Err::OK) return err;

        err = processImport(ctx, &importRoot, SyntaxNode::dir);
        if (err != Err::OK) return err;

        return Err::OK;

    }

    Err::Err parse(char* const flname, AstContext** outAstCtx) {
        *outAstCtx = Ast::init();
        ParseContext* ctx = init(*outAstCtx);
        return parse(ctx, flname);
        //return Err::OK;
    }



























    Lex::Token tryResolveError(ParseContext* ctx, Span* span, Lex::Token startToken) {

        // Recovery mode
        if constexpr (Config::ERROR_RECOVERY_ENABLED) {
            if (startToken.encoded < 0) return startToken;
            Lex::Token token = Lex::syncToken(span, { Lex::TK_STATEMENT_END }, { Lex::TK_SCOPE_END });
            return token;
        }

        // Strict mode, we just act as a dumb function and pray
        // that we get optimized away.
        return startToken;
    }

    Lex::Token parseScope(ParseContext* ctx, Span* span, const ScopeType type, const ScopeEnd end) {

        SpanEx    lspan = markSpanStart(span);
        StackMark smark = markStack(&ctx->nodeStack);

        Scope* currentScope = ctx->currentScope;
        int    idxInscope   = ctx->idxInScope;

        Lex::TokenValue tokenVal;
        Lex::Token token;

        Scope* node;

        if (type == SC_COMMON) {
            node = Ast::Node::makeScope();
            node->base.scope = currentScope;
            ctx->idxInScope = 0;
        } else {
            node = currentScope;
        }

        ctx->currentScope = node;

        Pos prevPos = lspan.start;
        Lex::Token prevToken = Lex::toToken(Lex::TK_NONE);

        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.encoded < 0) goto defer;

            switch (token.kind) {

                case Lex::TK_STATEMENT_END : continue;

                case Lex::TK_END : {

                    if (type == SC_GLOBAL) {
                        goto defer;
                    }

                    span->end = prevPos;

                    Logger::log(logErr, "Unexpected end of file! Showing the start of the relevant section.", &lspan);
                    token = Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
                    goto defer;

                }

                case Lex::TK_SCOPE_BEGIN : {

                    Scope* currentScope = ctx->currentScope;

                    token = parseScope(ctx, &lspan, SC_COMMON, SE_DEFAULT);
                    if (token.encoded < 0) goto defer;

                    break;

                }

                case Lex::TK_SCOPE_END : {

                    if (type != SC_GLOBAL) {
                        node->children = commitStack(&ctx->nodeStack, smark, &node->childrenCount);
                        goto defer;
                    }

                    Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan, "");
                    token = Lex::toToken(Err::UNEXPECTED_SYMBOL);
                    goto defer;

                }

                case Lex::TK_STRING : {

                    token = parsePrintLiteral(ctx, &lspan, &tokenVal);
                    if (token.encoded < 0) return token;
                    break;

                }

                case Lex::TK_KEYWORD : {

                    token = parseKeywordStatement(ctx, &lspan, (Keyword) token.detail, INCLUDE_TO_SCOPE);
                    if (token.encoded < 0) return token;
                    break;

                }

                case Lex::TK_DIRECTIVE : {

                    token = parseDirective(ctx, &lspan, (Lex::Directive) token.detail, NULL_FLAG);
                    if (token.encoded < 0) return token;
                    break;

                }

                case Lex::TK_ARRAY_BEGIN : {

                    token = parseForeignScope(ctx, &lspan);
                    if (token.encoded < 0) return token;
                    break;

                }

                case Lex::TK_STATEMENT_BEGIN : {

                    token = parseLabel(ctx, &lspan);
                    if (token.encoded < 0) return token;
                    break;

                }

                case Lex::TK_IDENTIFIER : {
                    // bare statement or custom data type definition

                    FullToken startToken = { token, tokenVal };
                    Pos startPos = { lspan.start.idx - 1, lspan.start.ln };

                    token = Lex::peekTokenSkipDecorators(&lspan, NULL);
                    if (token.kind == Lex::TK_IDENTIFIER) {
                        token = parseVariableDefinition(ctx, &lspan, startToken, End { Lex::TK_STATEMENT_END }, INCLUDE_TO_SCOPE, NULL);
                    } else {
                        token = parseBareStatement(ctx, &lspan, startPos, End { Lex::TK_STATEMENT_END });
                    }

                    if (token.encoded < 0) return token;
                    break;

                }

                default : {
                    // bare statement or error

                    token = parseBareStatement(ctx, &lspan, prevPos, End { Lex::TK_STATEMENT_END });
                    if (token.encoded < 0) return token;
                    break;

                }

            }

            if (end == SE_STATEMENT) {
                if (token.kind == Lex::TK_STATEMENT_END) {
                    // TODO : make it happen only once per function
                    // node->children = commitStack(&ctx->nodeStack, smark, &node->childrenCount);
                    return token;
                }
                Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            prevPos = lspan.end;
            prevToken = token;

        }

        defer:
        ctx->currentScope = currentScope;
        ctx->idxInScope   = idxInscope;

        node->children = commitStack(&ctx->nodeStack, smark, &node->childrenCount);
        node->base.span = finalizeSpan(&lspan, span);

        return token;

    }

    Lex::Token parsePrintLiteral(ParseContext* ctx, Span* span, Lex::TokenValue* startTokenVal) {

        SpanEx    lspan = markSpanStart(span);
        StackMark smark = markStack(&ctx->nodeStack);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        ctx->ast->usedFunctionMask |= (1 << Ast::Internal::IF_PRINTF);

        Function* const fcn = Ast::Internal::functions + Ast::Internal::IF_PRINTF;

        FunctionCall* call = Ast::Node::makeFunctionCall();
        call->fcn = fcn;
        call->name.buff = fcn->name.buff;
        call->name.len = fcn->name.len;

        Variable* format = Ast::Node::makeVariable();
        format->base.scope = ctx->currentScope;
        format->value.typeKind = Type::DT_STRING;
        format->base.span = getSpanStamp(&lspan);
        format->value.any = startTokenVal->any;

        Variable* callWrapper = Ast::Node::makeVariable();
        callWrapper->base.scope = ctx->currentScope;
        callWrapper->expression = (Expression*) call;
        callWrapper->value.str = NULL;

        DArray::push(&ctx->nodeStack, &format);
        token = parseList(ctx, &lspan, Lex::TK_LIST_SEPARATOR, Lex::TK_STATEMENT_END);
        if (token.encoded < 0) return token;
        call->inArgs = (Variable**) commitStack(&ctx->nodeStack, smark, &call->inArgCount);

        DArray::push(&ctx->ast->reg->fcnCalls, &callWrapper);
        DArray::push(&ctx->nodeStack, &callWrapper); // scope

        callWrapper->base.span = finalizeSpan(&lspan, span);

        return token;

    }

    Lex::Token parseLanguageTag(ParseContext* ctx, Span* span, String* tag) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(span, &tokenVal);
        QualifiedName* qname = (QualifiedName*) tokenVal.any;

        if (token.kind != Lex::TK_IDENTIFIER) {
            ndealloc(nalc, qname);
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (qname->pathSize != 0) {
            Logger::log(logErr, "Language tag cannot be a qualified name!", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        tag->buff = qname->buff;
        tag->len = qname->len;

        ndealloc(nalc, qname);

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind != Lex::TK_ARRAY_END) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        return token;

    }

    Lex::Token parseForeignScope(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        String tag;
        Lex::Token token;

        token = parseLanguageTag(ctx, &lspan, &tag);
        if (token.encoded < 0) return token;

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        const char* const codeStr = span->str + span->end.idx;
        const int codeLen = Lex::findBlockEnd(&lspan, Lex::SCOPE_BEGIN, Lex::SCOPE_END);
        if (codeLen < 0) {
            // ERROR
            Logger::log(logErr, Err::str(Err::UNEXPECTED_END_OF_FILE), span);
            return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
        }

        CodeBlock* codeBlock = Ast::Node::makeCodeBlock();
        codeBlock->code.tagStr = String(tag.buff, tag.len);
        codeBlock->code.codeStr = String((char*) codeStr, codeLen);

        DArray::push(&ctx->ast->reg->codeBlocks, &codeBlock);

        codeBlock->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseLabel(ParseContext* ctx, Span* span) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(&lspan, &tokenVal);

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan, "Identifier");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        Label* label = Ast::Node::makeLabel();
        label->base.scope = ctx->currentScope;
        label->name.buff = tokenVal.str->buff;
        label->name.len = tokenVal.str->len;
        setDefinitionIdx(ctx, &label->base);

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_LABEL_END) {
            uint64_t val = Lex::toIntStr(Lex::LABEL_END);
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan, &val);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        DArray::push(&ctx->nodeStack, &label);
        DArray::push(&ctx->ast->reg->labels, &label);
        DArray::push(&ctx->defStack, &label);

        label->span = finalizeSpan(&lspan, span);
        return token;

    }

    // assignment or expression
    Lex::Token parseBareStatement(ParseContext* ctx, Span* span, const Pos startPos, const End endToken) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        Variable* lvar;
        token = parseExpression(ctx, &lspan, &lvar, startPos, endToken, ALLOW_UNEXPECTED_END);
        if (token.encoded < 0) return token;

        if (isEndToken(token, endToken)) {

            Statement* stmt = Ast::Node::makeStatement();
            stmt->base.scope = ctx->currentScope;
            stmt->operand = lvar;

            DArray::push(&ctx->nodeStack, &stmt);
            DArray::push(&ctx->ast->reg->statements, &stmt);

            stmt->base.span = finalizeSpan(&lspan, span);
            return token;

        }

        if (token.kind != Lex::TK_EQUAL) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan, "'='");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        };

        Variable* rvar = Ast::Node::makeVariable();
        token = parseRValue(ctx, &lspan, rvar, endToken);
        if (token.encoded < 0) return token;

        VariableAssignment* varAssignment = Ast::Node::makeVariableAssignment();
        varAssignment->base.span = span;
        varAssignment->base.scope = ctx->currentScope;
        varAssignment->lvar = lvar;
        varAssignment->rvar = rvar;

        // TODO : move to parseRValue?
        if (lvar->value.typeKind == Type::DT_ARRAY) {
            DArray::push(&ctx->ast->reg->arraysAllocations, &varAssignment);
        }

        DArray::push(&ctx->ast->reg->variableAssignments, &varAssignment);
        DArray::push(&ctx->nodeStack, &varAssignment);

        varAssignment->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseVariableAssignment(ParseContext* ctx, Span* span, const Pos startPos, const End endToken) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        VariableAssignment* const varAssignment = Ast::Node::makeVariableAssignment();
        varAssignment->base.span = span;
        varAssignment->base.scope = ctx->currentScope;
        varAssignment->rvar = Ast::Node::makeVariable();
        varAssignment->rvar->base.scope = ctx->currentScope;

        token = parseExpression(ctx, &lspan, &(varAssignment->lvar), startPos, End { Lex::TK_EQUAL }, NULL_FLAG);
        if (token.encoded < 0) return token;

        if (token.kind != Lex::TK_EQUAL) {
            uint64_t val = Lex::toIntStr(Lex::EQUAL);
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan, &val);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = parseRValue(ctx, &lspan, varAssignment->rvar, endToken);
        if (token.encoded < 0) return token;

        // TODO : move to parseRValue?
        if (varAssignment->lvar->value.typeKind == Type::DT_ARRAY) {
            DArray::push(&ctx->ast->reg->arraysAllocations, (void*) &varAssignment);
        }

        DArray::push(&ctx->ast->reg->variableAssignments, (void*) &varAssignment);
        DArray::push(&ctx->nodeStack, (void*) &varAssignment);

        varAssignment->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseKnownDataType(ParseContext* ctx, Span* const span, Type::Kind type, String dtypeName, VariableDefinition* def, Lex::TokenValue* outLastValue) {

        Lex::Token token;

        if (type == Type::DT_CUSTOM) {

            def->var->value.typeKind = Type::DT_CUSTOM;
            def->var->value.any = NULL;
            def->dtype = Ast::Node::makeQualifiedName();
            def->dtype->buff = dtypeName.buff;
            def->dtype->len = dtypeName.len;

            DArray::push(&ctx->ast->reg->customDataTypesReferences, &def);

        } else if (type == Type::DT_FUNCTION) {

            FunctionPrototype* fptr;
            token = parseFunctionPointer(ctx, span, &fptr);
            if (token.encoded < 0) return token;

            def->var->value.fcn = fptr;
            def->var->value.typeKind = Type::DT_FUNCTION;

        } else {

            def->var->value.typeKind = type;
            def->var->value.any = Type::basicTypes + type;

        }

        Pointer* lastPtr = NULL;
        token = parseDataTypeDecorators(ctx, span, def->var, ALLOW_QUALIFIER, outLastValue, &lastPtr);
        if (token.encoded < 0) return token;

        def->lastPtr = lastPtr;
        def->var->base.span = getSpanStamp(span);

        return token;

    }

    // def->flags will be rewritten
    Lex::Token parseDataType(ParseContext* ctx, Span* span,FullToken prevToken, Flags flags, VariableDefinition* def, Lex::TokenValue* outLastValue) {

        Lex::Token token = prevToken.token;
        Lex::TokenValue tokenVal = prevToken.value;

        def->base.span = getSpanStamp(span);

        if (token.kind == Lex::TK_KEYWORD && (token.detail == KW_CONST || token.detail == KW_EMBED)) {

            if (!(flags & ALLOW_QUALIFIER)) {
                Logger::log(logErr, "Qualifier not expected here!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            def->base.flags = ((token.kind == KW_CONST) ? KW_CONST : KW_EMBED); // TODO
            token = Lex::nextToken(span, &tokenVal);

        } else {
            def->base.flags = 0;
        }

        def->var = Ast::Node::makeVariable();
        def->var->base.scope = ctx->currentScope;
        def->var->def = def;
        def->var->expression = NULL;
        def->base.scope = ctx->currentScope;

        if (token.kind == Lex::TK_IDENTIFIER) {

            token = parseKnownDataType(ctx, span, Type::DT_CUSTOM, *tokenVal.str, def, outLastValue);

        } else {

            if (!Lex::isDtype(token)) {
                Logger::log(logErr, "Data type expected!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            token = parseKnownDataType(ctx, span, Lex::toDtype(token), { NULL, 0 }, def, outLastValue);

        }

        // *val = tokenVal;
        return token;

    }

    Lex::Token parseFunctionPointer(ParseContext* ctx, Span* const span, FunctionPrototype** fcnOut) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        VariableDefinition* def;
        FunctionPrototype* fcn = Ast::Node::makeFunctionPrototype();

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_PARENTHESIS_BEGIN) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_ARROW) {
            StackMark smark = markStack(&ctx->nodeStack);
            while (1) {

                def = Ast::Node::makeVariableDefinition();
                DArray::push(&ctx->nodeStack, &def);

                token = parseDataType(ctx, &lspan, { token, tokenVal }, ALLOW_QUALIFIER, def, &tokenVal);
                if (token.encoded < 0) return token;

                if (token.kind == Lex::TK_LIST_SEPARATOR) {
                    token = Lex::nextToken(&lspan, NULL);
                    continue;
                } else if (token.kind == Lex::TK_ARROW) {
                    break;
                } else {
                    Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

            }

            fcn->inArgs = (VariableDefinition**) commitStack(&ctx->nodeStack, smark, &fcn->inArgCount);
        }

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_PARENTHESIS_END) {

            def = Ast::Node::makeVariableDefinition();
            token = parseDataType(ctx, &lspan, { token, tokenVal }, NULL_FLAG, def, &tokenVal);
            if (token.encoded < 0) return token;

            if (token.kind != Lex::TK_PARENTHESIS_END) {
                Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        } else {

            def = Ast::Node::makeVariableDefinition();

        }

        fcn->outArg = def;
        *fcnOut = fcn;

        def->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    // part starting with variable name in definition
    // ex: const int^ x ... from x
    Lex::Token parseDefinitionAssignment(ParseContext* ctx, Span* const span, FullToken prevToken, VariableDefinition* const def, const End endToken, Flags flags) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        QualifiedName* qname = (QualifiedName*) prevToken.value.any;
        def->var->name = *qname;

        assignId(def->var);

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind == Lex::TK_EQUAL) {

            // if var contains name, answer has to be wrapped
            // into unary expression, as name cannot be altered.

            QualifiedName qname = (QualifiedName) def->var->name;
            def->var->name = { 0 };

            // DataTypeEnum leftDtype = def->var->cvalue.dtypeEnum;

            token = parseRValue(ctx, span, def->var, endToken);
            if (token.encoded < 0) return token;

            // if numeric literal we have to cast it instead of
            // creating wrapper node
            //if (Validator::isBasicDtype(def->var->cvalue.dtypeEnum)) {
            //    Validator::castLiteral(&def->var->cvalue, leftDtype);
            //}

            if (def->var->name.buff) {
                UnaryExpression* uex = (UnaryExpression*) nalloc(nalc, AT_EXT_UNARY);
                uex->base.opType = OP_NONE;
                uex->operand = def->var;

                def->var = (Variable*) nalloc(nalc, NT_VARIABLE);
                def->var->expression = (Expression*) uex;
                def->var->base.scope = uex->operand->base.scope;
            }

            def->var->name = qname;

            if (def->base.flags & IS_CMP_TIME) {
                DArray::push(&ctx->ast->reg->cmpTimeVars, &def->var);
            } else {
                DArray::push(&ctx->ast->reg->initializations, (void*) &def);
            }

        } else if (isEndToken(token, endToken) || flags & USE_KEYWORD_AS_END) {

            def->var->expression = NULL;

        } else {

            errorBuffClear(ctx);
            if (endTokenToErrorBuff(ctx, endToken)) {
                errorBuffPush(ctx, ", ");
            }
            tokenToErrorBuff(ctx, Lex::TK_EQUAL);

            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span, ctx->errBuff);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        return token;

    }

    Lex::Token parseVariableDefinition(ParseContext* ctx, Span* const span, FullToken prevToken, const End endToken, Flags param, VariableDefinition** outVarDef) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        VariableDefinition* def = Ast::Node::makeVariableDefinition();
        def->var->base.scope = ctx->currentScope;

        if (Lex::isDtype(prevToken.token)) {
            Type::Kind type = Lex::toDtype((Keyword) prevToken.token.detail);
            token = parseKnownDataType(ctx, &lspan, type, { NULL, 0 }, def, &tokenVal);
        } else {
            token = parseDataType(ctx, &lspan, prevToken, ALLOW_QUALIFIER, def, &tokenVal);
        }

        if (token.encoded < 0) return token;

        token = parseDefinitionAssignment(ctx, &lspan, { token, tokenVal }, def, endToken, param);
        if (token.encoded < 0) return token;

        if (outVarDef) {
            *outVarDef = def;
        }

        if (Lex::isKeyword(prevToken.token, KW_EMBED)) {
            DArray::push(&ctx->ast->reg->cmpTimeVars, &def->var);
        }

        // TODO : we may want to split def search insert to seprate flag
        if (param & INCLUDE_TO_SCOPE) {
            setDefinitionIdx(ctx, (SyntaxNode*) def->var);
            DArray::push(&ctx->defStack, def);
            DArray::push(&ctx->nodeStack, def);
            DArray::push(&ctx->ast->reg->cmpTimeVars, &def->var);
            DArray::push(&ctx->ast->reg->variableDefinitions, &def);
        }

        def->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseArrayInitialization(ParseContext* ctx, Span* span, ArrayInitialization** initOut) {
        *initOut = Ast::Node::makeArrayInitialization();
        StackMark smark = markStack(&ctx->nodeStack);
        Lex::Token token = parseList(ctx, span, Lex::TK_LIST_SEPARATOR, Lex::TK_ARRAY_END);
        (*initOut)->attributes = (Variable**) commitStack(&ctx->nodeStack, smark, &(*initOut)->attributeCount);
        return token;
    }

    Lex::Token parseTypeInitialization(ParseContext* ctx, Span* const span, TypeInitialization** outTypeInit) {

        SpanEx    lspan = markSpanStart(span);
        StackMark smark = markStack(&ctx->nodeStack);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        TypeInitialization* typeInit = Ast::Node::makeTypeInitialization();

        token = Lex::nextToken(&lspan, &tokenVal);
        Lex::Token nextToken = Lex::peekToken(&lspan);

        if (nextToken.kind == Lex::TK_STATEMENT_BEGIN || token.kind == Lex::TK_THE_REST) {
            // names assumed

            int hasFillVar = 0;
            typeInit->fillVar = NULL;

            while (1) {

                Variable* var = Ast::Node::makeVariable();
                var->base.scope = ctx->currentScope;
                var->base.span = getSpanStamp(&lspan);

                if (token.kind == Lex::TK_THE_REST) {

                    if (hasFillVar) {
                        Logger::log(logErr, "Fill the rest symbol can be used only once per initialization!", span);
                        return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                    }

                    typeInit->fillVar = var;
                    hasFillVar = 1;

                } else if (token.kind == Lex::TK_IDENTIFIER) {

                    var->name.buff = tokenVal.str->buff;
                    var->name.len = tokenVal.str->len;
                    DArray::push(&ctx->nodeStack, var);

                } else {

                    // ERROR
                    Logger::log(logErr, "Attribute name expected!", span);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);

                }

                token = Lex::nextToken(&lspan, NULL);
                if (token.kind != Lex::TK_STATEMENT_BEGIN) {
                    Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan, Lex::toStr(Lex::TK_STATEMENT_BEGIN));
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                token = parseExpression(ctx, &lspan, var, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_SCOPE_END });
                if (token.encoded < 0) return token;

                if (token.kind == Lex::TK_SCOPE_END) break;
                token = Lex::nextToken(&lspan, &tokenVal);

            }

        } else {

            lspan.end = span->start;
            while (1) {

                Variable* var;
                token = parseExpression(ctx, &lspan, &var, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_SCOPE_END });
                if (token.encoded < 0) return token;

                DArray::push(&ctx->nodeStack, var);

                if (token.kind == Lex::TK_SCOPE_END) break;

            }

        }
        typeInit->attributes = (Variable**) commitStack(&ctx->nodeStack, smark, &typeInit->attributeCount);

        typeInit->idxs = (int*) nalloc(nalc, AT_BYTE, typeInit->attributeCount * sizeof(int));
        if (!typeInit->idxs) {
            Logger::log(logErr, Err::str(Err::MALLOC));
            return Lex::toToken(Err::MALLOC);
        }

        *outTypeInit = typeInit;

        span->start = lspan.super;
        span->end = lspan.end;

        return token;

    }

    // either alloc or expression
    // alloc [DtypeName, omitted if mainDtype >= 0] ['[' Expression defining length ']'] [:] [DtypeInit]
    Lex::Token parseRValue(ParseContext* ctx, Span* const span, Variable* outVar, const End endToken) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        Type::Kind mainDtype = outVar->value.typeKind;
        if (mainDtype == Type::DT_POINTER) {
            mainDtype = outVar->value.ptr->pointsToKind;
        } else if (mainDtype == Type::DT_ARRAY) {
            mainDtype = outVar->value.arr->base.pointsToKind;
        }

        Pos startPos = span->start;

        token = Lex::nextToken(span, NULL);
        if (Lex::isKeyword(token, KW_ALLOC)) {

            Pos startPos = span->end;

            Pointer* lastPtr = NULL;

            Alloc* alloc = Ast::Node::makeAlloc();

            VariableDefinition* varDef = alloc->def;
            varDef->base.scope = ctx->currentScope;
            varDef->base.span = getSpanStamp(span);

            Variable* var = alloc->def->var;

            token = Lex::nextToken(span, &tokenVal);
            if (token.kind == Lex::TK_IDENTIFIER) {

                var->value.typeKind = Type::DT_CUSTOM;
                var->value.any = NULL;

                varDef->dtype = Ast::Node::makeQualifiedName();
                varDef->dtype->buff = tokenVal.str->buff;
                varDef->dtype->len = tokenVal.str->len;

            } else if (token.kind == Lex::TK_KEYWORD) {

                if (!Lex::isDtype(token)) {
                    Logger::log(logErr, "TODO : data type expected, no place for generic keyword!");
                    return Lex::toToken(Err::INVALID_DATA_TYPE);
                }

                var->value.typeKind = Lex::toDtype((Keyword) token.detail);
                var->value.any = Type::basicTypes + Lex::toDtype((Keyword) token.detail);

            } else if (mainDtype <= 0) {

                Logger::log(logErr, "TODO : error parseRValue alloc requires dtype name! Can be omitted only in definition!");
                return Lex::toToken(Err::INVALID_DATA_TYPE);

            } else {

                var->value.typeKind = mainDtype;
                var->value.any = mainDtype == Type::DT_CUSTOM ? NULL : Type::basicTypes + mainDtype;

                if (outVar->def) {
                    varDef->dtype = Ast::Node::makeQualifiedName();
                    varDef->dtype = outVar->def->dtype;
                }

                span->end = startPos;

            }

            token = parseDataTypeDecorators(ctx, span, var, ALLOW_QUALIFIER, &tokenVal, &lastPtr);
            varDef->lastPtr = lastPtr;

            if (token.kind == Lex::TK_STATEMENT_BEGIN) {
                token = parseExpression(ctx, span, var, INVALID_POS, endToken);
                if (token.encoded < 0) return token;
            } else if (token.kind != Lex::TK_STATEMENT_END) {
                Logger::log(logErr, "TODO error: parseRValue unexpected symbol!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            outVar->expression = (Expression*) alloc;
            DArray::push(&ctx->ast->reg->allocations, &outVar);
            // initializations.push_back(varDef);

            // ...
            // DArray::push(&Reg.customDataTypesReferences.base, &varDef);

            outVar->base.flags |= IS_ALLOCATION;

        } else {

            outVar->base.scope = ctx->currentScope;
            token = parseExpression(ctx, span, outVar, startPos, endToken);
            if (token.encoded < 0) return token;

        }

        return token;

    }


    // pointers can ocur only before arrays
    // for now only one array
    Lex::Token parseDataTypeDecorators(ParseContext* ctx, Span* const span, Variable* var, Flags flags, Lex::TokenValue* outLastValue, Pointer** outLastPointer) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        int wasArray = 0;

        Pointer* mainPtr = NULL;
        while (1) {

            token = Lex::nextToken(span, &tokenVal);
            if (token.kind == Lex::TK_POINTER) {

                if (wasArray) {
                    Logger::log(logErr, "Pointer can't be used after array declaration!", span);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                Pointer* ptr = Ast::Node::makePointer();
                if (!mainPtr) *outLastPointer = ptr;
                else mainPtr->parentPointer = ptr;

                ptr->parentPointer = mainPtr;
                ptr->pointsTo = var->value.any;
                ptr->pointsToKind = var->value.typeKind;

                var->value.typeKind = Type::DT_POINTER;
                var->value.ptr = ptr;

                mainPtr = ptr;

            } else if (token.kind == Lex::TK_ARRAY_BEGIN) {
                // either const / embed or expression

                Pos arrStart = span->end;

                if (wasArray) {
                    Logger::log(logErr, "Multidimensional arrays not allowed!", span);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                wasArray = 1;

                Array* arr = Ast::Node::makeArray();
                if (!mainPtr) *outLastPointer = (Pointer*) arr;
                else mainPtr->parentPointer = (Pointer*) arr;

                arr->base.pointsTo = var->value.any;
                arr->base.pointsToKind = var->value.typeKind;

                Variable* lenVar = Ast::Node::makeVariable();
                lenVar->base.span = getSpanStamp(span);
                lenVar->base.scope = ctx->currentScope;
                lenVar->name.len = 0;

                token = Lex::nextToken(span, &tokenVal);
                if (token.kind == Lex::TK_KEYWORD) {

                    const int keyword = token.detail;

                    token = Lex::nextToken(span, NULL);
                    if (token.kind != Lex::TK_ARRAY_END) {
                        Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
                        return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                    }

                    if (keyword == KW_CONST) {
                        arr->flags = IS_CONST;
                    } else if (keyword == KW_AUTON) {
                        arr->flags = IS_ARRAY_LIST;
                    } else if (keyword == KW_MUTON) {
                        arr->flags = IS_DYNAMIC;
                    } else {
                        arr->flags = 0;
                    }

                    arr->length = NULL; // var->allocSize = NULL;

                } else {

                    token = parseExpression(ctx, span, lenVar, arrStart, End { Lex::TK_ARRAY_END }, EMPTY_EXPRESSION_ALLOWED);
                    if (token.encoded < 0 && token.encoded != Err::UNEXPECTED_END_OF_EXPRESSION) {
                        return token;
                    }

                    arr->flags = IS_CMP_TIME;
                    arr->length = lenVar; // var->allocSize = lenVar;

                }

                // var->flags = varDef->flags ^ IS_ARRAY;
                var->value.typeKind = Type::DT_ARRAY;
                var->value.arr = arr;
                // var->dtype = (void*) arr;
                var->base.flags = 0;

                if (flags & INCLUDE_TO_TREE) {
                    DArray::push(&ctx->ast->reg->arrays, &var);
                }

            } else {

                break;

            }

        }

        outLastValue->any = tokenVal.any;
        return token;

    }





    Lex::Token parseKeywordStatement(ParseContext* ctx, Span* const span, const Keyword keyword, Flags flags) {

        FullToken prevToken = { Lex::TK_KEYWORD, keyword };

        switch (keyword) {

            case KW_INT:
            case KW_I8:
            case KW_I16:
            case KW_I32:
            case KW_I64:
            case KW_U8:
            case KW_U16:
            case KW_U32:
            case KW_U64:
            case KW_F32:
            case KW_F64:
                return parseVariableDefinition(ctx, span, prevToken, End { Lex::TK_STATEMENT_END }, flags, NULL);
            case KW_EMBED:
                flags = flags | IS_CMP_TIME;
            case KW_CONST:
                flags = flags | IS_CONST;
                return parseVariableDefinition(ctx, span, prevToken, End { Lex::TK_STATEMENT_END }, flags, NULL);
            case KW_FCN:
                return parseFunction(ctx, span, flags);
            case KW_IF:
                return parseIfStatement(ctx, span);
            case KW_WHEN:
                return parseSwitchStatement(ctx, span);
            case KW_FOR:
                return parseForLoop(ctx, span);
            case KW_WHILE:
                return parseWhileLoop(ctx, span);
            case KW_GOTO:
                return parseGotoStatement(ctx, span);
            case KW_ENUM:
                return parseEnumDefinition(ctx, span);
            case KW_DEF:
                return parseTypeDefinition(ctx, span);
            case KW_RETURN:
                return parseReturnStatement(ctx, span);
            case KW_CONTINUE:
                return parseContinueStatement(ctx, span);
            case KW_BREAK:
                return parseBreakStatement(ctx, span);
            case KW_LOOP:
                return parseForEachLoop(ctx, span);
            case KW_NAMESPACE:
                return parseNamespace(ctx, span);
            case KW_ALLOC:
                return parseAllocStatement(ctx, span);
            case KW_FREE:
                return parseFreeStatement(ctx, span);
            case KW_IMPORT:
                return parseImport(ctx, span);
            case KW_ERROR:
                return parseError(ctx, span);
            default:
                Logger::log(logErr, "Unsupported keyword processing code encountered!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

    Lex::Token parseFunction(ParseContext* ctx, Span* const span, uint64_t param) {

        SpanEx lspan = markSpanStart(span);
        StackMark smark;

        Lex::TokenValue tokenVal;
        Lex::Token token;

        // first test if it could bee fcn pointer
        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind == Lex::TK_PARENTHESIS_BEGIN) {

            lspan.end.idx = lspan.start.idx - 1;
            token = { .kind = Lex::TK_KEYWORD, .detail = Lex::TD_KW_FCN };

            VariableDefinition* def;
            token = parseVariableDefinition(ctx, &lspan, { token, tokenVal }, End { Lex::TK_STATEMENT_END }, param, &def);
            if (token.encoded < 0) return token;

            // pushDefLike(scope->defSearch, def->var);
            DArray::push(&ctx->defStack, &def->var);
            setDefinitionIdx(ctx, (SyntaxNode*) def->var);

            DArray::push(&ctx->nodeStack, def);
            DArray::push(&ctx->ast->reg->variableDefinitions, &def);

            span->end = lspan.end;
            return token;

        }

        Function* fcn;

        Scope* newScope = Ast::Node::makeScope();
        // newScope->fcn = currentFunction;
        newScope->base.scope = ctx->currentScope;
        // setParentIdx(newScope);

        int foreignLang = 0;
        if (token.kind == Lex::TK_ARRAY_BEGIN) {

            foreignLang = 1;
            fcn = (Function*) Ast::Node::makeForeignFunction();

            Span* tagLoc = getSpanStamp(&lspan);

            String tag;
            token = parseLanguageTag(ctx, &lspan, &tag);
            if (token.encoded < 0) return token;

            ((ForeignFunction*) fcn)->code.tagStr = { tag.buff, tag.len };
            ((ForeignFunction*) fcn)->code.tagLoc = tagLoc;

            fcn->name.id = varId;
            varId++;

            token = nextToken(&lspan, &tokenVal);

        } else {

            fcn = Ast::Node::makeFunction();

        }

        // fcn->inArgsCnt = 0;
        fcn->base.scope = ctx->currentScope;
        fcn->errorSetName = NULL;
        fcn->prototype.outArg = Ast::Node::makeVariableDefinition();

        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(logErr, "Function name expected!", &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        fcn->name.buff = tokenVal.str->buff;
        fcn->name.len = tokenVal.str->len;

        fcn->name.id = varId;
        varId++;

        token = Lex::nextToken(&lspan);
        if (token.kind != Lex::TK_PARENTHESIS_BEGIN) {

            if (token.kind == Lex::TK_SCOPE_BEGIN) goto fcnParseScope;

            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        // parse input
        smark = markStack(&ctx->nodeStack);
        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_PARENTHESIS_END) break;

            VariableDefinition* varDef;
            token = parseVariableDefinition(ctx, &lspan, { token, tokenVal }, End { Lex::TK_PARENTHESIS_END, Lex::TK_LIST_SEPARATOR }, param | INCLUDE_TO_SCOPE, &varDef);
            if (token.encoded < 0) return token;

            // :)
            varDef->var->base.definitionIdx = -1;
            varDef->var->base.definitionIdx = -1;

            DArray::push(&ctx->nodeStack, &varDef);
            DArray::push(&ctx->ast->reg->variableDefinitions, &varDef);

            if (token.kind == Lex::TK_PARENTHESIS_END) break;

        }
        fcn->prototype.inArgs = (VariableDefinition**) commitStack(&ctx->nodeStack, smark, &fcn->prototype.inArgCount);

        // [using 'Error Set']
        token = Lex::nextToken(&lspan, &tokenVal);
        if (Lex::isKeyword(token, KW_USING)) {

            fcn->errorSetName = Ast::Node::makeQualifiedName();
            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.encoded < 0) return token;

            Using* tmp = Ast::Node::makeUsing();
            tmp->var = (SyntaxNode*) fcn;

            token = Lex::nextToken(&lspan, &tokenVal);

        }

        // ->
        if (token.kind != Lex::TK_ARROW) {

            if (token.kind == Lex::TK_SCOPE_BEGIN) goto fcnParseScope;

            // LOOK AT : maybe better name for this situation, something like "unexpected char sequence"
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan);
            Logger::log(logHnt, "'->' expected\n");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        // parse output
        token = Lex::nextToken(&lspan, &tokenVal);
        token = parseDataType(ctx, &lspan, { token, tokenVal }, NULL_FLAG, fcn->prototype.outArg, &tokenVal);
        if (token.encoded < 0) return token;

        // scope
        if (token.kind != Lex::TK_SCOPE_BEGIN && token.kind != Lex::TK_STATEMENT_BEGIN) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        fcnParseScope:

        if (foreignLang) {

            ForeignFunction* ffcn = (ForeignFunction*) fcn;

            ffcn->code.codeStr = (char*) (lspan.str + lspan.start.idx);
            ffcn->code.codeStr.len = Lex::findBlockEnd(&lspan, Lex::SCOPE_BEGIN, Lex::SCOPE_END);
            if (ffcn->code.codeStr.len < 0) {
                return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
            }

            ffcn->fcn.internalIdx = -3443431;
            DArray::push(&ctx->ast->reg->foreignFunctions, &ffcn);

            ffcn->fcn.base.span = finalizeSpan(&lspan, span);
            return token;

        }

        fcn->internalIdx = -1;

        ctx->currentFunction = fcn;

        const ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
        Scope* currentScope = ctx->currentScope;
        ctx->currentScope = newScope;
        token = parseScope(ctx, &lspan, SC_COMMON, scopeEnd);
        ctx->currentScope = currentScope;
        if (token.kind < 0) return token;

        ctx->currentFunction = NULL;

        fcn->bodyScope = newScope;

        DArray::push(&ctx->nodeStack, fcn);
        DArray::push(&ctx->ast->reg->fcns, &fcn);

        fcn->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseIfStatement(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;

        Scope* currentScope = ctx->currentScope;

        Scope* newScope = Ast::Node::makeScope();
        newScope->base.scope = ctx->currentScope;
        setDefinitionIdx(ctx, &newScope->base);

        Variable* newOperand;
        token = parseExpression(ctx, &lspan, &newOperand, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
        if (token.encoded < 0) return token;

        const ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);

        ctx->currentScope = newScope;
        token = parseScope(ctx, &lspan, SC_COMMON, scopeEnd);
        ctx->currentScope = currentScope;
        if (token.encoded < 0) return token;

        Branch* branch = Ast::Node::makeBranch();
        branch->base.scope = currentScope;

        StackMark smark = markStack(&ctx->nodeStack);

        DArray::push(&ctx->nodeStack, newScope);
        DArray::push(&ctx->nodeStack, newOperand);
        DArray::push(&ctx->ast->reg->branchExpressions, &newOperand);

        bool hasElse = false;
        while (1) {

            token = Lex::tryKeyword(&lspan, KW_ELSE);
            if (token.kind != Lex::TK_KEYWORD) break;

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_BEGIN || token.kind == Lex::TK_STATEMENT_BEGIN) {
                // else case

                hasElse = true;

                Scope* newScope = Ast::Node::makeScope();
                newScope->base.scope = currentScope;
                setDefinitionIdx(ctx, &newScope->base);

                ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);

                ctx->currentScope = newScope;
                token = parseScope(ctx, &lspan, SC_COMMON, scopeEnd);
                ctx->currentScope = currentScope;
                if (token.encoded < 0) return token;

                DArray::push(&ctx->nodeStack, newScope);

                break;

            }

            // else if case

            if (!Lex::isKeyword(token, KW_IF)) {
                Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan, "if, ':' or '{'");
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            Variable* newOperand;
            token = parseExpression(ctx, &lspan, &newOperand, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
            if (token.encoded < 0) return token;

            // TODO
            newOperand->base.span = getSpanStamp(&lspan);

            Scope *newScope = Ast::Node::makeScope();
            newScope->base.scope = ctx->currentScope;
            setDefinitionIdx(ctx, &newScope->base);

            ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
            ctx->currentScope = newScope;
            token = parseScope(ctx, &lspan, SC_COMMON, scopeEnd);
            ctx->currentScope = currentScope;
            if (token.encoded < 0) return token;

            DArray::push(&ctx->nodeStack, &newScope);
            DArray::push(&ctx->nodeStack, &newOperand);
            DArray::push(&ctx->ast->reg->branchExpressions, &newOperand);

        }

        // we need to manually resolve 'nodeStack'
        const int count = (ctx->nodeStack.size - smark) / 2;
        SyntaxNode** buffer = ((SyntaxNode**) ctx->nodeStack.buffer) + smark;

        branch->scopes = (Scope**) alloc(alc, sizeof(SyntaxNode**) * (count + hasElse));
        branch->expressions = (Variable**) alloc(alc, sizeof(SyntaxNode**) * count);
        branch->scopeCount = count + hasElse;
        branch->expressionCount = count;
        for (int i = 0; i < count; i++) {
            branch->scopes[2 * i] = (Scope*) buffer[2 * i];
            branch->expressions[2 * i + 1] = (Variable*) buffer[2 * i + 1];
        }

        if (hasElse) {
            branch->scopes[2 * count] = (Scope*) buffer[2 * count];
        }

        ctx->nodeStack.size = smark;

        DArray::push(&ctx->nodeStack, &branch);

        branch->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseSwitchStatement(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;

        SwitchCase* switchCase = Ast::Node::makeSwitchCase();
        switchCase->base.scope = ctx->currentScope;
        switchCase->elseCase = NULL;

        Variable* var;
        token = parseExpression(ctx, &lspan, &var, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
        if (token.encoded < 0) return token;

        switchCase->switchExp = var;

        int asStatement = token.kind == Lex::TK_STATEMENT_BEGIN;
        int elseCase = 0;
        int atLeastOneCase = 0;
        StackMark smark = markStack(&ctx->nodeStack);
        while (1) {

            if (!asStatement) {
                token = Lex::nextToken(&lspan);
                if (token.kind == Lex::TK_SCOPE_END) break;
            } else {
                token = Lex::tryKeyword(&lspan, KW_CASE);
                if (token.kind != Lex::TK_KEYWORD) {
                    token = Lex::tryKeyword(&lspan, KW_ELSE);
                    if (token.kind != Lex::TK_KEYWORD) break;
                }
            }

            elseCase = token.detail == Lex::TD_KW_ELSE;
            atLeastOneCase = 1;

            Variable* cmpExp;
            if (!elseCase) {
                token = parseExpression(ctx, &lspan, &cmpExp, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
                if (token.encoded < 0) return token;
            } else {
                if (!atLeastOneCase) {
                    Logger::log(logErr, "Else case can't be the first case!", &lspan);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }
                token = Lex::nextToken(&lspan);
            }

            Scope* sc = Ast::Node::makeScope();
            sc->base.scope = ctx->currentScope;
            // setParentIdx(sc);

            if (token.kind == Lex::TK_STATEMENT_BEGIN) {
                token = parseScope(ctx, &lspan, SC_COMMON, SE_STATEMENT);
            } else if (token.kind == Lex::TK_SCOPE_BEGIN) {
                token = parseScope(ctx, &lspan, SC_COMMON, SE_DEFAULT);
            } else {
                Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            if (elseCase) {
                switchCase->elseCase = sc;
                break;
            }

            DArray::push(&ctx->nodeStack, &sc);
            DArray::push(&ctx->nodeStack, &cmpExp);

        }

        // we need to manually resolve 'nodeStack'
        const int count = (ctx->nodeStack.size - smark) / 2;
        SyntaxNode** buffer = ((SyntaxNode**) ctx->nodeStack.buffer) + smark;

        switchCase->cases = (Scope**) alloc(alc, sizeof(SyntaxNode**) * count);
        switchCase->casesExp = (Variable**) alloc(alc, sizeof(SyntaxNode**) * count);
        switchCase->caseCount = count;
        switchCase->caseExpCount = count;
        for (int i = 0; i < count; i++) {
            switchCase->cases[2 * i] = (Scope*) buffer[2 * i];
            switchCase->casesExp[2 * i + 1] = (Variable*) buffer[2 * i + 1];
        }

        ctx->nodeStack.size = smark;

        DArray::push(&ctx->ast->reg->switchCases, &switchCase);
        DArray::push(&ctx->nodeStack, &switchCase);

        switchCase->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    // TODO: TODO
    Lex::Token parseForLoop(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        // TODO : something about empty expressions, maybe make them null or something...
        Lex::Token token;
        Lex::TokenValue tokenVal;

        ForLoop* loop = Ast::Node::makeForLoop();

        Scope* currentScope = ctx->currentScope;
        Scope* outerScope = Ast::Node::makeScope();
        outerScope->base.scope = ctx->currentScope;
        ctx->currentScope = outerScope;
        // setParentIdx(outerScope);

        Variable* initEx = Ast::Node::makeVariable();
        initEx->base.scope = outerScope;

        // can be either variable initialization or expression
        Pos startPos = lspan.end;
        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind == Lex::TK_KEYWORD && Lex::isDtype(token)) {
            token = parseVariableDefinition(ctx, &lspan, { token, tokenVal }, End { Lex::TK_STATEMENT_END }, INCLUDE_TO_SCOPE, NULL);
        } else {
            token = parseExpression(ctx, &lspan, initEx, startPos, End { Lex::TK_STATEMENT_END }, EMPTY_EXPRESSION_ALLOWED);
        }
        if (token.encoded < 0) return token;

        Scope* bodyScope = Ast::Node::makeScope();
        bodyScope->base.scope = outerScope;
        // setParentIdx(bodyScope);

        Variable* conditionEx = Ast::Node::makeVariable();
        conditionEx->base.scope = outerScope;

        token = parseExpression(ctx, &lspan, conditionEx, INVALID_POS, End { Lex::TK_STATEMENT_END }, EMPTY_EXPRESSION_ALLOWED);
        if (token.kind < 0) return token;

        // can be assignment
        Variable* actionEx;
        ctx->currentScope = outerScope;
        token = parseExpression(ctx, &lspan, &actionEx, INVALID_POS, End { Lex::TK_SCOPE_BEGIN, Lex::TK_STATEMENT_BEGIN }, EMPTY_EXPRESSION_ALLOWED);
        ctx->currentScope = currentScope;
        if (token.encoded < 0) return token;

        SyntaxNode* tmpLoop = ctx->currentLoop;
        ctx->currentLoop = (SyntaxNode*) loop;

        if (token.kind == Lex::TK_SCOPE_BEGIN || token.kind == Lex::TK_STATEMENT_BEGIN) {
            ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
            ctx->currentScope = bodyScope;
            token = parseScope(ctx, &lspan, SC_COMMON, scopeEnd);
            ctx->currentScope = currentScope;
            if (token.kind < 0) return token;
        } else {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        ctx->currentLoop = tmpLoop;

        loop->base.scope = ctx->currentScope;
        loop->bodyScope = bodyScope;
        loop->initEx = initEx;
        loop->conditionEx = conditionEx;
        loop->actionEx = actionEx;

        //DArray::push(&outerScope->children, &loop);
        //DArray::push(&scope->children, &outerScope);

        if (conditionEx->expression) {
            DArray::push(&ctx->ast->reg->branchExpressions, &conditionEx);
        } else {
            /* TODO: may lead to something...
            loop->conditionEx = NULL;
            freeSpanStamp(conditionEx->base.span);
            delete conditionEx;
            */
        }

        ctx->currentScope = currentScope;
        loop->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseWhileLoop(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        WhileLoop* loop = Ast::Node::makeWhileLoop();

        Scope* currentScope = ctx->currentScope;
        Scope* newScope = Ast::Node::makeScope();
        newScope->base.scope = ctx->currentScope;
        // setParentIdx(newScope);

        Variable* newOperand;
        ctx->currentScope = newScope;
        token = parseExpression(ctx, &lspan, &newOperand, INVALID_POS, End { Lex::TK_SCOPE_BEGIN, Lex::TK_STATEMENT_BEGIN });
        ctx->currentScope = currentScope;
        if (token.kind < 0) return token;

        SyntaxNode* tmpLoop = ctx->currentLoop;
        ctx->currentLoop = (SyntaxNode*) loop;

        ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);

        ctx->currentScope = newScope;
        token = parseScope(ctx, &lspan, SC_COMMON, scopeEnd);
        ctx->currentScope = currentScope;
        if (token.kind < 0) return token;

        ctx->currentLoop = tmpLoop;

        loop->base.scope = ctx->currentScope;
        loop->bodyScope = newScope;
        loop->expression = newOperand;

        DArray::push(&ctx->nodeStack, &loop);

        loop->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseForEachLoop(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(span, &tokenVal);

        Loop* loop = Ast::Node::makeLoop();

        Scope* currentScope = ctx->currentScope;
        Scope* outerScope = Ast::Node::makeScope();
        outerScope->base.scope = ctx->currentScope;
        // setParentIdx(outerScope);

        loop->base.scope = outerScope;

        Variable* newVar;
        token = parseExpression(ctx, &lspan, &newVar, INVALID_POS, End { Lex::TK_STATEMENT_END }, USE_KEYWORD_AS_END);
        if (token.kind < 0) return token;

        // TODO
        newVar->base.span = getSpanStamp(span);

        if (token.kind != Lex::TK_KEYWORD && token.detail != KW_USING) {
            Logger::log(logErr, Err::str(Err::INVALID_VARIABLE_NAME), span, "Variable name is matching key word name!");
            return Lex::toToken(Err::INVALID_VARIABLE_NAME);
        }

        loop->array = newVar;

        // var or var def (only basic dtypes)
        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind == Lex::TK_KEYWORD) {
            // var def

            if (!Lex::isInt((Keyword) token.detail)) {
                Logger::log(logErr, "Only integral datatypes allowed!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            loop->idx = NULL;
            loop->idxDef = Ast::Node::makeVariableDefinition();
            loop->idxDef->var->value.typeKind = Lex::toDtype(token);

            ctx->currentScope = outerScope;
            token = parseVariableDefinition(ctx, &lspan, { token, tokenVal }, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN }, INCLUDE_TO_SCOPE | USE_KEYWORD_AS_END, &(loop->idxDef));
            ctx->currentScope = currentScope;
            if (token.encoded < 0) return token;

            if (loop->idxDef->var->expression == NULL) {
                loop->idxDef->var->value.hasValue = 1;
                loop->idxDef->var->value.i64 = 0;
                loop->idxDef->var->value.typeKind = Type::DT_INT;
            }

        } else {
            // var

            loop->idxDef = NULL;
            loop->idx = Ast::Node::makeVariable();
            loop->idx->base.scope = outerScope;
            loop->idx->name.buff = tokenVal.str->buff;
            loop->idx->name.len = tokenVal.str->len;

            token = Lex::nextToken(&lspan);

        }

        // maybe 'to <var>'
        if (Lex::isKeyword(token, KW_TO)) {
            token = parseExpression(ctx, &lspan, loop->to, INVALID_POS, End { Lex::TK_SCOPE_BEGIN, Lex::TK_STATEMENT_BEGIN });
            if (token.encoded < 0) return token;
            loop->to->base.scope = outerScope;
        }

        loop->bodyScope = Ast::Node::makeScope();
        loop->bodyScope->base.scope = loop->base.scope;
        // setParentIdx(loop->bodyScope);

        SyntaxNode* tmpLoop = ctx->currentLoop;
        ctx->currentLoop = (SyntaxNode*) loop;

        ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);

        ctx->currentScope = loop->bodyScope;
        token = parseScope(ctx, &lspan, SC_COMMON, scopeEnd);
        ctx->currentScope = currentScope;
        if (token.encoded < 0) return token;

        ctx->currentLoop = tmpLoop;

        DArray::push(&ctx->ast->reg->loops, &loop);
        DArray::push(&ctx->nodeStack, &loop);

        loop->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseGotoStatement(ParseContext* ctx, Span* const span) {

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(span, &tokenVal);

        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(logErr, "Identifier expected!", span);
            return token.encoded < 0 ? token : Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        GotoStatement* gt = Ast::Node::makeGotoStatement();
        gt->span = getSpanStamp(span);
        gt->base.scope = ctx->currentScope;
        gt->name.buff = tokenVal.str->buff;
        gt->name.len = tokenVal.str->len;
        gt->label = NULL;

        DArray::push(&ctx->ast->reg->gotos, &gt);
        DArray::push(&ctx->nodeStack, &gt);

        return token;

    }

    Lex::Token parseContinueStatement(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_STATEMENT_END) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (!ctx->currentLoop) {
            Logger::log(logErr, "Continue statement used outside of the loop!");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        ContinueStatement* stmt = Ast::Node::makeContinueStatement();
        stmt->base.scope = ctx->currentScope;

        DArray::push(&ctx->nodeStack, &stmt);

        stmt->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseBreakStatement(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_STATEMENT_END) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (!ctx->currentLoop) {
            Logger::log(logErr, "Break statement used outside of the loop!");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        BreakStatement* stmt = Ast::Node::makeBreakStatement();
        stmt->base.scope = ctx->currentScope;

        DArray::push(&ctx->nodeStack, &stmt);

        stmt->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseNamespace(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Scope* currentScope = ctx->currentScope;

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(&lspan, &tokenVal);

        Namespace* nsc = Ast::Node::makeNamespace();
        nsc->scope.base.scope = ctx->currentScope;
        nsc->name.buff = tokenVal.str->buff;
        nsc->name.len = tokenVal.str->len;
        // setParentIdx(nsc);

        token = Lex::nextToken(&lspan);
        if (token.kind != Lex::TK_SCOPE_BEGIN && token.kind != Lex::TK_STATEMENT_BEGIN) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);

        ctx->currentScope = (Scope*) nsc;
        token = parseScope(ctx, &lspan, SC_COMMON, scopeEnd);
        ctx->currentScope = currentScope;
        if (token.encoded < 0) return token;

        DArray::push(&ctx->nodeStack, &nsc);

        nsc->scope.base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseAllocStatement(ParseContext* ctx, Span* const span) {

        return Lex::Token { Lex::TK_END };

    }

    Lex::Token parseFreeStatement(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        Free* freeEx = Ast::Node::makeFree();
        freeEx->var->base.scope = ctx->currentScope;

        token = parseExpression(ctx, &lspan, freeEx->var, INVALID_POS, End { Lex::TK_STATEMENT_END });
        if (token.encoded < 0) return token;

        Variable* wrapper = Ast::Node::makeVariable();
        wrapper->base.scope = ctx->currentScope;
        wrapper->expression = (Expression*) freeEx;
        wrapper->base.span = getSpanStamp(&lspan);

        Statement* st = Ast::Node::makeStatement();
        st->base.scope = ctx->currentScope;
        st->operand = wrapper;

        DArray::push(&ctx->nodeStack, &st);

        token = Lex::nextToken(&lspan);
        return token;

    }

    Lex::Token parseReturnStatement(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        ReturnStatement* ret = Ast::Node::makeReturnStatement();
        ret->fcn = ctx->currentFunction;
        ret->base.scope = ctx->currentScope;

        ret->var = NULL;
        ret->err = NULL;

        ret->idx = ctx->currentFunction->returnCount;

        DArray::push(&ctx->ast->reg->returnStatements, &ret);
        DArray::push(&ctx->nodeStack, &ret);

        Pos startPos = lspan.end;

        token = Lex::nextToken(&lspan);
        if (token.kind == Lex::TK_STATEMENT_END) {
            ret->base.span = finalizeSpan(&lspan, span);
            return token;
        }

        if (token.kind != Lex::TK_SKIP) {

            Variable* newVar;
            token = parseExpression(ctx, &lspan, &newVar, startPos, End { Lex::TK_LIST_SEPARATOR, Lex::TK_STATEMENT_END });
            if (token.encoded < 0) return token;

            // TODO
            newVar->base.span = getSpanStamp(&lspan);
            newVar->value.typeKind = Type::DT_I64;

            ret->var = newVar;

        } else {

            token = Lex::nextToken(&lspan);

        }

        if (token.kind == Lex::TK_STATEMENT_END) {
            ret->base.span = finalizeSpan(&lspan, span);
            return token;
        }

        if (token.kind != Lex::TK_LIST_SEPARATOR) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan, Lex::toStr(Lex::TK_LIST_SEPARATOR));
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        // error case
        Variable* newVar;
        token = parseExpression(ctx, &lspan, &newVar, INVALID_POS, End { Lex::TK_STATEMENT_END });
        if (token.kind < 0) return token;

        // TODO
        newVar->base.span = getSpanStamp(&lspan);
        newVar->value.typeKind = Type::DT_I64;

        ret->err = newVar;

        ret->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseEnumDefinition(ParseContext* ctx, Span* const span) {

        // enum <name> : <type> { .. }

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;

        Enumerator* enumerator = Ast::Node::makeEnumerator();
        enumerator->dtype = Type::DT_INT;

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(logErr, "Identifier expected!", &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        enumerator->name.buff = tokenVal.str->buff;
        enumerator->name.len = tokenVal.str->len;

        token = Lex::nextToken(&lspan);
        if (token.kind == Lex::TK_STATEMENT_BEGIN) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind != Lex::TK_KEYWORD) {
                Logger::log(logErr, "Data type expected!", &lspan);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            if (!Lex::isInt((Keyword) token.detail)) {
                Logger::log(logErr, Err::str(Err::UNKNOWN_DATA_TYPE), &lspan);
                return Lex::toToken(Err::UNKNOWN_DATA_TYPE);
            }

            enumerator->dtype = Lex::toDtype((Keyword) token.detail);

            token = Lex::nextToken(&lspan);

        }

        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(logErr, "'{' is expected!", &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        enumerator->base.span = getSpanStamp(&lspan);

        enumerator->name.id = defId;
        defId++;

        // assignId(enumerator);

        uint64_t lastValue = -1; //((uint64_t) 0) - ((uint64_t) 1);
        StackMark smark = markStack(&ctx->nodeStack);
        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            VariableDefinition* newVarDef = Ast::Node::makeVariableDefinition();
            Variable* newVar = Ast::Node::makeVariable();
            newVar->base.scope = ctx->currentScope;
            newVar->base.span = getSpanStamp(&lspan);
            newVar->value.typeKind = enumerator->dtype;

            newVarDef->var = newVar;
            newVarDef->var->value.hasValue = 0;
            newVarDef->var->value.typeKind = enumerator->dtype;
            newVarDef->base.span = getSpanStamp(&lspan);
            newVarDef->base.flags = IS_CMP_TIME;

            newVar->def = newVarDef;
            newVar->name.buff = tokenVal.str->buff;
            newVar->name.len = tokenVal.str->len;

            assignId(newVar);

            DArray::push(&ctx->nodeStack, &newVar);

            token = Lex::nextToken(&lspan);
            if (token.kind == Lex::TK_EQUAL) {

                token = parseExpression(ctx, &lspan, newVar, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_SCOPE_END }, 0);
                if (token.encoded < 0) return token;

                newVarDef->var->value.hasValue = true;

            }

            if (token.kind == Lex::TK_LIST_SEPARATOR) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;

            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan, "',' or '}'");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }
        enumerator->vars = (Variable**) commitStack(&ctx->nodeStack, smark, &enumerator->varCount);

        DArray::push(&ctx->ast->reg->enumerators, &enumerator);
        DArray::push(&ctx->nodeStack, &enumerator);

        enumerator->base.span = finalizeSpan(&lspan, span);

        return token;

    }

    Lex::Token parseTypeDefinition(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(&lspan, &tokenVal);

        Keyword keyword;
        if (token.kind == Lex::TK_KEYWORD) {

            if (token.detail == Lex::TD_KW_UNION || token.detail == Lex::TD_KW_STRUCT) {
                keyword = (Keyword) token.detail;
            } else {
                Logger::log(logErr, "Unexpected symbol!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            token = Lex::nextToken(&lspan);

        } else {

            keyword = KW_STRUCT;

        }

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(logErr, "Identifier expected!", &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        TypeDefinition* typeDef;
        if (keyword == KW_STRUCT) {
            typeDef = Ast::Node::makeTypeDefinition();
        } else {
            typeDef = (TypeDefinition*) Ast::Node::makeUnion();
        }

        typeDef->base.scope = ctx->currentScope;
        typeDef->name.buff = tokenVal.str->buff;
        typeDef->name.len = tokenVal.str->len;
        typeDef->base.definitionIdx = ctx->ast->reg->customDataTypes.size;
        typeDef->base.span = getSpanStamp(&lspan);

        typeDef->name.id = defId;
        defId++;

        token = Lex::nextToken(&lspan);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        StackMark mark = markStack(&ctx->nodeStack);
        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            VariableDefinition* def;
            token = parseVariableDefinition(ctx, &lspan, { token, tokenVal }, End { Lex::TK_SCOPE_END, Lex::TK_STATEMENT_END }, NULL_FLAG, &def);
            if (token.encoded < 0) return token;

            DArray::push(&ctx->nodeStack, &def->var);

            if (token.kind == Lex::TK_STATEMENT_END) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;

            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }
        typeDef->vars = (Variable**) commitStack(&ctx->nodeStack, mark, &typeDef->varCount);

        DArray::push(&ctx->ast->reg->customDataTypes, &typeDef);
        DArray::push(&ctx->nodeStack, &typeDef);

        typeDef->base.span = finalizeSpan(&lspan, span);

        return token;

    }

    Lex::Token parseErrorDeclaration(ParseContext* ctx, Span* const span) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        VariableDefinition* def = Ast::Node::makeVariableDefinition();
        def->var->name.buff = tokenVal.str->buff;
        def->var->name.len = tokenVal.str->len;
        def->var->value.typeKind = Type::DT_ERROR;
        def->var->base.span = getSpanStamp(span);
        def->base.scope = ctx->currentScope;
        setDefinitionIdx(ctx, (SyntaxNode*) def->var);

        DArray::push(&ctx->defStack, &def->var);
        assignId(def->var);

        token = Lex::nextToken(span, NULL);
        if (token.kind == Lex::TK_EQUAL) {

            token = parseExpression(ctx, span, def->var, INVALID_POS, End { Lex::TK_STATEMENT_END, Lex::TK_NONE });
            if (token.kind < 0) return token;

        } else if (token.kind != Lex::TK_STATEMENT_END) {

            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        DArray::push(&ctx->ast->reg->variableDefinitions, &def);
        DArray::push(&ctx->nodeStack, &def);

        return token;

    }

    Lex::Token parseErrorDefinition(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(&lspan, &tokenVal);

        ErrorSet* errorSet = Ast::Node::makeErrorSet();
        errorSet->base.scope = ctx->currentScope;
        errorSet->name.buff = tokenVal.str->buff;
        errorSet->name.len = tokenVal.str->len;
        errorSet->value = errId;
        errId++;

        errorSet->name.id = varId;
        varId++;

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), &lspan, Lex::toStr(Lex::TK_SCOPE_BEGIN));
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        StackMark smark = markStack(&ctx->nodeStack);
        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            Variable* var = Ast::Node::makeVariable();
            var->base.scope = ctx->currentScope;
            var->name.buff = tokenVal.str->buff;
            var->name.len = tokenVal.str->len;
            var->base.span = getSpanStamp(&lspan);
            var->value.hasValue = 0;
            var->value.typeKind = Type::DT_ERROR;
            var->value.u64 = errId;
            errId++;

            DArray::push(&ctx->nodeStack, &var);

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_STATEMENT_END) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;

            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }
        errorSet->vars = (Variable**) commitStack(&ctx->nodeStack, smark, &errorSet->varCount);

        DArray::push(&ctx->ast->reg->customErrors, &errorSet);
        DArray::push(&ctx->nodeStack, &errorSet);

        errorSet->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseError(ParseContext* ctx, Span* const span) {

        Lex::TokenValue tokenVal;
        Lex::Token token;

        token = Lex::peekNthToken(span, &tokenVal, 2);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {

            token = parseErrorDeclaration(ctx, span);
            if (token.encoded < 0) return token;

        }

        token = parseErrorDefinition(ctx, span);
        if (token.encoded < 0) return token;

        return token;

    }

    Lex::Token parseImport(ParseContext* ctx, Span* const span) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        ImportStatement* import = Ast::Node::makeImportStatement();
        // import->root = fileRootScope;
        import->base.scope = ctx->currentScope;

        ImportNode* importNode = initImportNode();
        importNode->import = import;
        importNode->parent = importCurrent;

        token = Lex::tryKeyword(&lspan, KW_FROM);
        if (token.kind != Lex::TK_KEYWORD) {
            token = Lex::nextFileName(&lspan, &tokenVal);
        }

        import->fname = *tokenVal.str;
        import->keyword = KW_VOID;

        pushImportChunk(importCurrent, importNode);

        token = Lex::nextToken(&lspan, &tokenVal);
        if (!Lex::isKeyword(token, KW_AS)) {
            Logger::log(logErr, "Keyword 'as' expected!", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_KEYWORD) {
            Logger::log(logErr, "Unsupported value is used! Use one of the following : 'namespace', 'fcn', 'scope'\n", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (token.detail != KW_NAMESPACE && token.detail != KW_SCOPE && token.detail != KW_FCN) {
            Logger::log(logErr, "Unsupported value is used! Use one of the following : 'namespace', 'fcn', 'scope'\n", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        import->keyword = (Keyword) token.detail;

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(logErr, "Identifier expected!", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_STATEMENT_END) {
            Logger::log(logErr, "End of statement expected!", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        //pushImportChunk(importCurrent, importNode);

        import->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseDirective(ParseContext* ctx, Span* const span, Lex::Directive directive, Flags param) {

        switch (directive) {

            case Lex::CD_TEST: {
                return Lex::toToken(Lex::TK_STATEMENT_END);
            }

            default: {
                Logger::log(logErr, "Unsupported language directive processing code encountered!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        }

        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

    Lex::Token parseList(ParseContext* ctx, Span* const span, Lex::TokenKind separator, Lex::TokenKind end) {

        Lex::Token token;
        int first = 1;

        while (1) {

            if (first) {
                token = Lex::tryToken(span, Lex::toToken(end));
                if (token.kind == end) return token;
            }

            Variable* newVar = NULL;
            token = parseExpression(ctx, span, &newVar, INVALID_POS, End{ separator, end }, first ? EMPTY_EXPRESSION_ALLOWED : 0);
            if (token.encoded < 0) return token;

            DArray::push(&ctx->nodeStack, &newVar);

            if (token.kind == end) {
                break;
            }

            if (token.kind != separator) {
                Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            first = 0;

        }

    }

    Lex::Token parseCatch(ParseContext* ctx, Span* const span, Variable* var) {

        Lex::Token token;
        Lex::TokenValue tokenVal;



        Variable* errVar = Ast::Node::makeVariable();
        errVar->base.scope = var->base.scope;
        errVar->value.typeKind = Type::DT_ERROR;

        Scope* newScope = Ast::Node::makeScope();
        newScope->base.scope = var->base.scope;
        // setParentIdx(newScope);

        Catch* cex = Ast::Node::makeCatch();
        cex->call = (FunctionCall*) var->expression;

        var->expression = (Expression*) cex;



        token = Lex::nextToken(span, &tokenVal);

        if (Lex::isKeyword(token, KW_RETURN)) {
            // basically just emulates following
            // .. .. catch err { return _, err; }

            ReturnStatement* ret = Ast::Node::makeReturnStatement();
            ret->err = errVar;
            ret->var = NULL;
            ret->base.scope = newScope;
            ret->fcn = ctx->currentFunction;

            DArray::push(&ctx->ast->reg->returnStatements, &ret);
            newScope->children = (SyntaxNode**) alloc(alc, sizeof(SyntaxNode*));
            newScope->children[0] = (SyntaxNode*) ret;

            token = Lex::nextToken(span, &tokenVal);
            if (token.kind != Lex::TK_STATEMENT_END) {
                Logger::log(logErr, "'catch return' expression has to be terminated with ';'!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            return token;
        }

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span, "error identifier");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        QualifiedName* errName = (QualifiedName*) (tokenVal.any);

        token = Lex::nextToken(span, &tokenVal);
        if (
            token.kind != Lex::TK_SCOPE_BEGIN &&
            token.kind != Lex::TK_STATEMENT_BEGIN &&
            !Lex::isKeyword(token, KW_RETURN)
        ) {
            // 'catch err' case

            errVar->name = *errName;

            DArray::push(&ctx->ast->reg->variables, &var);

            cex->err = errVar;
            cex->scope = NULL;

            return token;

        }



        // 'catch err { ... }' case
        VariableDefinition* errDef = Ast::Node::makeVariableDefinition();
        errDef->var = errVar;
        errDef->base.scope = newScope;
        errDef->var->base.scope = newScope;
        errDef->var->value.typeKind = Type::DT_ERROR;
        errDef->var->base.span = getSpanStamp(span);
        errDef->var->base.definitionIdx = -1;

        errDef->var->name = *errName;

        assignId(errDef->var);

        DArray::push(&ctx->ast->reg->variableDefinitions, &errDef);
        DArray::push(&ctx->nodeStack, &errDef->var);
        ctx->nodeStack.size -= sizeof(SyntaxNode*);

        DArray::push(&ctx->defStack, &errDef->var);

        cex->err = errDef->var;
        cex->scope = newScope;

        ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);

        Scope* currentScope = ctx->currentScope;
        ctx->currentScope = newScope;
        token = parseScope(ctx, span, SC_COMMON, scopeEnd);
        ctx->currentScope = currentScope;

        return token;

    }



    Lex::Token parseExpressionNode(ParseContext* ctx, Span* const span, Variable* var, OperatorEnum prevOp = OP_NONE) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        int consumeToken = 1;

        var->base.scope = ctx->currentScope;

        while (1) {

            token = Lex::nextToken(span, &tokenVal);

            OperatorEnum op = Lex::toUnaryOperator(token);
            if (op == OP_NONE) {
                break;
            }

            UnaryExpression* uex = Ast::Node::makeUnaryExpression();
            uex->base.opType = op;
            uex->operand = Ast::Node::makeVariable();
            uex->operand->base.scope = ctx->currentScope;

            var->expression = (Expression*) uex;
            var = uex->operand;

        }

        switch (token.kind) {

            case Lex::TK_IDENTIFIER: {

                token = Lex::nextToken(span, &tokenVal);
                if (token.kind == Lex::TK_PARENTHESIS_BEGIN) {

                    FunctionCall* call = Ast::Node::makeFunctionCall();
                    call->fptr = NULL;
                    call->fcn = NULL;

                    StackMark smark = markStack(&ctx->nodeStack);
                    token = parseList(ctx, span, Lex::TK_LIST_SEPARATOR, Lex::TK_PARENTHESIS_END);
                    if (token.encoded < 0) return token;
                    call->inArgs = (Variable**) commitStack(&ctx->nodeStack, smark, &call->inArgCount);

                    call->name = *((QualifiedName* )tokenVal.any);

                    var->value.hasValue = 0;
                    var->expression = (Expression*) call;

                    DArray::push(&ctx->ast->reg->fcnCalls, &var);

                } else {

                    var->name = *((QualifiedName*) tokenVal.any);
                    consumeToken = 0;

                    if (prevOp != OP_MEMBER_SELECTION) {
                        DArray::push(&ctx->ast->reg->variables, &var);
                    }

                }

                break;

            }

            case Lex::TK_NUMBER: {

                //var->name.buff = NULL;
                //var->name.len = 0;
                var->expression = NULL;
                var->value.hasValue = true;
                var->value.typeKind = Lex::toDtype(token);

                if (token.detail == Lex::TD_DT_U64) {
                    var->value.u64 = tokenVal.ival;
                } else if (token.detail == Lex::TD_DT_I64) {
                    var->value.i64 = tokenVal.ival;
                } else if (token.detail == Lex::TD_DT_F32) {
                    var->value.f32 = tokenVal.f32;
                } else if (token.detail == Lex::TD_DT_F64) {
                    var->value.f64 = tokenVal.f64;
                }

                break;

            }

            case Lex::TK_KEYWORD: {

                if (token.detail == Lex::TD_KW_TRUE) {

                    var->value.hasValue = 1;
                    var->value.typeKind = Type::DT_BOOL;
                    var->value.u64 = 1;

                } else if (token.detail == Lex::TD_KW_FALSE) {

                    var->value.hasValue = 1;
                    var->value.typeKind = Type::DT_BOOL;
                    var->value.u64 = 0;

                } else if (token.detail == Lex::TD_KW_NULL) {

                    var->value.hasValue = 1;
                    var->value.typeKind = Type::DT_POINTER;
                    var->value.u64 = 0;

                }

                break;

            }

            case Lex::TK_PARENTHESIS_BEGIN: {

                token = parseExpression(ctx, span, var, INVALID_POS, End { Lex::TK_PARENTHESIS_END, Lex::TK_NONE });
                if (token.kind < 0) return token;

                break;

            }

            case Lex::TK_STRING: {

                StringInitialization* init = (StringInitialization*) tokenVal.any; // Reg.Node.initStringInitialization();
                // init->rawPtr = tokenVal.str->buff;
                // init->rawPtrLen = tokenVal.str->len;

                var->expression = (Expression*) init;

                break;

            }

            case Lex::TK_CHAR: {

                var->value.typeKind = Type::DT_I64;
                var->value.i64 = tokenVal.ival;

                break;

            }

            case Lex::TK_ARRAY_BEGIN: {

                ArrayInitialization* init;
                token = parseArrayInitialization(ctx, span, &init);
                var->expression = (Expression*) init;

                break;

            }

            case Lex::TK_SCOPE_BEGIN: {

                TypeInitialization* init;
                token = parseTypeInitialization(ctx, span, &init);
                var->expression = (Expression*) init;
                var->base.span = getSpanStamp(span);

                break;

            }

            default: {

                return token;

            }

        }

        if (consumeToken) token = Lex::nextToken(span, &tokenVal);

        while (1) {

            OperatorEnum op = Lex::toPostfixOperator(token);
            if (op == OP_NONE) {
                break;
            }

            UnaryExpression* uex = Ast::Node::makeUnaryExpression();
            uex->base.opType = op;
            uex->operand = Ast::Node::makeVariable();
            uex->operand->value = var->value;
            uex->operand->expression = var->expression;

            var->expression = (Expression*) uex;

            token = Lex::nextToken(span, &tokenVal);

        }

        return token;

    }


    // the lower the rank, the higher precedence
    Lex::Token parseExpressionRecursive(ParseContext* ctx, Span* const span, Variable** var, BinaryExpression* prevBex, OperatorEnum prevOp) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        // INFO: as for now only one such operator exists, its hardcoded
        //       basically this should work just fine while there is no binary
        //       operator with same signature as required closure
        if (prevOp == OP_SUBSCRIPT) {

            token = parseExpressionRecursive(ctx, span, var, prevBex, OP_NONE);

            if (token.kind == Lex::TK_ARRAY_END) {

                token = nextToken(span, &tokenVal);

            } else if (token.kind == Lex::TK_SLICE) {

                Slice* slice = Ast::Node::makeSlice();

                slice->bidx = Ast::Node::makeVariable();
                slice->bidx->expression = (*var)->expression;

                token = parseExpressionRecursive(ctx, span, &(slice->eidx), NULL, OP_NONE);

                (*var)->expression = (Expression*) slice;

                if (token.kind != Lex::TK_ARRAY_END) {
                    tokenToErrorBuff(ctx, Lex::TK_ARRAY_END);
                    Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span, ctx->errBuff);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                token = nextToken(span, &tokenVal);

            } else {

                endTokenToErrorBuff(ctx, { Lex::TK_ARRAY_END, Lex::TK_SLICE });
                Logger::log(logErr, Err::str(Err::UNEXPECTED_SYMBOL), span, ctx->errBuff);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);

            }

        } else {
            token = parseExpressionNode(ctx, span, *var, prevOp);
        }

        if (token.encoded < 0) return token;

        if (Lex::isOperator(token)) {

            OperatorEnum op = Lex::toBinaryOperator(token);

            while (op != OP_NONE) {

                if (prevOp == OP_NONE || operators[prevOp].rank > operators[op].rank) {

                    BinaryExpression* bex = Ast::Node::makeBinaryExpression();
                    bex->left = *var;
                    bex->right = Ast::Node::makeVariable();
                    bex->right->base.scope = (*var)->base.scope;
                    bex->base.opType = op;

                    *var = Ast::Node::makeVariable();
                    (*var)->expression = (Expression*) bex;

                    if (prevBex) {
                        prevBex->right->expression = (Expression*) bex;
                        // prevOp = op;
                    }

                    token = parseExpressionRecursive(ctx, span, &bex->right, bex, op);
                    if (token.encoded < 0) return token;

                    op = Lex::toBinaryOperator(token);
                    if (op < 0) return token;

                } else {

                    prevBex->right = *var;
                    return Lex::toTokenAsBinaryOperator(op);

                }

            }

        } else {

            return token;

        }

        return token;

    }

    Lex::Token parseExpression(ParseContext* ctx, Span* const span, Variable** var, const Pos startPos, const End endToken, const Flags flags) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        if (isValidPos(startPos)) span->end = startPos;

        Variable* operand = Ast::Node::makeVariable();
        operand->base.scope = ctx->currentScope;
        token = parseExpressionRecursive(ctx, span, &operand, NULL, OP_NONE);
        if (token.encoded < 0) return token;

        *var = operand;

        if (Lex::isKeyword(token, KW_CATCH)) {

            if (operand->expression->type != EXT_FUNCTION_CALL) {
                Logger::log(logErr, "Yet only 'pure' function call expressions are allowed to be caught 🐱.", span);
                return Lex::toToken(Err::UNEXPECTED_END_OF_EXPRESSION);
            }

            // TODO : add flag somethign like ALLOW_CATCH_TO_DISRESPECT_ENDING
            token = parseCatch(ctx, span, operand);
            if (token.kind == Lex::TK_SCOPE_END) return token;

        }

        if (
            isEndToken(token, endToken) ||
            (flags & USE_KEYWORD_AS_END && token.kind == Lex::TK_KEYWORD) ||
            flags & ALLOW_UNEXPECTED_END
        ) {
            return token;
        }

        Logger::log(logErr, "Blablbalba", span);
        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

    // meh, but ok for now...
    Lex::Token parseExpression(ParseContext* ctx, Span* const span, Variable* var, const Pos startPos, const End endToken, const Flags flags) {

        Lex::Token token;

        Variable* tmpVar;
        token = parseExpression(ctx, span, &tmpVar, startPos, endToken, flags);
        if (token.encoded < 0) return token;

        UnaryExpression* uex = Ast::Node::makeUnaryExpression();
        uex->operand = tmpVar;
        uex->base.opType = OP_NONE;

        var->expression = (Expression*) uex;

        return token;

    }

}
