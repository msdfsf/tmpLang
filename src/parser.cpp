// Each 'parse' function should return the last token it encountered, or an error.
// Each 'parse' function processes the input stream starting with the first token
// already consumed.
//      (We read one or more tokens first, then decide what to do. In most cases,
//       a single token is enough to decide, so we simply skip it and pass along
//       extra information about it, instead of rollback. This keeps the stack
//       smaller and prevents processing the same token twice. In case of multiple
//       tokens, we just roll back after the first one.)
//
// Each 'parse' function takes the parent Span as the first argument and the Scope
// as the second argument.
//      - The parent span's end position should be updated to match the local span
//        when the function exits.
//      - The start position of a span that the function does not own must not be modified.
//      - If previous token information is needed, it should be passed as the third
//        argument. (The type for this is not fixed and should be chosen based on needs.)
//      - If information about ending tokens is needed, it should be passed as the
//        fourth argument. If previous-token info is not required, the third argument
//        is used instead.
//      - All other arguments can be case-specific and do not need to follow any rules.
//      - To simplify function calls and signatures, it is recommended to use a
//        'flag argument' parameter to carry any additional info via flags if possible.



#include "parser.h"
#include "data_types.h"
#include "globals.h"
#include "keywords.h"
#include "lexer.h"
#include "string.h"
#include "strlib.h"
#include "syntax.h"
#include "dynamic_arena.h"
#include "file_system.h"
#include "logger.h"
#include "error.h"



namespace Parser {

    Arena::Container errBuff;

    inline void errorBuffInit() {
        Arena::init(&errBuff, 128);
    }

    inline void errorBuffClear() {
        Arena::clear(&errBuff);
    }

    inline void errorBuffPush(const char* const str) {
        const size_t strSize = cstrlen(str);
        char* buff = (char*) Arena::push(&errBuff, strSize);
        memcpy(buff + 1, str, strSize);
    }

    inline void tokenToErrorBuff(Lex::TokenKind token) {

        const char* const str = Lex::toStr(token);
        const size_t strSize = cstrlen(str);

        char* buff = (char*) Arena::push(&errBuff, 2 + strSize);

        buff[0] = '\'';
        memcpy(buff + 1, str, strSize);
        buff[strSize + 1] = '\'';

    }

    // returns 1 if any token was added to the buffer
    inline int endTokenToErrorBuff(End end) {

        if (end.a != Lex::TK_NONE) {
            tokenToErrorBuff(end.a);
        }

        if (end.b != Lex::TK_NONE) {
            if (end.a != Lex::TK_NONE) {
                char* buff = (char*) Arena::push(&errBuff, 2);
                buff[0] = ',';
                buff[1] = ' ';
            }
            tokenToErrorBuff(end.b);
        }

        return (end.a != Lex::TK_NONE || end.b != Lex::TK_NONE);

    }

    inline int isEndToken(Lex::Token token, const End& end) {
        return token.kind == end.a || token.kind == end.b;
    }

    inline void insertVariableDefinition(Scope* scope, VariableDefinition* def) {
        DArray::push(&scope->children.base, &def);
        DArray::push(&scope->defs.base, &def->var);
    }

    inline void nullValue(Variable* var) {
        var->cvalue = {};
        var->expression = NULL;
    }

    inline void offsetParentIdx(DArraySyntaxNode* vec, const int offset);
    int isArrayLHS(char* const str, Span* span);



    const char* const logTag = "parser";
    const Logger::Type logErr = { .level = Logger::ERROR, .tag = logTag };
    const Logger::Type logWrn = { .level = Logger::WARNING, .tag = logTag };
    const Logger::Type logHnt = { .level = Logger::HINT, .tag = logTag };



    // TODO : change when multi thread support will be added!!!
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



    inline void setParentIdx(SyntaxNode* node) {
        node->parentIdx = node->scope->children.base.size;
    }



    inline Err::Err insertDefSearch(INamed* name, SyntaxNode* node, Span* span) {

        String str = String(name->buff, name->len);
        if (!OrderedDict::set(&node->scope->defSearch, str, node)) {
            Logger::log(logErr, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), span);
            return Err::SYMBOL_ALREADY_DEFINED;
        }

        return Err::OK;

    }

    // huh
    Function* currentFunction = NULL;
    SyntaxNode* currentLoop = NULL;
    Scope* fileRootScope = NULL; // maybe store it in Loacation





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



    void init() {
        Lex::init();
        errorBuffInit();
    }

    Err::Err parseFile(const FileSystem::Handle flhnd, ImportNode* import) {

        FileSystem::Path* path = FileSystem::computeRelativePath(flhnd, SyntaxNode::dir);
        if (!path) {
            Logger::log(logWrn, "Maximum file path size excided while computing relative path!");
        }

        Span span;
        span.fileInfo = FileSystem::getFileInfo(flhnd);
        span.str = FileSystem::getBuffer(flhnd);
        span.start = { 0, 1 };
        span.end = { -1, 1 }; // in overflow we trust

        importCurrent = import;

        Lex::Token token = parseScope(&span, (Scope*) import->fileScope, SC_GLOBAL, SE_DEFAULT);
        return (Err::Err) token.encoded;

    }

    Err::Err processImport(ImportNode* currentNode, String fpath) {

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

                importNode->fileScope = Reg.Node.initNamespace();
                FileSystem::setUserData(flhnd, importNode->fileScope);

                const Err::Err err = parseFile(flhnd, importNode);
                if (err != Err::OK) return err;

            } else {

                importNode->fileScope = fscope;

            }

            importNode->file = flhnd;
            ((SyntaxNode*) importNode->fileScope)->type = NT_SCOPE;

            if (doesImportExistInPath(currentNode, importNode)) {
                Logger::log(logErr, ERR_STR(Err::CIRCULAR_IMPORT), import->base.span);
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
                    for (int i = 0; i < nsc->scope.children.base.size; i++) {

                        SyntaxNode* node = ((SyntaxNode*) nsc->scope.children.base.buffer) + i;
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

                            Namespace* sc = Reg.Node.copy((Namespace*) symbol);

                            sc->scope = root->scope;
                            sc->scope.base.parentIdx = 0;

                            DArray::pushFront(&root->scope.children.base, &sc);
                            DArray::pushFront(&root->scope.namespaces.base, &sc);

                            break;

                        }

                        case NT_FUNCTION : {

                            Function* fcn = Reg.Node.copy((Function*) symbol);

                            fcn->base.scope = (Scope*) root;
                            fcn->base.parentIdx = 0;

                            DArray::pushFront(&root->scope.children.base, &fcn);
                            DArray::pushFront(&root->scope.fcns.base, &fcn);

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
                    sc->base.parentIdx = 0;

                    DArray::pushFront(&root->scope.children.base, &sc);

                    break;

                }

                case KW_FCN : {

                    Function* fcn = Reg.Node.initFunction();

                    fcn->bodyScope = (Scope*) (importNode->fileScope);
                    fcn->name.buff = import->param;
                    fcn->name.len = import->param.len;
                    fcn->base.scope = (Scope*) root;
                    fcn->base.type = NT_FUNCTION;
                    fcn->prototype.outArg = Reg.Node.initVariableDefinition();
                    fcn->prototype.outArg->var->cvalue.dtypeEnum = DT_VOID;
                    fcn->base.parentIdx = 0;
                    fcn->base.flags = 0;

                    fcn->name.id = varId;
                    varId++;

                    // as we importing function, order doesn't matter, so we can push it back
                    DArray::push(&root->scope.children.base, &fcn);
                    DArray::push(&root->scope.fcns.base, &fcn);

                    break;

                }

                case KW_NAMESPACE : {

                    Namespace* sc = (Namespace*) (importNode->fileScope);

                    sc->name.buff = import->param;
                    sc->name.len = import->param.len;
                    sc->scope = root->scope;
                    sc->scope.base.type = NT_NAMESPACE;
                    sc->scope.base.parentIdx = 0;
                    sc->scope.base.flags = 0;

                    DArray::pushFront(&root->scope.children.base, &sc);
                    DArray::pushFront(&root->scope.namespaces.base, &sc);

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
            const Err::Err err = processImport(&chunk->node, fpath);
            if (err != Err::OK) return err;
            chunk = chunk->link;
        }

        return Err::OK;

    }

    Err::Err parse(char* const flname) {

        Err::Err err;

        SyntaxNode::root = Reg.Node.initScope();
        SyntaxNode::root->fcn = NULL;
        SyntaxNode::root->base.scope = NULL;
        SyntaxNode::root->base.parentIdx = 0;

        importRoot.file = FileSystem::load(String(flname));
        if (importRoot.file == FileSystem::null) {
            Logger::log(logErr, "File %s not found!", NULL, flname);
            return Err::FILE_DOES_NOT_EXISTS;
        }

        SyntaxNode::dir = FileSystem::getDirectory(importRoot.file);

        importRoot.fileScope = (Namespace*) SyntaxNode::root;
        importRoot.import = NULL;
        importRoot.parent = NULL;

        Internal::functionUsed = 0;

        err = parseFile(importRoot.file, &importRoot);
        if (err != Err::OK) return err;

        err = processImport(&importRoot, SyntaxNode::dir);
        if (err != Err::OK) return err;

        return Err::OK;

    }




























    Lex::Token parseScope(Span* const span, Scope* scope, const ScopeType scopeType, const ScopeEnd endType) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;

        Scope* node;

        if (scopeType == SC_COMMON) {
            node = Reg.Node.initScope();
            node->base.scope = scope;
            // setParentIdx(node);
        } else {
            node = scope;
            scope = NULL;
        }

        Pos prevPos = lspan.start;
        Lex::Token prevToken = Lex::toToken(Lex::TK_NONE);

        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.encoded < 0) {
                return token;
            }

            switch (token.kind) {

                case Lex::TK_STATEMENT_END : continue;

                case Lex::TK_END : {

                    if (scopeType == SC_GLOBAL) {
                        node->base.span = finalizeSpan(&lspan, span);
                        return Lex::toToken(Err::OK);
                    }

                    span->end = prevPos;

                    Logger::log(logErr, "Unexpected end of file! Showing the start of the relevant section.", &lspan);
                    return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);

                }

                case Lex::TK_SCOPE_BEGIN : {

                    const Lex::Token err = parseScope(&lspan, node, SC_COMMON, SE_DEFAULT);
                    if (err.encoded < 0) return err;
                    break;

                }

                case Lex::TK_SCOPE_END : {

                    if (scopeType != SC_GLOBAL) {
                        node->base.span = finalizeSpan(&lspan, span);
                        DArray::push(&scope->children.base, &node);
                        return token;
                    }

                    // ERROR
                    Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, "");
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);

                }

                case Lex::TK_STRING : {

                    token = parsePrintLiteral(&lspan, node, &tokenVal);
                    if (token.encoded < 0) return token;
                    break;

                }

                case Lex::TK_KEYWORD : {

                    token = parseKeywordStatement(&lspan, node, (Lex::Keyword) token.detail, NULL_FLAG);
                    if (token.encoded < 0) return token;
                    break;

                }

                case Lex::TK_DIRECTIVE : {

                    token = parseDirective(&lspan, node, (Lex::Directive) token.detail, NULL_FLAG);
                    if (token.encoded < 0) return token;
                    break;

                }

                case Lex::TK_ARRAY_BEGIN : {

                    token = parseForeignScope(&lspan, node);
                    if (token.encoded < 0) return token;
                    break;

                }

                case Lex::TK_STATEMENT_BEGIN : {

                    token = parseLabel(&lspan, node);
                    if (token.encoded < 0) return token;
                    break;

                }

                case Lex::TK_IDENTIFIER : {
                    // bare statement or custom data type definition

                    // TODO: would be nice that prevPos starts after whitespaces

                    // TODO: better naming as it can be ambiguous?
                    FullToken startToken = { token, tokenVal };
                    Pos startPos = { lspan.start.idx - 1, lspan.start.ln };

                    token = Lex::peekTokenSkipDecorators(&lspan, NULL);
                    if (token.kind == Lex::TK_IDENTIFIER) {
                        token = parseVariableDefinition(&lspan, node, startToken, End { Lex::TK_STATEMENT_END }, NULL_FLAG, NULL);
                    } else {
                        token = parseBareStatement(&lspan, node, startPos, End { Lex::TK_STATEMENT_END });
                    }

                    if (token.encoded < 0) return token;
                    break;

                }

                default : {
                    // bare statement or error

                    token = parseBareStatement(&lspan, node, prevPos, End { Lex::TK_STATEMENT_END });
                    if (token.encoded < 0) return token;
                    break;

                }

            }

            if (endType == SE_STATEMENT) {
                if (token.kind == Lex::TK_STATEMENT_END) {
                    finalizeSpan(&lspan, span);
                    return token;
                }
                Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            prevPos = lspan.end;
            prevToken = token;

        }

    }

    Lex::Token parsePrintLiteral(Span* const span, Scope* const scope, Lex::TokenValue* const startTokenValue) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        Internal::functionUsed = Internal::functionUsed | (1 << (Internal::IF_PRINTF - 1));

        Function* const fcn = Internal::functions + (Internal::IF_PRINTF - 1);

        FunctionCall* fcnCall = Reg.Node.initFunctionCall();
        fcnCall->fcn = fcn;
        fcnCall->name.buff = fcn->name.buff;
        fcnCall->name.len = fcn->name.len;

        Variable* format = Reg.Node.initVariable();
        format->base.scope = scope;
        format->cvalue.dtypeEnum = DT_STRING;
        format->base.span = getSpanStamp(&lspan);
        format->cvalue.any = startTokenValue->any;

        DArray::push(&fcnCall->inArgs.base, &format);

        Variable* callWrapper = Reg.Node.initVariable();
        callWrapper->base.scope = scope;
        callWrapper->expression = (Expression*) fcnCall;
        callWrapper->cvalue.str = NULL;

        DArray::push(&scope->children.base, &callWrapper);
        DArray::push(&Reg.fcnCalls.base, &callWrapper);

        token = parseList(&lspan, Lex::TK_LIST_SEPARATOR, Lex::TK_STATEMENT_END, &fcnCall->inArgs.base);
        if (token.encoded < 0) return token;

        callWrapper->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseLanguageTag(Span* span, String* tag) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(span, &tokenVal);
        QualifiedName* qname = (QualifiedName*) tokenVal.any;

        if (token.kind != Lex::TK_IDENTIFIER) {
            ndealloc(nalc, qname);
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
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
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        return token;

    }

    Lex::Token parseForeignScope(Span* const span, Scope* scope) {

        SpanEx lspan = markSpanStart(span);

        String tag;
        Lex::Token token;

        token = parseLanguageTag(&lspan, &tag);
        if (token.encoded < 0) return token;

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        const char* const codeStr = span->str + span->end.idx;
        const int codeLen = Lex::findBlockEnd(&lspan, Lex::SCOPE_BEGIN, Lex::SCOPE_END);
        if (codeLen < 0) {
            // ERROR
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_END_OF_FILE), span);
            return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
        }

        CodeBlock* codeBlock = Reg.Node.initCodeBlock();
        codeBlock->code.tagStr = String(tag.buff, tag.len);
        codeBlock->code.codeStr = String((char*) codeStr, codeLen);

        DArray::push(&Reg.codeBlocks.base, &codeBlock);

        codeBlock->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseLabel(Span* span, Scope* scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(&lspan, &tokenVal);

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, "Identifier");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        Label* label = Reg.Node.initLabel();
        label->base.scope = scope;
        label->name.buff = tokenVal.str->buff;
        label->name.len = tokenVal.str->len;
        setParentIdx(&label->base);

        DArray::push(&scope->children.base, &label);
        DArray::push(&scope->labels.base, &label);
        DArray::push(&Reg.labels.base, &label);

        const int ierr = insertDefSearch((INamed*) &label->name, &label->base, span);
        if (ierr != Err::OK) return Lex::toToken(ierr);

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_LABEL_END) {
            uint64_t val = Lex::toIntStr(Lex::LABEL_END);
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, &val);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        label->span = finalizeSpan(&lspan, span);
        return token;

    }

    // assignment or expression
    Lex::Token parseBareStatement(Span* span, Scope* scope, const Pos startPos, const End endToken) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        Variable* lvar = Reg.Node.initVariable();
        token = parseExpression(&lspan, lvar, startPos, endToken, ALLOW_UNEXPECTED_END);
        if (token.encoded < 0) return token;

        if (isEndToken(token, endToken)) {

            Statement* stmt = Reg.Node.initStatement();
            stmt->base.scope = scope;
            stmt->operand = lvar;

            DArray::push(&scope->children.base, &stmt);
            DArray::push(&Reg.statements.base, &stmt);

            stmt->base.span = finalizeSpan(&lspan, span);
            return token;

        }

        if (token.kind != Lex::TK_EQUAL) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, "'='");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        };

        Variable* rvar = Reg.Node.initVariable();
        token = parseRValue(&lspan, scope, rvar, endToken);
        if (token.encoded < 0) return token;

        VariableAssignment* varAssignment = Reg.Node.initVariableAssignment();
        varAssignment->base.span = span;
        varAssignment->base.scope = scope;
        varAssignment->lvar = lvar;
        varAssignment->rvar = rvar;

        // TODO : move to parseRValue?
        if (lvar->cvalue.dtypeEnum == DT_ARRAY) {
            DArray::push(&Reg.arraysAllocations.base, &varAssignment);
        }

        DArray::push(&Reg.variableAssignments.base, &varAssignment);
        DArray::push(&scope->children.base, &varAssignment);

        varAssignment->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseVariableAssignment(Span* span, Scope* scope, const Pos startPos, const End endToken) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        VariableAssignment* const varAssignment = Reg.Node.initVariableAssignment();
        varAssignment->base.span = span;
        varAssignment->base.scope = scope;
        varAssignment->lvar = Reg.Node.initVariable();
        varAssignment->lvar->base.scope = scope;
        varAssignment->rvar = Reg.Node.initVariable();
        varAssignment->rvar->base.scope = scope;

        token = parseExpression(&lspan, varAssignment->lvar, startPos, End { Lex::TK_EQUAL });
        if (token.encoded < 0) return token;

        if (token.kind != Lex::TK_EQUAL) {
            uint64_t val = Lex::toIntStr(Lex::EQUAL);
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, &val);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = parseRValue(&lspan, scope, varAssignment->rvar, endToken);
        if (token.encoded < 0) return token;

        // TODO : move to parseRValue?
        if (varAssignment->lvar->cvalue.dtypeEnum == DT_ARRAY) {
            DArray::push(&Reg.arraysAllocations.base, (void*) &varAssignment);
        }

        DArray::push(&Reg.variableAssignments.base, (void*) &varAssignment);
        DArray::push(&scope->children.base, (void*) &varAssignment);

        varAssignment->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseKnownDataType(Span* const span, Scope* scope, DataTypeEnum dtype, String dtypeName, VariableDefinition* def, Lex::TokenValue* outLastValue) {

        Lex::Token token;

        if (dtype == DT_UNDEFINED) {
            // dt_custom case

            def->var->cvalue.dtypeEnum = DT_CUSTOM;
            def->var->cvalue.any = NULL;
            def->dtype = Reg.Node.initQualifiedName();
            def->dtype->buff = dtypeName.buff;
            def->dtype->len = dtypeName.len;

            DArray::push(&Reg.customDataTypesReferences.base, &def);

        } else if (dtype == DT_FUNCTION) {

            FunctionPrototype* fptr;
            token = parseFunctionPointer(span, scope, &fptr);
            if (token.encoded < 0) return token;

            def->var->cvalue.fcn = fptr;
            def->var->cvalue.dtypeEnum = (DataTypeEnum) DT_FUNCTION;

        } else {

            def->var->cvalue.dtypeEnum = dtype;
            def->var->cvalue.any = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;

        }

        Pointer* lastPtr = NULL;
        token = parseDataTypeDecorators(span, scope, def->var, ALLOW_QUALIFIER, outLastValue, &lastPtr);
        if (token.encoded < 0) return token;

        def->lastPtr = lastPtr;
        def->var->base.span = getSpanStamp(span);

        return token;

    }

    // def->flags will be rewritten
    Lex::Token parseDataType(Span* span, Scope* scope, FullToken prevToken, Flags flags, VariableDefinition* def, Lex::TokenValue* outLastValue) {

        Lex::Token token = prevToken.token;
        Lex::TokenValue tokenVal = prevToken.value;

        def->base.span = getSpanStamp(span);

        if (token.kind == Lex::TK_KEYWORD && (token.detail == Lex::KW_CONST || token.detail == Lex::KW_EMBED)) {

            if (!(flags & ALLOW_QUALIFIER)) {
                Logger::log(logErr, "Qualifier not expected here!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            def->base.flags = ((token.kind == KW_CONST) ? KW_CONST : KW_EMBED); // TODO
            token = Lex::nextToken(span, &tokenVal);

        } else {
            def->base.flags = 0;
        }

        def->var = Reg.Node.initVariable();
        def->var->base.scope = scope;
        def->var->def = def;
        def->var->unrollExpression = 0;
        def->var->expression = NULL;
        def->base.scope = scope;

        if (token.kind == Lex::TK_IDENTIFIER) {

            token = parseKnownDataType(span, scope, DT_CUSTOM, *tokenVal.str, def, outLastValue);

        } else {

            if (!Lex::isDtype(token)) {
                Logger::log(logErr, "Data type expected!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            token = parseKnownDataType(span, scope, Lex::toDtype(token), { NULL, 0 }, def, outLastValue);

        }

        // *val = tokenVal;
        return token;

    }

    Lex::Token parseFunctionPointer(Span* const span, Scope* scope, FunctionPrototype** fcnOut) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        VariableDefinition* def;
        FunctionPrototype* fcn = Reg.Node.initFunctionPrototype();

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_PARENTHESIS_BEGIN) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind == Lex::TK_ARROW) {
            goto fOutArgs;
        }

        while (1) {

            def = Reg.Node.initVariableDefinition();
            DArray::push(&fcn->inArgs.base, &def);

            token = parseDataType(&lspan, scope, { token, tokenVal }, ALLOW_QUALIFIER, def, &tokenVal);
            if (token.encoded < 0) return token;

            if (token.kind == Lex::TK_LIST_SEPARATOR) {
                token = Lex::nextToken(&lspan, NULL);
                continue;
            } else if (token.kind == Lex::TK_ARROW) {
                break;
            } else {
                Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        }

        fOutArgs:

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_PARENTHESIS_END) {

            def = Reg.Node.initVariableDefinition();
            token = parseDataType(&lspan, scope, { token, tokenVal }, NULL_FLAG, def, &tokenVal);
            if (token.encoded < 0) return token;

            if (token.kind != Lex::TK_PARENTHESIS_END) {
                Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        } else {

            def = Reg.Node.initVariableDefinition();

        }

        fcn->outArg = def;
        *fcnOut = fcn;

        def->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    // part starting with variable name in definition
    // ex: const int^ x ... from x
    Lex::Token parseDefinitionAssignment(Span* const span, Scope* scope, FullToken prevToken, VariableDefinition* const def, const End endToken, int includeToScope) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        QualifiedName* qname = (QualifiedName*) prevToken.value.any;
        def->var->name = *qname;

        assignId(def->var);

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind == Lex::TK_EQUAL) {

            token = parseRValue(span, scope, def->var, endToken);
            if (token.encoded < 0) return token;

            if (def->base.flags & IS_CMP_TIME) {
                DArray::push(&Reg.cmpTimeVars.base, &def->var);
            } else {
                DArray::push(&Reg.initializations.base, (void*) &def);
            }

        } else if (token.kind == endToken.a || token.kind == endToken.b) {

            def->var->expression = NULL;

        } else {

            errorBuffClear();
            if (endTokenToErrorBuff(endToken)) {
                errorBuffPush(", ");
            }
            tokenToErrorBuff(Lex::TK_EQUAL);

            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span, errBuff);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        if (includeToScope) {
            //scope->children.push_back(def);
            // pushDefLike(scope->defSearch, def->var);

            setParentIdx((SyntaxNode*) def->var);

            const int ierr = insertDefSearch((INamed*) &def->var->name, (SyntaxNode*) def->var, span);
            if (ierr != Err::OK) return Lex::toToken(ierr);

            DArray::push(&scope->children.base, (void*) &def);
            DArray::push(&Reg.variableDefinitions.base, (void*) &def);
            DArray::push(&scope->defs.base, &def->var);

        }

        return token;

    }

    Lex::Token parseVariableDefinition(Span* const span, Scope* scope, FullToken prevToken, const End endToken, Flags param, VariableDefinition** outVarDef) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        VariableDefinition* def = Reg.Node.initVariableDefinition();
        def->var->base.scope = scope;

        if (Lex::isDtype(prevToken.token)) {
            DataTypeEnum dtype = Lex::toDtype((Lex::Keyword) prevToken.token.detail);
            token = parseKnownDataType(&lspan, scope, dtype, { NULL, 0 }, def, &tokenVal);
        } else {
            token = parseDataType(&lspan, scope, prevToken, ALLOW_QUALIFIER, def, &tokenVal);
        }

        if (token.encoded < 0) return token;

        token = parseDefinitionAssignment(&lspan, scope, { token, tokenVal }, def, endToken, 0);
        if (token.encoded < 0) return token;

        if (outVarDef) *outVarDef = def;
        else insertVariableDefinition(scope, def);

        def->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseStringLiteral(Span* const span, String* str, StringInitialization** initOut) {

        Lex::Token token = Lex::nextToken(span, NULL);
        const int rawStringRequired = token.kind == Lex::TK_RAW ? 1 : 0;

        StringInitialization* init = Reg.Node.initStringInitialization();
        init->rawStr = std::string(str->buff, str->len);
        init->rawPtr = str->buff;
        init->rawPtrLen = str->len;

        // meh but whatever
        if (rawStringRequired) {

            init->wideStr = NULL;
            init->wideDtype = DT_U8;

        } else {

            int utf8Len;
            int utf8BytesPerChar;
            char* utf8Str = Strings::encodeUtf8(init->rawPtr, init->rawPtrLen, &utf8Len, &utf8BytesPerChar, 0);

            if (utf8BytesPerChar != 1) {
                init->wideStr = utf8Str;
                init->wideDtype = (DataTypeEnum) (DT_U8 + utf8BytesPerChar - 1);
                init->wideLen = utf8Len;
            } else {
                init->wideStr = NULL;
                init->wideDtype = DT_U8;
            }

        }

        *initOut = init;

        return Lex::toToken(Lex::TK_STRING);

    }

    Lex::Token parseArrayInitialization(Span* span, Scope* scope, ArrayInitialization** initOut) {

        Lex::Token token;
        *initOut = Reg.Node.initArrayInitialization();

        token = Lex::nextToken(span);

        while (1) {

            Variable* var = Reg.Node.initVariable();
            var->base.scope = scope;

            token = parseExpression(span, var, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_ARRAY_END });
            if (token.kind < 0) return token;

            DArray::push(&(*initOut)->attributes.base, &var);

            if (token.kind == Lex::TK_ARRAY_END) break;
            if (token.kind != Lex::TK_LIST_SEPARATOR) {
                uint64_t tmp = Lex::toIntStr(Lex::LIST_SEPARATOR);
                Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span, &tmp);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        }

        return token;

    }

    Lex::Token parseTypeInitialization(Span* const span, Scope* scope, TypeInitialization** outTypeInit) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        TypeInitialization* dtypeInit = Reg.Node.initTypeInitialization();

        token = Lex::nextToken(&lspan, &tokenVal);
        Lex::Token nextToken = Lex::peekToken(&lspan);

        if (nextToken.kind == Lex::TK_STATEMENT_BEGIN || token.kind == Lex::TK_THE_REST) {
            // names assumed

            int hasFillVar = 0;
            dtypeInit->fillVar = NULL;

            while (1) {

                Variable* var = Reg.Node.initVariable();
                var->base.scope = scope;
                var->base.span = getSpanStamp(&lspan);

                if (token.kind == Lex::TK_THE_REST) {

                    if (hasFillVar) {
                        Logger::log(logErr, "Fill the rest symbol can be used only once per initialization!", span);
                        return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                    }

                    dtypeInit->fillVar = var;
                    hasFillVar = 1;

                } else if (token.kind == Lex::TK_IDENTIFIER) {

                    var->name.buff = tokenVal.str->buff;
                    var->name.len = tokenVal.str->len;
                    DArray::push(&dtypeInit->attributes.base, &var);

                } else {

                    // ERROR
                    Logger::log(logErr, "Attribute name expected!", span);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);

                }

                token = Lex::nextToken(&lspan, NULL);
                if (token.kind != Lex::TK_STATEMENT_BEGIN) {
                    Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, Lex::toStr(Lex::TK_STATEMENT_BEGIN));
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                token = parseExpression(&lspan, var, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_SCOPE_END });
                if (token.encoded < 0) return token;

                if (token.kind == Lex::TK_SCOPE_END) break;
                token = Lex::nextToken(&lspan, &tokenVal);

            }

        } else {

            while (1) {

                Variable* var = Reg.Node.initVariable();

                token = parseExpression(&lspan, var, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_SCOPE_END });
                if (token.encoded < 0) return token;

                DArray::push(&dtypeInit->attributes.base, &var);

                if (token.kind == Lex::TK_SCOPE_END) break;

            }

        }

        dtypeInit->idxs = (int*) nalloc(nalc, AT_BYTE, dtypeInit->attributes.base.size * sizeof(int));
        if (!dtypeInit->idxs) {
            Logger::log(logErr, Err::str[-Err::MALLOC]);
            return Lex::toToken(Err::MALLOC);
        }

        *outTypeInit = dtypeInit;

        span->start = lspan.super;
        span->end = lspan.end;

        return token;

    }

    // either alloc or expression
    // alloc [DtypeName, omitted if mainDtype >= 0] ['[' Expression defining length ']'] [:] [DtypeInit]
    Lex::Token parseRValue(Span* const span, Scope* scope, Variable* outVar, const End endToken) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        DataTypeEnum mainDtype = outVar->cvalue.dtypeEnum;
        if (mainDtype == DT_POINTER) {
            mainDtype = outVar->cvalue.ptr->pointsToEnum;
        } else if (mainDtype == DT_ARRAY) {
            mainDtype = outVar->cvalue.arr->base.pointsToEnum;
        }

        Pos startPos = span->start;

        token = Lex::nextToken(span, NULL);
        if (Lex::isKeyword(token, Lex::KW_ALLOC)) {

            Pos startPos = span->end;

            Pointer* lastPtr = NULL;

            VariableDefinition* varDef = Reg.Node.initVariableDefinition();
            varDef->base.scope = scope;
            varDef->base.span = getSpanStamp(span);

            Variable* var = Reg.Node.initVariable();
            var->def = varDef;

            varDef->var = var;

            token = Lex::nextToken(span, &tokenVal);
            if (token.kind == Lex::TK_IDENTIFIER) {

                var->cvalue.dtypeEnum = DT_CUSTOM;
                var->cvalue.any = NULL;

                varDef->dtype = Reg.Node.initQualifiedName();
                varDef->dtype->buff = tokenVal.str->buff;
                varDef->dtype->len = tokenVal.str->len;

            } else if (token.kind == Lex::TK_KEYWORD) {

                if (!Lex::isDtype(token)) {
                    Logger::log(logErr, "TODO : data type expected, no place for generic keyword!");
                    return Lex::toToken(Err::INVALID_DATA_TYPE);
                }

                var->cvalue.dtypeEnum = Lex::toDtype(token);
                var->cvalue.any = dataTypes + Lex::toDtype(token);

            } else if (mainDtype <= 0) {

                Logger::log(logErr, "TODO : error parseRValue alloc requires dtype name! Can be omitted only in definition!");
                return Lex::toToken(Err::INVALID_DATA_TYPE);

            } else {

                var->cvalue.dtypeEnum = mainDtype;
                var->cvalue.any = mainDtype == DT_CUSTOM ? NULL : dataTypes + mainDtype;

                if (outVar->def) {
                    varDef->dtype = Reg.Node.initQualifiedName();
                    varDef->dtype = outVar->def->dtype;
                }

                span->end = startPos;

            }

            Function* const fcn = Internal::functions + (Internal::IF_ALLOC - 1);

            FunctionCall* fcnCall = Reg.Node.initFunctionCall();
            fcnCall->fptr = NULL;
            fcnCall->fcn = fcn;
            fcnCall->name.buff = fcn->name.buff;
            fcnCall->name.len = fcn->name.len;
            fcnCall->outArg = Reg.Node.initVariable();
            fcnCall->outArg->cvalue.dtypeEnum = DT_POINTER;

            token = parseDataTypeDecorators(span, scope, var, ALLOW_QUALIFIER, &tokenVal, &lastPtr);
            varDef->lastPtr = lastPtr;

            if (token.kind == Lex::TK_STATEMENT_BEGIN) {
                token = parseExpression(span, var, INVALID_POS, endToken);
                if (token.encoded < 0) return token;
            } else if (token.kind != Lex::TK_STATEMENT_END) {
                Logger::log(logErr, "TODO error: parseRValue unexpected symbol!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            DArray::push(&fcnCall->inArgs.base, &var);

            outVar->expression = (Expression*) fcnCall;
            DArray::push(&Reg.fcnCalls.base, &outVar);
            // initializations.push_back(varDef);

            // ...
            DArray::push(&Reg.customDataTypesReferences.base, &varDef);

            outVar->base.flags |= IS_ALLOCATION;

        } else {

            token = parseExpression(span, outVar, startPos, endToken);
            if (token.encoded < 0) return token;

        }

        return token;

    }


    // pointers can ocur only before arrays
    // for now only one array
    Lex::Token parseDataTypeDecorators(Span* const span, Scope* scope, Variable* var, Flags flags, Lex::TokenValue* outLastValue, Pointer** outLastPointer) {

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

                Pointer* ptr = Reg.Node.initPointer();
                if (!mainPtr) *outLastPointer = ptr;
                else mainPtr->parentPointer = ptr;

                ptr->parentPointer = mainPtr;
                ptr->pointsTo = var->cvalue.any;
                ptr->pointsToEnum = var->cvalue.dtypeEnum;

                var->cvalue.dtypeEnum = DT_POINTER;
                var->cvalue.ptr = ptr;

                mainPtr = ptr;

            } else if (token.kind == Lex::TK_ARRAY_BEGIN) {
                // either const / embed or expression

                Pos arrStart = span->end;

                if (wasArray) {
                    Logger::log(logErr, "Multidimensional arrays not allowed!", span);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                wasArray = 1;

                Array* arr = Reg.Node.initArray();
                if (!mainPtr) *outLastPointer = (Pointer*) arr;
                else mainPtr->parentPointer = (Pointer*) arr;

                arr->base.pointsTo = var->cvalue.any;
                arr->base.pointsToEnum = var->cvalue.dtypeEnum;

                Variable* lenVar = Reg.Node.initVariable();
                lenVar->base.span = getSpanStamp(span);
                lenVar->base.scope = scope;
                lenVar->name.len = 0;

                token = Lex::nextToken(span, &tokenVal);
                if (token.kind == Lex::TK_KEYWORD) {

                    const int keyword = token.detail;

                    token = Lex::nextToken(span, NULL);
                    if (token.kind != Lex::TK_ARRAY_END) {
                        Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
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

                    token = parseExpression(span, lenVar, arrStart, End { Lex::TK_ARRAY_END }, EMPTY_EXPRESSION_ALLOWED);
                    if (token.encoded < 0 && token.encoded != Err::UNEXPECTED_END_OF_EXPRESSION) {
                        return token;
                    }

                    arr->flags = IS_CMP_TIME;
                    arr->length = lenVar; // var->allocSize = lenVar;

                }

                // var->flags = varDef->flags ^ IS_ARRAY;
                var->cvalue.dtypeEnum = DT_ARRAY;
                var->cvalue.arr = arr;
                // var->dtype = (void*) arr;
                var->base.flags = 0;

                if (flags & INCLUDE_TO_TREE) {
                    DArray::push(&Reg.arrays.base, &var);
                }

            } else {

                break;

            }

        }

        outLastValue->any = tokenVal.any;
        return token;

    }





    Lex::Token parseKeywordStatement(Span* const span, Scope* const scope, const Lex::Keyword keyword, Flags flags) {

        FullToken prevToken = { Lex::TK_KEYWORD, keyword };

        switch (keyword) {

            case Lex::KW_INT:
            case Lex::KW_I8:
            case Lex::KW_I16:
            case Lex::KW_I32:
            case Lex::KW_I64:
            case Lex::KW_U8:
            case Lex::KW_U16:
            case Lex::KW_U32:
            case Lex::KW_U64:
            case Lex::KW_F32:
            case Lex::KW_F64:
                return parseVariableDefinition(span, scope, prevToken, End { Lex::TK_STATEMENT_END }, flags, NULL);
            case Lex::KW_EMBED:
                flags = flags | IS_CMP_TIME;
            case Lex::KW_CONST:
                flags = flags | IS_CONST;
                return parseVariableDefinition(span, scope, prevToken, End { Lex::TK_STATEMENT_END }, flags, NULL);
            case Lex::KW_FCN:
                return parseFunction(span, scope, flags);
            case Lex::KW_IF:
                return parseIfStatement(span, scope);
            case Lex::KW_WHEN:
                return parseSwitchStatement(span, scope);
            case Lex::KW_FOR:
                return parseForLoop(span, scope);
            case Lex::KW_WHILE:
                return parseWhileLoop(span, scope);
            case Lex::KW_GOTO:
                return parseGotoStatement(span, scope);
            case Lex::KW_ENUM:
                return parseEnumDefinition(span, scope);
            case Lex::KW_DEF:
                return parseTypeDefinition(span, scope);
            case Lex::KW_RETURN:
                return parseReturnStatement(span, scope);
            case Lex::KW_CONTINUE:
                return parseContinueStatement(span, scope);
            case Lex::KW_BREAK:
                return parseBreakStatement(span, scope);
            case Lex::KW_LOOP:
                return parseForEachLoop(span, scope);
            case Lex::KW_NAMESPACE:
                return parseNamespace(span, scope);
            case Lex::KW_ALLOC:
                return parseAllocStatement(span, scope);
            case Lex::KW_FREE:
                return parseFreeStatement(span, scope);
            case Lex::KW_IMPORT:
                return parseImport(span, scope);
            case Lex::KW_ERROR:
                return parseError(span, scope);
            default:
                Logger::log(logErr, "Unsupported keyword processing code encountered!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

    Lex::Token parseFunction(Span* const span, Scope* const scope, uint64_t param) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;

        // first test if it could bee fcn pointer
        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind == Lex::TK_PARENTHESIS_BEGIN) {

            lspan.end.idx = lspan.start.idx - 1;
            token = { .kind = Lex::TK_KEYWORD, .detail = Lex::TD_KW_FCN };

            VariableDefinition* def;
            token = parseVariableDefinition(&lspan, scope, { token, tokenVal }, End { Lex::TK_STATEMENT_END }, param, &def);
            if (token.encoded < 0) return token;

            // pushDefLike(scope->defSearch, def->var);

            const int ierr = insertDefSearch((INamed*) &def->var->name, (SyntaxNode*) def->var, span);
            if (ierr != Err::OK) return Lex::toToken(ierr);

            setParentIdx((SyntaxNode*) def->var);

            DArray::push(&scope->children.base, &def);
            DArray::push(&Reg.variableDefinitions.base, &def);
            DArray::push(&scope->defs.base, &def->var);

            span->end = lspan.end;
            return token;

        }

        Function* fcn;

        Scope* newScope = Reg.Node.initScope();
        newScope->fcn = currentFunction;
        newScope->base.scope = scope;
        // setParentIdx(newScope);

        int foreignLang = 0;
        if (token.kind == Lex::TK_ARRAY_BEGIN) {

            foreignLang = 1;
            fcn = (Function*) Reg.Node.initForeignFunction();

            Span* tagLoc = getSpanStamp(&lspan);

            String tag;
            token = parseLanguageTag(&lspan, &tag);
            if (token.encoded < 0) return token;

            ((ForeignFunction*) fcn)->code.tagStr = { tag.buff, tag.len };
            ((ForeignFunction*) fcn)->code.tagLoc = tagLoc;

            fcn->name.id = varId;
            varId++;

            token = nextToken(&lspan, &tokenVal);

        } else {

            fcn = Reg.Node.initFunction();

        }

        // fcn->inArgsCnt = 0;
        fcn->base.scope = scope;
        fcn->errorSetName = NULL;
        fcn->prototype.outArg = Reg.Node.initVariableDefinition();

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

            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        // parse input
        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_PARENTHESIS_END) break;

            fcn->prototype.inArgsCnt++;

            VariableDefinition* varDef;
            token = parseVariableDefinition(&lspan, newScope, { token, tokenVal }, End { Lex::TK_PARENTHESIS_END, Lex::TK_LIST_SEPARATOR }, param, &varDef);
            if (token.encoded < 0) return token;

            // :)
            varDef->base.parentIdx = -1;
            varDef->base.parentIdx = -1;

            const int ierr = insertDefSearch((INamed*) &varDef->var->name, (SyntaxNode*) varDef->var, span);
            if (ierr != Err::OK) return Lex::toToken(ierr);

            DArray::push(&Reg.variableDefinitions.base, &varDef);
            DArray::push(&newScope->defs.base, &varDef->var);

            DArray::push(&fcn->prototype.inArgs.base, &varDef);

            if (token.kind == Lex::TK_PARENTHESIS_END) break;

        }

        // [using 'Error Set']
        token = Lex::nextToken(&lspan, &tokenVal);
        if (Lex::isKeyword(token, Lex::KW_USING)) {

            fcn->errorSetName = Reg.Node.initQualifiedName();
            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.encoded < 0) return token;

            Using* tmp = Reg.Node.initUsing();
            tmp->var = (SyntaxNode*) fcn;

            DArray::push(&newScope->usings.base, &tmp);

            token = Lex::nextToken(&lspan, &tokenVal);

        }

        // ->
        if (token.kind != Lex::TK_ARROW) {

            if (token.kind == Lex::TK_SCOPE_BEGIN) goto fcnParseScope;

            // LOOK AT : maybe better name for this situation, something like "unexpected char sequence"
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            Logger::log(logHnt, "'->' expected\n");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        // parse output
        token = Lex::nextToken(&lspan, &tokenVal);
        token = parseDataType(&lspan, scope, { token, tokenVal }, NULL_FLAG, fcn->prototype.outArg, &tokenVal);
        if (token.encoded < 0) return token;

        // scope
        if (token.kind != Lex::TK_SCOPE_BEGIN && token.kind != Lex::TK_STATEMENT_BEGIN) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
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
            DArray::push(&scope->fcns.base, &ffcn);
            DArray::push(&Reg.foreignFunctions.base, &ffcn);

            ffcn->fcn.base.span = finalizeSpan(&lspan, span);
            return token;

        }

        fcn->internalIdx = 0;

        currentFunction = fcn;

        const ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
        token = parseScope(&lspan, newScope, SC_COMMON, scopeEnd);
        if (token.kind < 0) return token;

        currentFunction = NULL;

        fcn->bodyScope = newScope;

        DArray::push(&scope->children.base, &fcn);
        DArray::push(&scope->fcns.base, &fcn);
        DArray::push(&Reg.fcns.base, &fcn);

        fcn->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseIfStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;
        const int parentIdx = scope->children.base.size;

        Scope* newScope = Reg.Node.initScope();
        newScope->fcn = currentFunction;
        newScope->base.scope = scope;
        newScope->base.parentIdx = parentIdx;

        Variable* newOperand = Reg.Node.initVariable();
        newOperand->base.scope = scope;

        token = parseExpression(&lspan, newOperand, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
        if (token.encoded < 0) return token;

        const ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
        token = parseScope(&lspan, newScope, SC_COMMON, scopeEnd);
        if (token.encoded < 0) return token;

        Branch* branch = Reg.Node.initBranch();
        branch->base.scope = scope;

        DArray::push(&branch->scopes.base, &newScope);
        DArray::push(&branch->expressions.base, &newOperand);

        DArray::push(&scope->children.base, &branch);
        DArray::push(&Reg.branchExpressions.base, &newOperand);

        while (1) {

            token = Lex::tryKeyword(&lspan, Lex::KW_ELSE);
            if (token.kind != Lex::TK_KEYWORD) {
                branch->base.span = finalizeSpan(&lspan, span);
                return token;
            }

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_BEGIN || token.kind == Lex::TK_STATEMENT_BEGIN) {
                // else case

                Scope* newScope = Reg.Node.initScope();
                newScope->fcn = currentFunction;
                newScope->base.scope = scope;
                newScope->base.parentIdx = parentIdx;

                ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
                token = parseScope(&lspan, newScope, SC_COMMON, scopeEnd);
                if (token.encoded < 0) return token;

                DArray::push(&branch->scopes.base, &newScope);

                branch->base.span = finalizeSpan(&lspan, span);
                return token;

            }

            // else if case

            if (!Lex::isKeyword(token, Lex::KW_IF)) {
                Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, "if, ':' or '{'");
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            Variable* newOperand = Reg.Node.initVariable();
            newOperand->base.scope = scope;
            newOperand->base.span = getSpanStamp(&lspan);

            token = parseExpression(&lspan, newOperand, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
            if (token.encoded < 0) return token;

            Scope *newScope = Reg.Node.initScope();
            newScope->fcn = currentFunction;
            newScope->base.scope = scope;
            newScope->base.parentIdx = parentIdx;

            ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
            token = parseScope(&lspan, newScope, SC_COMMON, scopeEnd);
            if (token.encoded < 0) return token;

            DArray::push(&branch->scopes.base, &newScope);
            DArray::push(&branch->expressions.base, &newOperand);
            DArray::push(&Reg.branchExpressions.base, &newOperand);

        }

    }

    Lex::Token parseSwitchStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;

        SwitchCase* switchCase = Reg.Node.initSwitchCase();
        switchCase->base.scope = scope;
        switchCase->elseCase = NULL;

        Variable* var = Reg.Node.initVariable();
        var->base.scope = scope;

        token = parseExpression(&lspan, var, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
        if (token.encoded < 0) return token;

        switchCase->switchExp = var;

        int asStatement = token.kind == Lex::TK_STATEMENT_BEGIN;
        int elseCase = 0;
        int atLeastOneCase = 0;
        while (1) {

            if (!asStatement) {
                token = Lex::nextToken(&lspan);
                if (token.kind == Lex::TK_SCOPE_END) break;
            } else {
                token = Lex::tryKeyword(&lspan, Lex::KW_CASE);
                if (token.kind != Lex::TK_KEYWORD) {
                    token = Lex::tryKeyword(&lspan, Lex::KW_ELSE);
                    if (token.kind != Lex::TK_KEYWORD) break;
                }
            }

            elseCase = token.detail == Lex::TD_KW_ELSE;
            atLeastOneCase = 1;

            Variable* cmpExp;
            if (!elseCase) {
                cmpExp = Reg.Node.initVariable();
                cmpExp->base.scope = scope;
                token = parseExpression(&lspan, cmpExp, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
                if (token.encoded < 0) return token;
            } else {
                if (!atLeastOneCase) {
                    Logger::log(logErr, "Else case can't be the first case!", &lspan);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }
                token = Lex::nextToken(&lspan);
            }

            Scope* sc = Reg.Node.initScope();
            sc->fcn = currentFunction;
            sc->base.scope = scope;
            // setParentIdx(sc);

            if (token.kind == Lex::TK_STATEMENT_BEGIN) {
                token = parseScope(&lspan, sc, SC_COMMON, SE_STATEMENT);
            } else if (token.kind == Lex::TK_SCOPE_BEGIN) {
                token = parseScope(&lspan, sc, SC_COMMON, SE_DEFAULT);
            } else {
                Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            if (elseCase) {
                switchCase->elseCase = sc;
                break;
            }

            DArray::push(&switchCase->casesExp.base, &cmpExp);
            DArray::push(&switchCase->cases.base, &sc);

        }

        DArray::push(&Reg.switchCases.base, &switchCase);
        DArray::push(&scope->children.base, &switchCase);

        switchCase->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseForLoop(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        // TODO : something about empty expressions, maybe make them null or something...
        Lex::Token token;
        Lex::TokenValue tokenVal;

        ForLoop* loop = Reg.Node.initForLoop();

        Scope* outerScope = Reg.Node.initScope();
        outerScope->fcn = currentFunction;
        outerScope->base.scope = scope;
        // setParentIdx(outerScope);

        Variable* initEx = Reg.Node.initVariable();
        initEx->base.scope = outerScope;

        // can be either variable initialization or expression
        Pos startPos = lspan.end;
        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind == Lex::TK_KEYWORD && Lex::isDtype(token)) {
            token = parseVariableDefinition(&lspan, scope, { token, tokenVal }, End { Lex::TK_STATEMENT_END }, NULL_FLAG, NULL);
        } else {
            token = parseExpression(&lspan, initEx, startPos, End { Lex::TK_STATEMENT_END }, EMPTY_EXPRESSION_ALLOWED);
        }
        if (token.encoded < 0) return token;

        Scope* bodyScope = Reg.Node.initScope();
        bodyScope->fcn = currentFunction;
        bodyScope->base.scope = outerScope;
        // setParentIdx(bodyScope);

        Variable* conditionEx = Reg.Node.initVariable();
        conditionEx->base.scope = outerScope;

        token = parseExpression(&lspan, conditionEx, INVALID_POS, End { Lex::TK_STATEMENT_END }, EMPTY_EXPRESSION_ALLOWED);
        if (token.kind < 0) return token;

        // can be assignment
        Variable* actionEx = Reg.Node.initVariable();
        actionEx->base.scope = outerScope;

        token = parseExpression(&lspan, actionEx, INVALID_POS, End { Lex::TK_SCOPE_BEGIN, Lex::TK_STATEMENT_BEGIN }, EMPTY_EXPRESSION_ALLOWED);
        if (token.encoded < 0) return token;

        SyntaxNode* tmpLoop = currentLoop;
        currentLoop = (SyntaxNode*) loop;

        if (token.kind == Lex::TK_SCOPE_BEGIN || token.kind == Lex::TK_STATEMENT_BEGIN) {
            ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
            token = parseScope(&lspan, bodyScope, SC_COMMON, scopeEnd);
            if (token.kind < 0) return token;
        } else {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        currentLoop = tmpLoop;

        loop->base.scope = scope;
        loop->bodyScope = bodyScope;
        loop->initEx = initEx;
        loop->conditionEx = conditionEx;
        loop->actionEx = actionEx;

        DArray::push(&outerScope->children.base, &loop);
        DArray::push(&scope->children.base, &outerScope);

        if (conditionEx->expression) {
            DArray::push(&Reg.branchExpressions.base, &conditionEx);
        } else {
            /* TODO: may lead to something...
            loop->conditionEx = NULL;
            freeSpanStamp(conditionEx->base.span);
            delete conditionEx;
            */
        }

        loop->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseWhileLoop(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        WhileLoop* loop = Reg.Node.initWhileLoop();

        Scope* newScope = Reg.Node.initScope();
        newScope->fcn = currentFunction;
        newScope->base.scope = scope;
        // setParentIdx(newScope);

        Variable* newOperand = Reg.Node.initVariable();
        newOperand->base.scope = newScope;

        token = parseExpression(&lspan, newOperand, INVALID_POS, End { Lex::TK_SCOPE_BEGIN, Lex::TK_STATEMENT_BEGIN });
        if (token.kind < 0) return token;

        SyntaxNode* tmpLoop = currentLoop;
        currentLoop = (SyntaxNode*) loop;

        ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
        token = parseScope(&lspan, newScope, SC_COMMON, scopeEnd);
        if (token.kind < 0) return token;

        currentLoop = tmpLoop;

        loop->base.scope = scope;
        loop->bodyScope = newScope;
        loop->expression = newOperand;

        DArray::push(&scope->children.base, &loop);

        loop->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseForEachLoop(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(span, &tokenVal);

        Loop* const loop = Reg.Node.initLoop();

        Scope* outerScope = Reg.Node.initScope();
        outerScope->fcn = currentFunction;
        outerScope->base.scope = scope;
        // setParentIdx(outerScope);

        loop->base.scope = outerScope;

        Variable* newVar = Reg.Node.initVariable();
        newVar->base.scope = loop->base.scope;
        newVar->base.span = getSpanStamp(span);

        token = parseExpression(&lspan, newVar, INVALID_POS, End { Lex::TK_STATEMENT_END }, USE_KEYWORD_AS_END);
        if (token.kind < 0) return token;

        if (token.kind != Lex::TK_KEYWORD && token.detail != KW_USING) {
            Logger::log(logErr, ERR_STR(Err::INVALID_VARIABLE_NAME), span, "Variable name is matching key word name!");
            return Lex::toToken(Err::INVALID_VARIABLE_NAME);
        }

        loop->array = newVar;

        // var or var def (only basic dtypes)
        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind == Lex::TK_KEYWORD) {
            // var def

            if (!Lex::isInt((Lex::Keyword) token.detail)) {
                Logger::log(logErr, "Only integral datatypes allowed!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            loop->idx = NULL;
            loop->idxDef = Reg.Node.initVariableDefinition();
            loop->idxDef->var->cvalue.dtypeEnum = Lex::toDtype(token);

            token = parseVariableDefinition(&lspan, outerScope, { token, tokenVal }, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN }, NULL_FLAG, &(loop->idxDef));
            if (token.encoded < 0) return token;

            if (loop->idxDef->var->expression == NULL) {
                loop->idxDef->var->cvalue.hasValue = 1;
                loop->idxDef->var->cvalue.i64 = 0;
                loop->idxDef->var->cvalue.dtypeEnum = DT_INT;
            }

        } else {
            // var

            loop->idxDef = NULL;
            loop->idx = Reg.Node.initVariable();
            loop->idx->base.scope = outerScope;
            loop->idx->name.buff = tokenVal.str->buff;
            loop->idx->name.len = tokenVal.str->len;

            token = Lex::nextToken(&lspan);

        }

        loop->bodyScope = Reg.Node.initScope();
        loop->bodyScope->fcn = currentFunction;
        loop->bodyScope->base.scope = loop->base.scope;
        // setParentIdx(loop->bodyScope);

        SyntaxNode* tmpLoop = currentLoop;
        currentLoop = (SyntaxNode*) loop;

        ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
        token = parseScope(&lspan, loop->bodyScope, SC_COMMON, scopeEnd);
        if (token.encoded < 0) return token;

        currentLoop = tmpLoop;

        DArray::push(&Reg.loops.base, (void*) &loop);
        DArray::push(&scope->children.base, (void*) &loop);

        loop->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseGotoStatement(Span* const span, Scope* const scope) {

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(span, &tokenVal);

        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(logErr, "Identifier expected!", span);
            return token.encoded < 0 ? token : Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        GotoStatement* gt = Reg.Node.initGotoStatement();
        gt->span = getSpanStamp(span);
        gt->base.scope = scope;
        gt->name.buff = tokenVal.str->buff;
        gt->name.len = tokenVal.str->len;
        gt->label = NULL;

        DArray::push(&scope->children.base, &gt);
        DArray::push(&scope->gotos.base, &gt);
        DArray::push(&Reg.gotos.base, &gt);

        return token;

    }

    Lex::Token parseContinueStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_STATEMENT_END) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (!currentLoop) {
            Logger::log(logErr, "Continue statement used outside of the loop!");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        ContinueStatement* stmt = Reg.Node.initContinueStatement();
        stmt->base.scope = scope;

        DArray::push(&scope->children.base, &stmt);

        stmt->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseBreakStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_STATEMENT_END) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (!currentLoop) {
            Logger::log(logErr, "Break statement used outside of the loop!");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        BreakStatement* stmt = Reg.Node.initBreakStatement();
        stmt->base.scope = scope;

        DArray::push(&scope->children.base, &stmt);

        stmt->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseNamespace(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(&lspan, &tokenVal);

        Namespace* nsc = Reg.Node.initNamespace();
        nsc->scope.base.scope = scope;
        nsc->name.buff = tokenVal.str->buff;
        nsc->name.len = tokenVal.str->len;
        // setParentIdx(nsc);

        token = Lex::nextToken(&lspan);
        if (token.kind != Lex::TK_SCOPE_BEGIN && token.kind != Lex::TK_STATEMENT_BEGIN) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
        token = parseScope(&lspan, (Scope*) nsc, SC_COMMON, scopeEnd);
        if (token.encoded < 0) return token;

        DArray::push(&scope->children.base, &nsc);
        DArray::push(&scope->namespaces.base, &nsc);

        nsc->scope.base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseAllocStatement(Span* const span, Scope* const scope) {

        return Lex::Token { Lex::TK_END };

    }

    Lex::Token parseFreeStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        Function* const fcn = Internal::functions + (Internal::IF_FREE - 1);

        FunctionCall* fcnCall = Reg.Node.initFunctionCall();
        fcnCall->fptr = NULL;
        fcnCall->fcn = fcn;
        fcnCall->name.buff = fcn->name.buff;
        fcnCall->name.len = fcn->name.len;
        fcnCall->outArg = Reg.Node.initVariable();
        fcnCall->outArg->cvalue.dtypeEnum = DT_VOID;

        Variable* inVar = Reg.Node.initVariable();
        token = parseExpression(&lspan, inVar, INVALID_POS, End { Lex::TK_STATEMENT_END });
        if (token.encoded < 0) return token;

        fcnCall->inArgsCnt = 1;
        DArray::push(&fcnCall->inArgs.base, &inVar);

        Variable* wrapper = Reg.Node.initVariable();
        wrapper->base.scope = scope;
        wrapper->expression = (Expression*) fcnCall;
        wrapper->base.span = getSpanStamp(&lspan);

        Statement* st = Reg.Node.initStatement();
        st->base.scope = scope;
        st->operand = wrapper;

        DArray::push(&Reg.fcnCalls.base, &wrapper);
        DArray::push(&scope->children.base, &st);

        token = Lex::nextToken(&lspan);
        return token;

    }

    Lex::Token parseReturnStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        ReturnStatement* ret = Reg.Node.initReturnStatement();
        ret->fcn = currentFunction;
        ret->base.scope = scope;

        ret->var = NULL;
        ret->err = NULL;

        ret->idx = currentFunction->returns.base.size;
        DArray::push(&currentFunction->returns.base, &ret);

        DArray::push(&scope->children.base, &ret);
        DArray::push(&Reg.returnStatements.base, &ret);

        Pos startPos = lspan.end;

        token = Lex::nextToken(&lspan);
        if (token.kind == Lex::TK_STATEMENT_END) {
            ret->base.span = finalizeSpan(&lspan, span);
            return token;
        }

        if (token.kind != Lex::TK_SKIP) {

            Variable* newVar = Reg.Node.initVariable();
            newVar->base.scope = scope;
            newVar->base.span = getSpanStamp(&lspan);
            newVar->cvalue.dtypeEnum = DT_I64;

            token = parseExpression(&lspan, newVar, startPos, End { Lex::TK_LIST_SEPARATOR, Lex::TK_STATEMENT_END });
            if (token.encoded < 0) return token;

            ret->var = newVar;

        } else {

            token = Lex::nextToken(&lspan);

        }

        if (token.kind == Lex::TK_STATEMENT_END) {
            ret->base.span = finalizeSpan(&lspan, span);
            return token;
        }

        if (token.kind != Lex::TK_LIST_SEPARATOR) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, Lex::toStr(Lex::TK_LIST_SEPARATOR));
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        // error case
        Variable* newVar = Reg.Node.initVariable();
        newVar->base.scope = scope;
        newVar->base.span = getSpanStamp(&lspan);
        newVar->cvalue.dtypeEnum = DT_I64;

        token = parseExpression(&lspan, newVar, INVALID_POS, End { Lex::TK_STATEMENT_END });
        if (token.kind < 0) return token;

        ret->err = newVar;

        ret->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseEnumDefinition(Span* const span, Scope* const scope) {

        // enum <name> : <type> { .. }

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;

        Enumerator* enumerator = Reg.Node.initEnumerator();
        enumerator->dtype = DT_INT;

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

            if (!Lex::isInt((Lex::Keyword) token.detail)) {
                Logger::log(logErr, ERR_STR(Err::UNKNOWN_DATA_TYPE), &lspan);
                return Lex::toToken(Err::UNKNOWN_DATA_TYPE);
            }

            enumerator->dtype = Lex::toDtype((Lex::Keyword) token.detail);

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
        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            VariableDefinition* newVarDef = Reg.Node.initVariableDefinition();
            Variable* newVar = Reg.Node.initVariable();
            newVar->base.scope = scope;
            newVar->base.span = getSpanStamp(&lspan);
            newVar->cvalue.dtypeEnum = enumerator->dtype;

            newVarDef->var = newVar;
            newVarDef->var->cvalue.hasValue = 0;
            newVarDef->var->cvalue.dtypeEnum = enumerator->dtype;
            newVarDef->base.span = getSpanStamp(&lspan);
            newVarDef->base.flags = IS_CMP_TIME;

            newVar->def = newVarDef;
            newVar->name.buff = tokenVal.str->buff;
            newVar->name.len = tokenVal.str->len;
            newVar->unrollExpression = 1;

            assignId(newVar);

            DArray::push(&enumerator->vars.base, &newVar);

            token = Lex::nextToken(&lspan);
            if (token.kind == Lex::TK_EQUAL) {

                token = parseExpression(&lspan, newVar, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_SCOPE_END }, 0);
                if (token.encoded < 0) return token;

                newVarDef->var->cvalue.hasValue = 1;

            }

            if (token.kind == Lex::TK_LIST_SEPARATOR) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;

            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, "',' or '}'");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        DArray::push(&Reg.enumerators.base, &enumerator);
        DArray::push(&scope->children.base, &enumerator);
        DArray::push(&scope->enums.base, &enumerator);

        enumerator->base.span = finalizeSpan(&lspan, span);

        return token;

    }

    Lex::Token parseTypeDefinition(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(&lspan, &tokenVal);

        Lex::Keyword keyword;
        if (token.kind == Lex::TK_KEYWORD) {

            if (token.detail == Lex::TD_KW_UNION || token.detail == Lex::TD_KW_STRUCT) {
                keyword = (Lex::Keyword) token.detail;
            } else {
                Logger::log(logErr, "Unexpected symbol!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            token = Lex::nextToken(&lspan);

        } else {

            keyword = Lex::KW_STRUCT;

        }

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(logErr, "Identifier expected!", &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        TypeDefinition* newTypeDefinition;
        if (keyword == Lex::KW_STRUCT) {
            newTypeDefinition = Reg.Node.initTypeDefinition();
        } else {
            newTypeDefinition = (TypeDefinition*) Reg.Node.initUnion();
        }

        newTypeDefinition->base.scope = scope;
        newTypeDefinition->name.buff = tokenVal.str->buff;
        newTypeDefinition->name.len = tokenVal.str->len;
        newTypeDefinition->base.parentIdx = Reg.customDataTypes.base.size;
        newTypeDefinition->base.span = getSpanStamp(&lspan);

        newTypeDefinition->name.id = defId;
        defId++;

        token = Lex::nextToken(&lspan);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        DArray::push(&Reg.customDataTypes.base, &newTypeDefinition);
        DArray::push(&scope->customDataTypes.base, &newTypeDefinition);
        DArray::push(&scope->children.base, &newTypeDefinition);

        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            VariableDefinition* varDef;
            token = parseVariableDefinition(&lspan, scope, { token, tokenVal }, End { Lex::TK_SCOPE_END, Lex::TK_STATEMENT_END }, NULL_FLAG, &varDef);
            if (token.kind < 0) return token;

            DArray::push(&newTypeDefinition->vars.base, &varDef->var);

            if (token.kind == Lex::TK_STATEMENT_END) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;

            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        newTypeDefinition->base.span = finalizeSpan(&lspan, span);

        return token;

    }

    Lex::Token parseErrorDeclaration(Span* const span, Scope* scope) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        VariableDefinition* def = Reg.Node.initVariableDefinition();
        def->var->name.buff = tokenVal.str->buff;
        def->var->name.len = tokenVal.str->len;
        def->var->cvalue.dtypeEnum = DT_ERROR;
        def->var->base.span = getSpanStamp(span);
        def->base.scope = scope;
        setParentIdx((SyntaxNode*) def->var);

        const int ierr = insertDefSearch((INamed*) &def->var->name, (SyntaxNode*) def->var, span);
        if (ierr != Err::OK) return Lex::toToken(ierr);

        assignId(def->var);

        token = Lex::nextToken(span, NULL);
        if (token.kind == Lex::TK_EQUAL) {

            token = parseExpression(span, def->var, INVALID_POS, End { Lex::TK_STATEMENT_END, Lex::TK_NONE });
            if (token.kind < 0) return token;

        } else if (token.kind != Lex::TK_STATEMENT_END) {

            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        DArray::push(&Reg.variableDefinitions.base, &def);
        DArray::push(&scope->defs.base, &def->var);
        DArray::push(&scope->children.base, &def);

        return token;

    }

    Lex::Token parseErrorDefinition(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(&lspan, &tokenVal);

        ErrorSet* errorSet = Reg.Node.initErrorSet();
        errorSet->base.scope = scope;
        errorSet->name.buff = tokenVal.str->buff;
        errorSet->name.len = tokenVal.str->len;
        errorSet->value = errId;
        errId++;

        errorSet->name.id = varId;
        varId++;

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, Lex::toStr(Lex::TK_SCOPE_BEGIN));
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            Variable* var = Reg.Node.initVariable();
            var->base.scope = scope;
            var->name.buff = tokenVal.str->buff;
            var->name.len = tokenVal.str->len;
            var->base.span = getSpanStamp(&lspan);
            var->cvalue.hasValue = 0;
            var->cvalue.dtypeEnum = DT_ERROR;
            var->cvalue.u64 = errId;
            errId++;

            DArray::push(&errorSet->vars.base, &var);

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_STATEMENT_END) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;

            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        DArray::push(&Reg.customErrors.base, &errorSet);
        DArray::push(&scope->customErrors.base, &errorSet);
        DArray::push(&scope->children.base, &errorSet);

        errorSet->base.span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseError(Span* const span, Scope* const scope) {

        Lex::TokenValue tokenVal;
        Lex::Token token;

        token = Lex::peekNthToken(span, &tokenVal, 2);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {

            token = parseErrorDeclaration(span, scope);
            if (token.encoded < 0) return token;

        }

        token = parseErrorDefinition(span, scope);
        if (token.encoded < 0) return token;

        return token;

    }

    Lex::Token parseImport(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        ImportStatement* import = Reg.Node.initImportStatement();
        // import->root = fileRootScope;
        import->base.scope = scope;

        ImportNode* importNode = initImportNode();
        importNode->import = import;
        importNode->parent = importCurrent;

        token = Lex::tryKeyword(&lspan, Lex::KW_FROM);
        if (token.kind != Lex::TK_KEYWORD) {
            token = Lex::nextFileName(&lspan, &tokenVal);
        }

        import->fname = *tokenVal.str;
        import->keyword = KW_VOID;

        pushImportChunk(importCurrent, importNode);

        token = Lex::nextToken(&lspan, &tokenVal);
        if (!Lex::isKeyword(token, Lex::KW_AS)) {
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

        import->keyword = (KeywordType) token.detail;

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

    Lex::Token parseDirective(Span* const span, Scope* const scope, Lex::Directive directive, Flags param) {

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

    Lex::Token parseList(Span* const span, Lex::TokenKind separator, Lex::TokenKind end, DArray::Container* args) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        int first = 1;
        while (1) {

            Variable* newVar = (Variable*) nalloc(nalc, NT_VARIABLE);
            token = parseExpression(span, newVar, INVALID_POS, End { separator, end }, first ? EMPTY_EXPRESSION_ALLOWED : 0);
            if (token.encoded < 0) return token;

            if (first && (token.kind == end)) return token;

            DArray::push(args, &newVar);

            if (token.kind == end) break;
            if (token.kind != separator) {
                Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            first = 0;

        }

        return Lex::Token { Lex::TK_END };

    }

    Lex::Token parseCatch(Span* const span, Variable* var) {

        Lex::Token token;
        Lex::TokenValue tokenVal;



        Variable* errVar = Reg.Node.initVariable();
        errVar->base.scope = var->base.scope;
        errVar->cvalue.dtypeEnum = DT_ERROR;

        Scope* newScope = Reg.Node.initScope();
        newScope->base.scope = var->base.scope;
        // setParentIdx(newScope);

        Catch* cex = Reg.Node.initCatch();
        cex->call = (FunctionCall*) var->expression;

        var->expression = (Expression*) cex;



        token = Lex::nextToken(span, &tokenVal);

        if (Lex::isKeyword(token, Lex::KW_RETURN)) {
            // basically just emulates following
            // .. .. catch err { return _, err; }

            ReturnStatement* ret = Reg.Node.initReturnStatement();
            ret->err = errVar;
            ret->var = NULL;
            ret->base.scope = newScope;
            ret->fcn = currentFunction;

            DArray::push(&newScope->children.base, &ret);
            DArray::push(&Reg.returnStatements.base, &ret);

            token = Lex::nextToken(span, &tokenVal);
            if (token.kind != Lex::TK_STATEMENT_END) {
                Logger::log(logErr, "'catch return' expression has to be terminated with ';'!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            return token;
        }

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span, "error identifier");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        QualifiedName* errName = (QualifiedName*) (tokenVal.any);

        token = Lex::nextToken(span, &tokenVal);
        if (
            token.kind != Lex::TK_SCOPE_BEGIN &&
            token.kind != Lex::TK_STATEMENT_BEGIN &&
            !Lex::isKeyword(token, Lex::KW_RETURN)
        ) {
            // 'catch err' case

            errVar->name = *errName;

            DArray::push(&Reg.variables.base, &var);

            cex->err = errVar;
            cex->scope = NULL;

            return token;

        }



        // 'catch err { ... }' case
        VariableDefinition* errDef = Reg.Node.initVariableDefinition();
        errDef->var = errVar;
        errDef->base.scope = newScope;
        errDef->var->base.scope = newScope;
        errDef->var->cvalue.dtypeEnum = DT_ERROR;
        errDef->var->base.span = getSpanStamp(span);
        errDef->var->base.parentIdx = -1;

        errDef->var->name = *errName;

        assignId(errDef->var);

        DArray::push(&newScope->defs.base, &errDef->var);
        DArray::push(&Reg.variableDefinitions.base, &errDef);

        const int ierr = insertDefSearch((INamed*) &errDef->var->name, (SyntaxNode*) errDef->var, span);
        if (ierr != Err::OK) return Lex::toToken(ierr);

        cex->err = errDef->var;
        cex->scope = newScope;

        ScopeEnd scopeEnd = (ScopeEnd) (token.kind == Lex::TK_STATEMENT_BEGIN);
        return parseScope(span, newScope, SC_COMMON, scopeEnd);

    }



    Lex::Token parseExpressionNode(Span* const span, Scope* scope, Variable* var, int lastDefIdx) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        int consumeToken = 1;

        while (1) {

            token = Lex::nextToken(span, &tokenVal);

            OperatorEnum op = Lex::toUnaryOperator(token);
            if (op == OP_NONE) {
                break;
            }

            UnaryExpression* uex = Reg.Node.initUnaryExpression();
            uex->base.opType = op;

            var->expression = (Expression*) uex;
            var = Reg.Node.initVariable();

        }

        switch (token.kind) {

            case Lex::TK_IDENTIFIER: {

                token = Lex::nextToken(span, &tokenVal);
                if (token.kind == Lex::TK_PARENTHESIS_BEGIN) {

                    FunctionCall* call = Reg.Node.initFunctionCall();
                    call->fptr = NULL;
                    call->fcn = NULL;

                    token = parseList(span, Lex::TK_LIST_SEPARATOR, Lex::TK_PARENTHESIS_END, &call->inArgs.base);
                    if (token.encoded < 0) return token;

                    call->name = *((QualifiedName* )tokenVal.any);

                    var->cvalue.hasValue = 0;
                    var->expression = (Expression*) call;

                } else {

                    var->name = *((QualifiedName*) tokenVal.any);
                    consumeToken = 0;

                }

                break;

            }

            case Lex::TK_NUMBER: {

                var->name.buff = NULL;
                var->name.len = 0;
                var->expression = NULL;
                var->cvalue.hasValue = 1;
                var->cvalue.dtypeEnum = Lex::toDtype(token);
                var->unrollExpression = 0;

                if (token.detail == Lex::TD_DT_U64) {
                    var->cvalue.u64 = tokenVal.ival;
                }
                else if (token.detail == Lex::TD_DT_I64) {
                    var->cvalue.i64 = tokenVal.ival;
                }
                else if (token.detail == Lex::TD_DT_F64) {
                    var->cvalue.f64 = tokenVal.fval;
                }

                break;

            }

            case Lex::TK_KEYWORD: {

                if (token.detail == Lex::TD_KW_TRUE) {

                    var->cvalue.hasValue = 1;
                    var->cvalue.dtypeEnum = DT_BOOL;
                    var->cvalue.u64 = 1;

                } else if (token.detail == Lex::TD_KW_FALSE) {

                    var->cvalue.hasValue = 1;
                    var->cvalue.dtypeEnum = DT_BOOL;
                    var->cvalue.u64 = 0;

                } else if (token.detail == Lex::TD_KW_NULL) {

                    var->cvalue.hasValue = 1;
                    var->cvalue.dtypeEnum = DT_POINTER;
                    var->cvalue.u64 = 0;

                }

                break;

            }

            case Lex::TK_PARENTHESIS_BEGIN: {

                token = parseExpression(span, var, INVALID_POS, End { Lex::TK_PARENTHESIS_END, Lex::TK_NONE }, 0, lastDefIdx);
                if (token.kind < 0) return token;

                break;

            }

            case Lex::TK_STRING: {

                StringInitialization* init = Reg.Node.initStringInitialization();
                init->rawPtr = tokenVal.str->buff;
                init->rawPtrLen = tokenVal.str->len;

                var->expression = (Expression*) init;

                break;

            }

            case Lex::TK_CHAR: {

                var->cvalue.dtypeEnum = DT_I64;
                var->cvalue.i64 = tokenVal.ival;

                break;

            }

            case Lex::TK_ARRAY_BEGIN: {

                ArrayInitialization* init = Reg.Node.initArrayInitialization();
                token = parseArrayInitialization(span, scope, &init);
                var->expression = (Expression*) init;

                break;

            }

            case Lex::TK_SCOPE_BEGIN: {

                TypeInitialization* init = Reg.Node.initTypeInitialization();
                token = parseTypeInitialization(span, scope, &init);
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

            UnaryExpression* uex = Reg.Node.initUnaryExpression();
            uex->base.opType = op;

            var->expression = (Expression*) uex;
            var = Reg.Node.initVariable();

            token = Lex::nextToken(span, &tokenVal);

        }

        return token;

    }


    // the lower the rank, the higher precedence
    Lex::Token parseExpressionRecursive(Span* const span, Variable* var, BinaryExpression* prevBex, OperatorEnum prevOp, int lastDefIdx) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        // INFO: as for now only one such operator exists, its hardcoded
        //       basically this should work just fine while there is no binary
        //       operator with same signature as required closure
        if (prevOp == OP_SUBSCRIPT) {

            token = parseExpressionRecursive(span, var, prevBex, OP_NONE, lastDefIdx);

            if (token.kind == Lex::TK_ARRAY_END) {

                token = nextToken(span, &tokenVal);

            } else if (token.kind == Lex::TK_SLICE) {

                Slice* slice = Reg.Node.initSlice();

                slice->bidx = Reg.Node.initVariable();
                slice->bidx->expression = var->expression;

                slice->eidx = Reg.Node.initVariable();
                token = parseExpressionRecursive(span, slice->eidx, NULL, OP_NONE, lastDefIdx);

                var->expression = (Expression*) slice;

                if (token.kind != Lex::TK_ARRAY_END) {
                    tokenToErrorBuff(Lex::TK_ARRAY_END);
                    Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span, errBuff);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                token = nextToken(span, &tokenVal);

            } else {

                endTokenToErrorBuff({ Lex::TK_ARRAY_END, Lex::TK_SLICE });
                Logger::log(logErr, ERR_STR(Err::UNEXPECTED_SYMBOL), span, errBuff);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);

            }

        } else {
            token = parseExpressionNode(span, var->base.scope, var, lastDefIdx);
        }

        if (token.encoded < 0) return token;

        if (Lex::isOperator(token)) {

            OperatorEnum op = Lex::toBinaryOperator(token);

            while (op != OP_NONE) {

                if (prevOp == OP_NONE || operators[prevOp].rank > operators[op].rank) {

                    BinaryExpression* bex = Reg.Node.initBinaryExpression();
                    bex->left = Reg.Node.initVariable();
                    bex->right = Reg.Node.initVariable();
                    bex->base.opType = op;

                    bex->left->cvalue = var->cvalue;
                    //nullValue(var);

                    var->expression = (Expression*) bex;

                    if (prevBex) {
                        prevBex->right->expression = (Expression*) bex;
                        // prevOp = op;
                    }

                    token = parseExpressionRecursive(span, bex->right, bex, op, lastDefIdx);
                    if (token.encoded < 0) return token;

                    op = Lex::toBinaryOperator(token);
                    if (op < 0) return token;

                } else {

                    prevBex->right = var;
                    return Lex::toTokenAsBinaryOperator(op);

                }

            }

        } else {

            return token;

        }

        return token;

    }

    Lex::Token parseExpression(Span* const span, Variable* operand, const Pos startPos, const End endToken, const int param, const int defIdx) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        if (isValidPos(startPos)) span->end = startPos;

        token = parseExpressionRecursive(span, operand, NULL, OP_NONE, param);
        if (token.encoded < 0) return token;

        if (Lex::isKeyword(token, Lex::KW_CATCH)) {

            if (operand->expression->type != EXT_FUNCTION_CALL) {
                Logger::log(logErr, "Yet only 'pure' function call expressions are allowed to be caught 🐱.", span);
                return Lex::toToken(Err::UNEXPECTED_END_OF_EXPRESSION);
            }

            // TODO : add flag somethign like ALLOW_CATCH_TO_DISRESPECT_ENDING
            token = parseCatch(span, operand);
            if (token.kind == Lex::TK_SCOPE_END) return token;

        }

        if (
            isEndToken(token, endToken) ||
            (param & USE_KEYWORD_AS_END && token.kind == Lex::TK_KEYWORD) ||
            param & ALLOW_UNEXPECTED_END
        ) {
            return token;
        }

        Logger::log(logErr, "Blablbalba", span);
        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

}
