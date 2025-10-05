
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






// TODO : redo ASSIGN ID
// TODO : utilize String more
// TODO : make all bools and stuff as flags or something so it is known frmo the call whats even passing
// TODO : redo comments

// TODO : unexpected symbol/EOF show the last relevant (parsed) section
// TODO : 'auto res' to a function

#include <vector>

#include <stdlib.h>
#include <locale.h>
#include <stdio.h>
#include <string.h>

#include <string>
#include <wchar.h>

#include "globals.h"
#include "lexer.h"
#include "parser.h"
#include "syntax.h"
#include "file_driver.h"
#include "utils.h"
#include "logger.h"
#include "error.h"
#include <map>

#define ASSIGN_ID(var) {varId++; (var)->id = varId;}

namespace Parser {

    enum {
        USE_KEYWORD_AS_END = 0x1,
        EMPTY_EXPRESSION_ALLOWED = 0x2,
        ALLOW_QUALIFIER = 0x4,
        INCLUDE_TO_TREE = 0x8,
        ALLOW_UNEXPECTED_END = 0x10,
    };

    struct FullToken {
        Lex::Token token;
        Lex::TokenValue value;
    };

    struct End {
        Lex::TokenKind a;
        Lex::TokenKind b;
    };

    typedef uint64_t Flags;

    // Lex::Token parseScope(Span* const span, Scope* scope, const ScopeType global = SC_COMMON, const char endWithStatement = 0);
    Lex::Token parseForeignScope(Span* const span, Scope* scope);
    Lex::Token parseLabel(Span* const span, Scope* scope); // ✓
    Lex::Token parseKeywordStatement(Span* const span, Scope* scope, const Lex::Keyword keyword, Flags flags);
    Lex::Token parseDirective(Span* const span, Scope* scope, const Lex::Directive directive, Flags flags);
    Lex::Token parsePrintLiteral(Span* const span, Scope* const scope, Lex::TokenValue* const startTokenValue); // ✓
    Lex::Token parseVariableAssignment(Span* const span, Scope* scope, const Pos startPos, const End endToken); // ✓

    Lex::Token parseVariableDefinition(Span* const span, Scope* scope, FullToken prevToken, const End endToken, Flags flags, VariableDefinition** out);
    
    // TODO : maybe just remove 'known' version to simplify things
    Lex::Token parseDataType(Span* const span, Scope* scope, FullToken prevToken, Flags flags, VariableDefinition* def, Lex::TokenValue* outLastValue); // ✓
    Lex::Token parseKnownDataType(Span* const span, Scope* scope, DataTypeEnum dtype, String dtypeName, VariableDefinition* def, Lex::TokenValue* outLastValue); // ✓
    
    Lex::Token parseDataTypeDecorators(Span* const span, Scope* scope, Variable* var, Flags flags, Lex::TokenValue* outLastValue, Pointer** outLastPointer); // ✓

    Lex::Token parseBareStatement(Span* span, Scope* scope, const Pos startPos, const End endToken); // ✓

    Lex::Token parseLanguageTag(Span* span, String* tag);
    Lex::Token parseFunctionPointer(Span* const span, Scope* scope, FunctionPrototype** fcnOut); // ✓
    Lex::Token parseImport(Span* const span, Scope* sc);
    Lex::Token parseDefinitionAssignment(Span* const span, Scope* scope, FullToken prevToken, VariableDefinition* const def, const End endToken, int includeToScope);
    Lex::Token parseTypeInitialization(Span* const span, Scope* scope, TypeInitialization** outTypeInit);
    Lex::Token parseRValue(Span* const span, Scope* scope, Variable* outVar, const End endToken);
    Lex::Token parseIfStatement(Span* const span, Scope* const scope); // ✓
    Lex::Token parseSwitchStatement(Span* const span, Scope* const scope); // ✓
    Lex::Token parseForLoop(Span* const span, Scope* const scope); // ✓
    Lex::Token parseFunction(Span* const span, Scope* const scope, uint64_t param); // ✓
    Lex::Token parseWhileLoop(Span* const span, Scope* const scope); // ✓
    Lex::Token parseGotoStatement(Span* const span, Scope* const scope); // ✓
    Lex::Token parseReturnStatement(Span* const span, Scope* const scope); // ✓
    Lex::Token parseContinueStatement(Span* const span, Scope* const scope); // ✓
    Lex::Token parseBreakStatement(Span* const span, Scope* const scope); // ✓
    Lex::Token parseForEachLoop(Span* const span, Scope* const scope);
    Lex::Token parseNamespace(Span* const span, Scope* const scope);
    Lex::Token parseEnumDefinition(Span* const span, Scope* const scope); // ✓
    Lex::Token parseError(Span* const span, Scope* const scope);
    Lex::Token parseFreeStatement(Span* const span, Scope* const scope);
    Lex::Token parseTypeDefinition(Span* const span, Scope* const scope); // ✓
    Lex::Token parseArrayInitialization(Span* span, Scope* scope, ArrayInitialization** initOut);
    Lex::Token parseExpression(Span* const span, Variable* var, const Pos startPos, const End endToken, const int param = 0, const int defIdx = -1);
    Lex::Token parseAllocStatement(Span* const span, Scope* const scope);
    Lex::Token parseCatch(Span* const span, Variable* var);
    Lex::Token parseList(Span* const span, Lex::TokenKind separator, Lex::TokenKind end, std::vector<Variable*>& args);



    void copy(Variable* dest, Variable* src);
    void copy(Variable* dest, Variable* src);
    void copy(Scope* scA, Scope* scB);

    void appendScope(Scope* scA, Scope* scB);
    void appendPrefixScope(Scope* scA, Scope* scB);

    void stripWrapperExpressions(Variable** op);

    inline void offsetParentIdx(std::vector<SyntaxNode*> vec, const int offset);
    
    int isArrayLHS(char* const str, Span* span);

    inline int isEndToken(Lex::Token token, const End& end) {
        return token.kind == end.a || token.kind == end.b;
    }

    inline void insertVariableDefinition(Scope* scope, VariableDefinition* def) {
        scope->children.push_back(def);
        scope->defs.push_back(def->var);
    }

    inline void copyValue(Variable* dest, Variable* src) {
        dest->cvalue = src->cvalue;
        dest->expression = src->expression;
    }

    inline void nullValue(Variable* var) {
        var->cvalue = {};
        var->expression = NULL;
    }

    inline void assignQualifiedName(QualifiedName* dest, QualifiedName* src) {
        dest->name = src->name;
        dest->nameLen = src->nameLen;
        dest->path = src->path;
        dest->id = src->id;
    }



    std::string errBuff;

    inline void tokenToErrorBuff(Lex::TokenKind token) {
        errBuff += '\'';
        errBuff += Lex::toStr(token);
        errBuff += '\'';
    }

    // returns 1 if any token was added to the buffer
    inline int endTokenToErrorBuff(End end) {
        
        if (end.a != Lex::TK_NONE) {
            tokenToErrorBuff(end.a);
        }

        if (end.b != Lex::TK_NONE) {
            if (end.a != Lex::TK_NONE) {
                errBuff += ", ";
            }
            tokenToErrorBuff(end.b);
        }

        return (end.a != Lex::TK_NONE || end.b != Lex::TK_NONE);

    }



    // TODO : change when multi thread support will be added!!!
    //      used in ASSIGN_ID macro
    uint32_t varId = 0;
    // to assign each array/string initialization an id, so
    // render can easily create separate variable for them 
    uint32_t arrId = 0;

    uint64_t errId = 1;

    uint64_t defId = 0;
    



    // huh
    Function* currentFunction = NULL;
    SyntaxNode* currentLoop = NULL;
    Scope* fileRootScope = NULL; // maybe store it in Loacation






    // TODO : make it 'static' if its even possible
    // LOOK AT: move to the syntax.h/.c ?
    Function internalFunctions[] = {
        
        Function(
            SyntaxNode::root,
            (char *) IFS_PRINTF,
            sizeof(IFS_PRINTF) - 1,
            std::vector<VariableDefinition*>({
                new VariableDefinition(new Variable(SyntaxNode::root, DT_STRING), 0),
                new VariableDefinition(new Variable(SyntaxNode::root, DT_MULTIPLE_TYPES), 0)
            }),
            new VariableDefinition(new Variable(SyntaxNode::root, DT_VOID), 0),
            IF_PRINTF
        ),

        Function(
            SyntaxNode::root,
            (char *) IFS_ALLOC,
            sizeof(IFS_ALLOC) - 1,
            std::vector<VariableDefinition*>({
                new VariableDefinition(new Variable(SyntaxNode::root, DT_MULTIPLE_TYPES), 0)
            }),
            new VariableDefinition(new Variable(SyntaxNode::root, DT_POINTER), 0),
            IF_ALLOC
        ),

        Function(
            SyntaxNode::root,
            (char *) IFS_FREE,
            sizeof(IFS_FREE) - 1,
            std::vector<VariableDefinition*>({
                new VariableDefinition(new Variable(SyntaxNode::root, DT_POINTER), 0)
            }),
            new VariableDefinition(new Variable(SyntaxNode::root, DT_VOID), 0),
            IF_FREE
        ),
    
    };

    // import related stuff

    // to represent tree of imports
    // so we can check for circular imports and log user full path
    struct ImportNode {
        FileId* fileId;
        Scope* fileScope;
        ImportStatement* import;
        ImportNode* parent;
        std::vector<ImportNode*> children;
    };

    ImportNode* importRoot = new ImportNode;
    ImportNode* importCurrent = new ImportNode;
    std::map<FileId, Namespace*> parsedFiles;

    int areFileIdEqual(FileId* a, FileId* b) {
        return a->size == b->size && a->time == b->time;
    }

    int doesImportExistInPath(ImportNode* pathNode, ImportNode* checkNode) {
        
        ImportNode* node = pathNode;
        while (node) {
            if (areFileIdEqual(node->fileId, checkNode->fileId)) {
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
            Logger::log(Logger::PLAIN, " -> %.*s", NULL, node->import->fname.len, node->import->fname.buff);
        } else {
            Logger::log(Logger::PLAIN, " import path: MAIN FILE");
        }

    }

    // meh
    std::filesystem::path* getPath(String fpath, String fname) {
        std::string strA(fpath.buff, fpath.len);
        std::string strB(fname.buff, fname.len);
        return new std::filesystem::path(std::filesystem::path(strA) / std::filesystem::path(strB));
    }

    // meh
    FileId* genFileId(std::filesystem::path* const path) {
        
        if (!std::filesystem::exists(*path)) return NULL;

        FileId* id = new FileId;
            
        id->size = std::filesystem::file_size(*path);
        id->time = std::filesystem::last_write_time(*path);
        
        return id;
    
    }



    inline void setParentIdx(SyntaxNode* node) {
        node->parentIdx = node->scope->children.size();
    }




    int parseFile(char* const flname, ImportNode* import) {
        
        char* buffer;
        if (FileDriver::readFile(flname, &buffer)) {
            Logger::log(Logger::ERROR, "Failed to read file %s", NULL, flname);
            return Err::SYSTEM_COMMAND_EXECUTION_FAILED; // TODO : better error
        }

        std::filesystem::path absPath = std::filesystem::absolute({flname});
        
        Span span;
        span.file = new File { absPath, NULL, flname, buffer };
        span.str = buffer;
        span.start = { 0, 1 };
        span.end = { -1, 1 }; // TODO : i guess its better to add 'empty' character at the beginning of buffer, so non signed type can be used in the future
        
        importCurrent = import;

        // fileRootScope = fileScope ? fileScope : scope;
        return parseScope(&span, import->fileScope, SC_GLOBAL, 0).encoded;

    }

    int processImport(ImportNode* currentNode, String fpath) {

        // 1) process all children imports in current file
        for (int i = 0; i < currentNode->children.size(); i++) {
            
            ImportNode* const importNode = currentNode->children[i];
            ImportStatement* import = importNode->import;

            // maybe store also file path in ImportNode
            std::filesystem::path* filePath = getPath(fpath, import->fname);
            importNode->fileId = genFileId(filePath);
            if (!importNode->fileId) {
                Logger::log(Logger::ERROR, "File %.*s does not exists!", import->span, import->fname.len, import->fname.buff);
                return Err::FILE_DOES_NOT_EXISTS;
            }

            if (parsedFiles.find(*(importNode->fileId)) != parsedFiles.end()) {

                // LOOK AT: reusing already parsed stuff, for now as reference
                // may be copy will be needed
                importNode->fileScope = parsedFiles[*(importNode->fileId)];
            
            } else {
                
                // as namespace, so we can easier switch in the future
                importNode->fileScope = new Namespace;
                importNode->fileScope->type = NT_SCOPE;
                
                const int err = parseFile((char*) strdup(filePath->string().c_str()), importNode);
                if (err != Err::OK) {
                    printf("return err;\n");
                    return err;
                }

                // parsedFiles.insert(std::make_pair(*(importNode->fileId), (Namespace*) importNode->fileScope));
                parsedFiles.emplace(*(importNode->fileId), (Namespace*) importNode->fileScope);
            
            }

            if (doesImportExistInPath(currentNode, importNode)) {
                Logger::log(Logger::ERROR, ERR_STR(Err::CIRCULAR_IMPORT), import->span);
                logImportPath(importNode);
                return Err::CIRCULAR_IMPORT;
            }            
            
            Scope* root = importNode->parent->fileScope ? importNode->parent->fileScope : SyntaxNode::root;
            switch (import->keyWord) {
                
                case -1 : {

                    // import foo from file

                    int symbolType = -1;
                    SyntaxNode* symbol = NULL;

                    Namespace* nsc = (Namespace*) (importNode->fileScope);
                    for (int i = 0; i < nsc->children.size(); i++) {
                        
                        SyntaxNode* node = nsc->children[i];
                        if (node->type == NT_NAMESPACE) {
                            
                            Namespace* nspace = (Namespace*) node;
                            if (nspace->nameLen == import->param.len && strncmp(nspace->name, import->param.buff, import->param.len) == 0) {
                                symbolType = NT_NAMESPACE;
                                symbol = nspace;
                                break;
                            }

                        } else if (node->type == NT_FUNCTION) {
                            
                            Function* fcn = (Function*) node;
                            if (fcn->nameLen == import->param.len && strncmp(fcn->name, import->param.buff, import->param.len) == 0) {
                                symbolType = NT_FUNCTION;
                                symbol = fcn;
                                break;
                            }

                        }

                    }

                    if (symbolType < 0) {
                        Logger::log(Logger::ERROR, "Aprepritee symbol not found! Note, only namespaces and functions can be exported.", import->span);
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    switch (symbolType) {
                        
                        case NT_NAMESPACE : {

                            Namespace* sc = Utils::getCopy((Namespace*) symbol);

                            sc->scope = root;
                            sc->parentIdx = 0;
        
                            Utils::pushFornt<SyntaxNode*>(root->children, sc);
                            Utils::pushFornt<Namespace*>(root->namespaces, sc);
                            
                            break;

                        }

                        case NT_FUNCTION : {
                            
                            Function* fcn = Utils::getCopy((Function*) symbol);

                            fcn->scope = root;
                            fcn->parentIdx = 0;

                            Utils::pushFornt<SyntaxNode*>(root->children, fcn);
                            Utils::pushFornt<Function*>(root->fcns, fcn); 
                            
                            break;

                        }
                    
                        default:
                            Logger::log(Logger::ERROR, "Aprepritee symbol not found! Note, only namespaces and functions can be exported.", import->span);
                            return Err::UNEXPECTED_SYMBOL;
                    
                    }

                    break;
                
                }

                case KW_SCOPE : {

                    Scope* sc = (Namespace*) (importNode->fileScope);

                    sc->scope = root;
                    sc->type = NT_SCOPE;
                    sc->parentIdx = 0;

                    Utils::pushFornt<SyntaxNode*>(root->children, sc);

                    break;

                }

                case KW_FUNCTION : {

                    Function* fcn = new Function();
                    
                    fcn->bodyScope = (Namespace*) (importNode->fileScope);
                    fcn->name = import->param;
                    fcn->nameLen = import->param.len;
                    fcn->scope = root;
                    fcn->type = NT_FUNCTION;
                    fcn->outArg = Utils::createEmptyVariableDefinition();
                    fcn->outArg->var->cvalue.dtypeEnum = DT_VOID;
                    fcn->parentIdx = 0;
                    fcn->snFlags = 0;

                    ASSIGN_ID(fcn);

                    // as we importing function, order doesn't matter, so we can push it back
                    root->children.push_back(fcn);
                    root->fcns.push_back(fcn);

                    break;

                }

                case KW_NAMESPACE : {

                    Namespace* sc = (Namespace*) (importNode->fileScope);

                    sc->name = import->param;
                    sc->nameLen = import->param.len;
                    sc->scope = root;
                    sc->type = NT_NAMESPACE;
                    sc->parentIdx = 0;
                    sc->snFlags = 0;

                    Utils::pushFornt<SyntaxNode*>(root->children, sc);
                    Utils::pushFornt<Namespace*>(root->namespaces, sc);
                    
                    // in case of namespace we dont need to update parentIdx for searchDefs
                    // but may be wrong..

                    break;

                }

            }

        }

        // 2) process new imports of each children
        for (int i = 0; i < currentNode->children.size(); i++) {
            ImportNode* node = currentNode->children[i];
            const int err = processImport(node, fpath);
            if (err != Err::OK) return err;
        }

        return Err::OK;

    }

    int parse(char* const flname) {

        // NOTE : future parallelism in mind

        int err;

        char* dirStr = flname;
        uint64_t dirLen = Utils::stripDir(flname);

        Logger::log(Logger::INFO, "Parsing...\n");

        SyntaxNode::root = new Scope;
        SyntaxNode::root->fcn = NULL;
        SyntaxNode::root->scope = NULL;
        SyntaxNode::root->parentIdx = 0;
        SyntaxNode::dir = new INamed(dirStr, dirLen);

        std::filesystem::path flpath(flname);
        importRoot->fileId = genFileId(&flpath);
        if (!importRoot->fileId) {
            Logger::log(Logger::ERROR, ERR_STR(Err::FILE_DOES_NOT_EXISTS), NULL, flname);
            return Err::FILE_DOES_NOT_EXISTS;
        }

        importRoot->fileScope = SyntaxNode::root;
        importRoot->import = NULL;
        importRoot->parent = NULL;

        internalFunctionUsed = 0;

        err = parseFile(flname, importRoot);
        if (err != Err::OK) return err;

        err = processImport(importRoot, { dirStr, dirLen });
        if (err != Err::OK) return err;

        return 0;
    
    }




























    Lex::Token parseScope(Span* const span, Scope* scope, const ScopeType scopeType, const char endAsStatement) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;

        Scope* node;

        if (scopeType == SC_COMMON) {
            node = new Scope;
            node->scope = scope;
            setParentIdx(node);
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
                        node->span = finalizeSpan(&lspan, span);
                        return Lex::toToken(Err::OK);
                    }
                    
                    span->end = prevPos;
                    
                    Logger::log(Logger::ERROR, "Unexpected end of file! Showing the start of the relevant section.", &lspan);
                    return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
                
                }

                case Lex::TK_SCOPE_BEGIN : {

                    const Lex::Token err = parseScope(&lspan, node, SC_COMMON, 0);
                    if (err.encoded < 0) return err;
                    break;

                }
                
                case Lex::TK_SCOPE_END : {

                    if (scopeType != SC_GLOBAL) {
                        node->span = finalizeSpan(&lspan, span);
                        scope->children.push_back(node);
                        return token;
                    }

                    // ERROR
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, "");
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

            if (endAsStatement) {
                if (token.kind == Lex::TK_STATEMENT_END) {
                    finalizeSpan(&lspan, span);
                    return token;
                }
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
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

        internalFunctionUsed = internalFunctionUsed | (1 << (IF_PRINTF - 1));

        Function* const fcn = internalFunctions + (IF_PRINTF - 1);

        FunctionCall* fcnCall = new FunctionCall();
        fcnCall->fcn = fcn;
        fcnCall->name = fcn->name;
        fcnCall->nameLen = fcn->nameLen;

        Variable* format = new Variable(scope, DT_STRING, &lspan);
        format->cvalue.any = startTokenValue->any;

        fcnCall->inArgs.push_back(format);

        Variable* callWrapper = new Variable();
        callWrapper->scope = scope;
        callWrapper->expression = fcnCall;
        callWrapper->cvalue.str = NULL;

        scope->children.push_back(callWrapper);

        NodeRegistry::fcnCalls.push_back(callWrapper);

        token = parseList(&lspan, Lex::TK_LIST_SEPARATOR, Lex::TK_STATEMENT_END, fcnCall->inArgs);
        if (token.encoded < 0) return token;

        callWrapper->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseLanguageTag(Span* span, String* tag) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(span, &tokenVal);
        QualifiedName* qname = (QualifiedName*) tokenVal.any;

        if (token.kind != Lex::TK_IDENTIFIER) {
            delete qname;
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (qname->path.size() != 0) {
            Logger::log(Logger::ERROR, "Language tag cannot be a qualified name!", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        tag->buff = qname->name;
        tag->len = qname->nameLen;

        delete qname;

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind != Lex::TK_ARRAY_END) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
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
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        char* const codeStr = span->str + span->end.idx;
        const int codeLen = Lex::findBlockEnd(&lspan, Lex::SCOPE_BEGIN, Lex::SCOPE_END);
        if (codeLen < 0) {
            // ERROR
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE), span);
            return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
        }

        CodeBlock* codeBlock = new CodeBlock();
        codeBlock->tagStr = tag.buff;
        codeBlock->tagLen = tag.len;
        codeBlock->codeStr = codeStr;
        codeBlock->codeLen = codeLen;

        NodeRegistry::codeBlocks.push_back(codeBlock);

        codeBlock->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseLabel(Span* span, Scope* scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(&lspan, &tokenVal);

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, "Identifier");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        Label* label = new Label();
        label->scope = scope;
        label->name = tokenVal.str->buff;
        label->nameLen = tokenVal.str->len;
        setParentIdx(label);

        scope->children.push_back(label);
        scope->labels.push_back(label);
        NodeRegistry::labels.push_back(label);

        auto res = scope->defSearch.insert({std::string_view(label->name, label->nameLen), label});
        if (!res.second) {
            Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), &lspan);
            return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
        }

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_LABEL_END) {
            uint64_t val = Lex::toIntStr(Lex::LABEL_END);
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, &val);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        label->span = finalizeSpan(&lspan, span);
        return token;

    }

    // assignment or expression
    Lex::Token parseBareStatement(Span* span, Scope* scope, const Pos startPos, const End endToken) {
        
        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        
        Variable* lvar = new Variable();
        token = parseExpression(&lspan, lvar, startPos, endToken, ALLOW_UNEXPECTED_END);
        if (token.encoded < 0) return token;
        
        if (isEndToken(token, endToken)) {
            
            Statement* stmt = new Statement();
            stmt->scope = scope;
            stmt->op = lvar;

            scope->children.push_back(stmt);
            NodeRegistry::statements.push_back(stmt);
            
            stmt->span = finalizeSpan(&lspan, span);
            return token;
        
        }

        if (token.kind != Lex::TK_EQUAL) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, "'='");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        };

        Variable* rvar = new Variable();
        token = parseRValue(&lspan, scope, rvar, endToken);
        if (token.encoded < 0) return token;

        VariableAssignment* varAssignment = new VariableAssignment(span);
        varAssignment->scope = scope;
        varAssignment->lvar = lvar;
        varAssignment->rvar = rvar;

        // TODO : move to parseRValue?
        if (lvar->cvalue.dtypeEnum == DT_ARRAY) {
            NodeRegistry::arraysAllocations.push_back(varAssignment);
        }

        NodeRegistry::variableAssignments.push_back(varAssignment);
        scope->children.push_back(varAssignment);

        varAssignment->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseVariableAssignment(Span* span, Scope* scope, const Pos startPos, const End endToken) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        VariableAssignment* const varAssignment = new VariableAssignment(span);
        varAssignment->scope = scope;
        varAssignment->lvar = new Variable(scope);
        varAssignment->rvar = new Variable(scope);

        token = parseExpression(&lspan, varAssignment->lvar, startPos, End { Lex::TK_EQUAL });
        if (token.encoded < 0) return token;

        if (token.kind != Lex::TK_EQUAL) {
            uint64_t val = Lex::toIntStr(Lex::EQUAL);
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, &val);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = parseRValue(&lspan, scope, varAssignment->rvar, endToken);
        if (token.encoded < 0) return token;

        // TODO : move to parseRValue?
        if (varAssignment->lvar->cvalue.dtypeEnum == DT_ARRAY) {
            NodeRegistry::arraysAllocations.push_back(varAssignment);
        }

        NodeRegistry::variableAssignments.push_back(varAssignment);
        scope->children.push_back(varAssignment);

        varAssignment->span = finalizeSpan(&lspan, span);
        return token;

    }

    /*
    int parseVariableDefinition(Span* const span, Span* const startLoc) {

        VariableDefinition* varDef = new VariableDefinition(startLoc);

        varDef->dtype = new QualifiedName();
        parseScopeNames(varDef->dtype, str, startLoc);
        varDef->span->idx -= varDef->dtype->nameLen + 1;

        err = processDataType(DT_CUSTOM, scope, str, span, 0, STATEMENT_END, &varDef, 1, 0);
        if (err < 0) return err;

        NodeRegistry::customDataTypesReferences.push_back(varDef);

    }
    */
    
    Lex::Token parseKnownDataType(Span* const span, Scope* scope, DataTypeEnum dtype, String dtypeName, VariableDefinition* def, Lex::TokenValue* outLastValue) {

        Lex::Token token;

        if (dtype == DT_UNDEFINED) {
            // dt_custom case

            def->var->cvalue.dtypeEnum = DT_CUSTOM;
            def->var->cvalue.any = NULL;
            def->dtype = new QualifiedName();
            def->dtype->name = dtypeName.buff;
            def->dtype->nameLen = dtypeName.len;

            NodeRegistry::customDataTypesReferences.push_back(def);
        
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
        def->var->span = getSpanStamp(span);

        return token;

    }

    // def->flags will be rewritten
    Lex::Token parseDataType(Span* span, Scope* scope, FullToken prevToken, Flags flags, VariableDefinition* def, Lex::TokenValue* outLastValue) {

        Lex::Token token = prevToken.token;
        Lex::TokenValue tokenVal = prevToken.value;

        def->span = getSpanStamp(span);

        if (token.kind == Lex::TK_KEYWORD && (token.detail == Lex::KW_CONST || token.detail == Lex::KW_EMBED)) {
            
            if (!(flags & ALLOW_QUALIFIER)) {
                Logger::log(Logger::ERROR, "Qualifier not expected here!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            def->flags = ((token.kind == KW_CONST) ? KW_CONST : KW_CMP_TIME);
            token = Lex::nextToken(span, &tokenVal);

        } else {
            def->flags = 0;
        }

        def->var = new Variable(scope);
        def->var->def = def;
        def->var->unrollExpression = 0;
        def->var->expression = NULL;
        def->scope = scope;

        if (token.kind == Lex::TK_IDENTIFIER) {
            
            token = parseKnownDataType(span, scope, DT_CUSTOM, *tokenVal.str, def, outLastValue);

        } else {
            
            if (!Lex::isDtype(token)) {
                Logger::log(Logger::ERROR, "Data type expected!", span);
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
        FunctionPrototype* fcn = new FunctionPrototype;

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_PARENTHESIS_BEGIN) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);            
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }
        
        token = Lex::nextToken(&lspan, NULL);
        if (token.kind == Lex::TK_ARROW) {
            goto fOutArgs;
        }

        while (1) {

            def = new VariableDefinition();
            fcn->inArgs.push_back(def);

            token = parseDataType(&lspan, scope, { token, tokenVal }, ALLOW_QUALIFIER, def, &tokenVal);
            if (token.encoded < 0) return token;
            
            if (token.kind == Lex::TK_LIST_SEPARATOR) {
                token = Lex::nextToken(&lspan, NULL);
                continue;
            } else if (token.kind == Lex::TK_ARROW) {
                break;
            } else {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        }

        fOutArgs:

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_PARENTHESIS_END) {
            
            def = new VariableDefinition();
            token = parseDataType(&lspan, scope, { token, tokenVal }, NULL_FLAG, def, &tokenVal);
            if (token.encoded < 0) return token;

            if (token.kind != Lex::TK_PARENTHESIS_END) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        } else {

            def = Utils::createEmptyVariableDefinition();
        
        }

        fcn->outArg = def;
        *fcnOut = fcn;

        def->span = finalizeSpan(&lspan, span);
        return token;

    }

    // part starting with variable name in definition
    // ex: const int^ x ... from x
    Lex::Token parseDefinitionAssignment(Span* const span, Scope* scope, FullToken prevToken, VariableDefinition* const def, const End endToken, int includeToScope) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        QualifiedName* qname = (QualifiedName*) prevToken.value.any;
        def->var->name = qname->name;
        def->var->nameLen = qname->nameLen;
        def->var->path = qname->path;
        
        ASSIGN_ID(def->var);

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind == Lex::TK_EQUAL) {

            token = parseRValue(span, scope, def->var, endToken);
            if (token.encoded < 0) return token;

            if (def->flags & IS_CMP_TIME) NodeRegistry::cmpTimeVars.push_back(def->var);
            else NodeRegistry::initializations.push_back(def);

        } else if (token.kind == endToken.a || token.kind == endToken.b) {

            def->var->expression = NULL;
        
        } else {
            
            errBuff.clear();
            if (endTokenToErrorBuff(endToken)) {
                errBuff += ", ";
            }
            tokenToErrorBuff(Lex::TK_EQUAL);

            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span, errBuff.c_str());
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        
        }

        if (includeToScope) {
            //scope->children.push_back(def);
            // pushDefLike(scope->defSearch, def->var);
            
            setParentIdx(def->var);

            auto res = scope->defSearch.insert({std::string_view(def->var->name, def->var->nameLen), def->var});
            if (!res.second) {
                Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), def->var->span);
                return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
            }

            scope->children.push_back(def);
            NodeRegistry::variableDefinitions.push_back(def);
            scope->defs.push_back(def->var);
        }

        return token;

    }
    
    Lex::Token parseVariableDefinition(Span* const span, Scope* scope, FullToken prevToken, const End endToken, Flags param, VariableDefinition** outVarDef) {       

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        VariableDefinition* def = new VariableDefinition();
        
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
        
        def->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseStringLiteral(Span* const span, String* str, StringInitialization** initOut) {

        Lex::Token token = Lex::nextToken(span, NULL);
        const int rawStringRequired = token.kind == Lex::TK_RAW ? 1 : 0;

        StringInitialization* init = new StringInitialization;
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
            char* utf8Str = Utils::encodeUtf8(init->rawPtr, init->rawPtrLen, &utf8Len, &utf8BytesPerChar);
            
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
        *initOut = new ArrayInitialization();

        token = Lex::nextToken(span);

        while (1) {

            Variable* var = new Variable();
            var->scope = scope;

            token = parseExpression(span, var, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_ARRAY_END });
            if (token.kind < 0) return token;

            (*initOut)->attributes.push_back(var);

            if (token.kind == Lex::TK_ARRAY_END) break;
            if (token.kind != Lex::TK_LIST_SEPARATOR) {
                uint64_t tmp = Lex::toIntStr(Lex::LIST_SEPARATOR);
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span, &tmp);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        }

        return token;
    
    }

    Lex::Token parseTypeInitialization(Span* const span, Scope* scope, TypeInitialization** outTypeInit) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        TypeInitialization* dtypeInit = new TypeInitialization();

        token = Lex::nextToken(&lspan, &tokenVal);
        Lex::Token nextToken = Lex::peekToken(&lspan);

        if (nextToken.kind == Lex::TK_STATEMENT_BEGIN || token.kind == Lex::TK_THE_REST) {
            // names assumed

            int hasFillVar = 0;
            dtypeInit->fillVar = NULL;

            while (1) {

                Variable* var = new Variable(scope);
                var->span = getSpanStamp(&lspan);

                if (token.kind == Lex::TK_THE_REST) {
                    
                    if (hasFillVar) {
                        Logger::log(Logger::ERROR, "Fill the rest symbol can be used only once per initialization!", span);
                        return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                    }

                    dtypeInit->fillVar = var;
                    hasFillVar = 1;

                } else if (token.kind == Lex::TK_IDENTIFIER) {

                    var->name = tokenVal.str->buff;
                    var->nameLen = tokenVal.str->len;
                    dtypeInit->attributes.push_back(var);
                
                } else {
                    
                    // ERROR
                    Logger::log(Logger::ERROR, "Attribute name expected!", span);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                
                }

                token = Lex::nextToken(&lspan, NULL);
                if (token.kind != Lex::TK_STATEMENT_BEGIN) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, Lex::toStr(Lex::TK_STATEMENT_BEGIN));
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                token = parseExpression(&lspan, var, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_SCOPE_END });
                if (token.encoded < 0) return token;

                if (token.kind == Lex::TK_SCOPE_END) break;
                token = Lex::nextToken(&lspan, &tokenVal);

            }

        } else {

            while (1) {
                
                Variable* var = new Variable();

                token = parseExpression(&lspan, var, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_SCOPE_END });
                if (token.encoded < 0) return token;

                dtypeInit->attributes.push_back(var);

                if (token.kind == Lex::TK_SCOPE_END) break;
                
            }

        }

        dtypeInit->idxs = (int*) malloc(dtypeInit->attributes.size() * sizeof(int));
        if (!dtypeInit->idxs) {
            Logger::log(Logger::ERROR, Err::str[-Err::MALLOC]);
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
            mainDtype = outVar->cvalue.arr->pointsToEnum;
        }

        Pos startPos = span->start;

        token = Lex::nextToken(span, NULL);
        if (Lex::isKeyword(token, Lex::KW_ALLOC)) {
        
            Pos startPos = span->end;

            Pointer* lastPtr = NULL;

            VariableDefinition* varDef = new VariableDefinition();
            varDef->scope = scope;
            varDef->span = getSpanStamp(span);

            Variable* var = new Variable();
            var->def = varDef;

            varDef->var = var;

            token = Lex::nextToken(span, &tokenVal);
            if (token.kind == Lex::TK_IDENTIFIER) {
                
                var->cvalue.dtypeEnum = DT_CUSTOM;
                var->cvalue.any = NULL;

                varDef->dtype = new QualifiedName();
                varDef->dtype->name = tokenVal.str->buff;
                varDef->dtype->nameLen = tokenVal.str->len;
            
            } else if (token.kind == Lex::TK_KEYWORD) {
                
                if (!Lex::isDtype(token)) {
                    Logger::log(Logger::ERROR, "TODO : data type expected, no place for generic keyword!");
                    return Lex::toToken(Err::INVALID_DATA_TYPE);
                }

                var->cvalue.dtypeEnum = Lex::toDtype(token);
                var->cvalue.any = dataTypes + Lex::toDtype(token);

            } else if (mainDtype <= 0) {

                Logger::log(Logger::ERROR, "TODO : error parseRValue alloc requires dtype name! Can be omitted only in definition!");
                return Lex::toToken(Err::INVALID_DATA_TYPE);
            
            } else {
                
                var->cvalue.dtypeEnum = mainDtype;
                var->cvalue.any = mainDtype == DT_CUSTOM ? NULL : dataTypes + mainDtype;

                if (outVar->def) {
                    varDef->dtype = new QualifiedName();
                    varDef->dtype = outVar->def->dtype;
                }

                span->end = startPos;
            
            }

            Function* const fcn = internalFunctions + (IF_ALLOC - 1);

            FunctionCall* fcnCall = new FunctionCall();
            fcnCall->fptr = NULL;
            fcnCall->fcn = fcn;
            fcnCall->name = fcn->name;
            fcnCall->nameLen = fcn->nameLen;
            fcnCall->outArg = new Variable();
            fcnCall->outArg->cvalue.dtypeEnum = DT_POINTER;

            token = parseDataTypeDecorators(span, scope, var, ALLOW_QUALIFIER, &tokenVal, &lastPtr);
            varDef->lastPtr = lastPtr;

            if (token.kind == Lex::TK_STATEMENT_BEGIN) {
                token = parseExpression(span, var, INVALID_POS, endToken);
                if (token.encoded < 0) return token;
            } else if (token.kind != Lex::TK_STATEMENT_END) {
                Logger::log(Logger::ERROR, "TODO error: parseRValue unexpected symbol!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }
            
            fcnCall->inArgs.push_back(var);

            outVar->expression = fcnCall;
            NodeRegistry::fcnCalls.push_back(outVar);
            // initializations.push_back(varDef);
            
            // ...
            NodeRegistry::customDataTypesReferences.push_back(varDef);

            outVar->snFlags |= IS_ALLOCATION;

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
                    Logger::log(Logger::ERROR, "Pointer can't be used after array declaration!", span);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }
            
                Pointer* ptr = new Pointer();
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
                    Logger::log(Logger::ERROR, "Multidimensional arrays not allowed!", span);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                wasArray = 1;

                Array* arr = new Array;
                if (!mainPtr) *outLastPointer = arr;
                else mainPtr->parentPointer = arr;

                arr->pointsTo = var->cvalue.any;
                arr->pointsToEnum = var->cvalue.dtypeEnum;

                Variable* lenVar = new Variable(span);
                lenVar->scope = scope;
                lenVar->nameLen = 0;
                
                token = Lex::nextToken(span, &tokenVal);
                if (token.kind == Lex::TK_KEYWORD) {

                    const int keyword = token.detail;

                    token = Lex::nextToken(span, NULL);
                    if (token.kind != Lex::TK_ARRAY_END) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
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
                var->flags = 0;

                if (flags & INCLUDE_TO_TREE) NodeRegistry::arrays.push_back(var);

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

        }

        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

    // basic data types only
    /*
    int parseBasicVariableDefinition(Span* const span, Scope* const scope, const DataTypeEnum dtype, 
        const uint64_t param, 
        const uint16_t endChar,
        VariableDefinition** outVarDef,
        const int include,
        const int alloc
    ) {

        Lex::Token token;

        VariableDefinition* def;
        if (alloc) {
            def = new VariableDefinition(span);
        } else {
            def = *outVarDef;
        }
        
        def->var = new Variable(scope);
        def->var->def = def;
        def->var->cvalue.dtypeEnum = dtype;
        def->var->cvalue.any = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;
        def->var->unrollExpression = 0;
        def->var->expression = NULL;
        def->flags = param;
        def->scope = scope;

        Pointer* lastPtr = NULL;
        token = parseDataTypeDecorators(span, scope, def->var, &lastPtr, 1);
        if (token.encoded < 0) return token;

        def->lastPtr = lastPtr;
        def->var->span = getSpanStamp(span);

        token = parseDefinitionAssignment(span, scope, def, endChar, include);
        if (token.encoded < 0) return token;

        if (outVarDef) *outVarDef = def;
        return Err::OK;

    }
    */

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
            auto res = scope->defSearch.insert({std::string_view(def->var->name, def->var->nameLen), def->var});
            if (!res.second) {
                Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), def->var->span);
                return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
            }

            setParentIdx(def->var);

            scope->children.push_back(def);
            NodeRegistry::variableDefinitions.push_back(def);
            scope->defs.push_back(def->var);

            span->end = lspan.end;
            return token;
        
        }

        Function* fcn;

        Scope* newScope = new Scope();
        newScope->fcn = currentFunction;
        newScope->scope = scope;
        setParentIdx(newScope);

        int foreignLang = 0;
        if (token.kind == Lex::TK_ARRAY_BEGIN) {
                
            foreignLang = 1;
            fcn = new ForeignFunction();

            Span* tagLoc = getSpanStamp(&lspan);
            
            String tag;
            token = parseLanguageTag(&lspan, &tag);
            if (token.encoded < 0) return token;

            ((ForeignFunction*) fcn)->tagLen = tag.len;
            ((ForeignFunction*) fcn)->tagStr = tag.buff;
            ((ForeignFunction*) fcn)->tagLoc = tagLoc;
            
            ASSIGN_ID(fcn);

            token = nextToken(&lspan, &tokenVal);

        } else {
            
            fcn = new Function();
        
        }

        fcn->inArgsCnt = 0;
        fcn->scope = scope;
        fcn->errorSetName = NULL;
        fcn->outArg = Utils::createEmptyVariableDefinition();

        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(Logger::ERROR, "Function name expected!", &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        fcn->name = tokenVal.str->buff;
        fcn->nameLen = tokenVal.str->len;

        ASSIGN_ID(fcn);

        token = Lex::nextToken(&lspan);
        if (token.kind != Lex::TK_PARENTHESIS_BEGIN) {

            if (token.kind == Lex::TK_SCOPE_BEGIN) goto fcnParseScope;

            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        
        }

        // parse input
        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_PARENTHESIS_END) break;

            fcn->inArgsCnt++;

            VariableDefinition* varDef;
            token = parseVariableDefinition(&lspan, scope, { token, tokenVal }, End { Lex::TK_PARENTHESIS_END, Lex::TK_LIST_SEPARATOR }, param, &varDef);
            if (token.encoded < 0) return token;

            // :)
            varDef->var->parentIdx = -1;
            varDef->parentIdx = -1;

            auto res = newScope->defSearch.insert({std::string_view(varDef->var->name, varDef->var->nameLen), varDef->var});
            if (!res.second) {
                Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), varDef->var->span);
                return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
            }
            
            NodeRegistry::variableDefinitions.push_back(varDef);
            newScope->defs.push_back(varDef->var);

            fcn->inArgs.push_back(varDef);

            if (token.kind == Lex::TK_PARENTHESIS_END) break;
        
        }

        // [using 'Error Set']
        token = Lex::nextToken(&lspan, &tokenVal);
        if (Lex::isKeyword(token, Lex::KW_USING)) {

            fcn->errorSetName = new QualifiedName();
            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.encoded < 0) return token;

            Using* tmp = new Using();
            tmp->var = fcn;
            
            newScope->usings.push_back(tmp);

            token = Lex::nextToken(&lspan, &tokenVal);

        }

        // ->
        if (token.kind != Lex::TK_ARROW) {
            
            if (token.kind == Lex::TK_SCOPE_BEGIN) goto fcnParseScope;

            // LOOK AT : maybe better name for this situation, something like "unexpected char sequence"
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            Logger::log(Logger::HINT, "'->' expected\n");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        // parse output
        token = Lex::nextToken(&lspan, &tokenVal);
        token = parseDataType(&lspan, scope, { token, tokenVal }, NULL_FLAG, fcn->outArg, &tokenVal);
        if (token.encoded < 0) return token;

        // scope
        if (token.kind != Lex::TK_SCOPE_BEGIN && token.kind != Lex::TK_STATEMENT_BEGIN) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        fcnParseScope:
        
        if (foreignLang) {
            
            ForeignFunction* ffcn = (ForeignFunction*) fcn;
            
            ffcn->codeStr = lspan.str + lspan.start.idx;
            ffcn->codeLen = Lex::findBlockEnd(&lspan, Lex::SCOPE_BEGIN, Lex::SCOPE_END);
            if (ffcn->codeLen < 0) {
                return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
            }

            ffcn->internalIdx = -3443431;
            scope->fcns.push_back(ffcn);
            NodeRegistry::foreignFunctions.push_back(ffcn);

            ffcn->span = finalizeSpan(&lspan, span);
            return token;
        
        }

        fcn->internalIdx = 0;

        //Scope* newScope = new Scope;
        //newScope->fcn = currentFunction;
        //newScope->scope = outerScope;

        currentFunction = fcn;
        token = parseScope(&lspan, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
        if (token.kind < 0) return token;
        currentFunction = NULL;

        fcn->bodyScope = newScope;

        scope->children.push_back(fcn);
        scope->fcns.push_back(fcn);
        NodeRegistry::fcns.push_back(fcn);

        fcn->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseIfStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;
        const int parentIdx = scope->children.size();

        Scope* newScope = new Scope();
        newScope->fcn = currentFunction;
        newScope->scope = scope;
        newScope->parentIdx = parentIdx;

        Variable* newOperand = new Variable(scope);
        token = parseExpression(&lspan, newOperand, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
        if (token.encoded < 0) return token;
        
        token = parseScope(&lspan, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
        if (token.encoded < 0) return token;

        Branch* branch = new Branch();
        branch->scope = scope;
        branch->scopes.push_back(newScope);
        branch->expressions.push_back(newOperand);

        scope->children.push_back(branch);
        NodeRegistry::branchExpressions.push_back(newOperand);

        while (1) {

            token = Lex::tryKeyword(&lspan, Lex::KW_ELSE);
            if (token.kind != Lex::TK_KEYWORD) {
                branch->span = finalizeSpan(&lspan, span);
                return token;
            }
            
            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_BEGIN || token.kind == Lex::TK_STATEMENT_BEGIN) {
                // else case

                Scope* newScope = new Scope();
                newScope->fcn = currentFunction;
                newScope->scope = scope;
                newScope->parentIdx = parentIdx;

                token = parseScope(&lspan, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
                if (token.encoded < 0) return token;

                branch->scopes.push_back(newScope);

                branch->span = finalizeSpan(&lspan, span);
                return token;

            }

            // else if case
            
            if (!Lex::isKeyword(token, Lex::KW_IF)) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, "if, ':' or '{'");
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);                
            }

            Variable* newOperand = new Variable(scope);
            newOperand->span = getSpanStamp(&lspan);

            token = parseExpression(&lspan, newOperand, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
            if (token.encoded < 0) return token;

            Scope *newScope = new Scope();
            newScope->fcn = currentFunction;
            newScope->scope = scope;
            newScope->parentIdx = parentIdx;

            token = parseScope(&lspan, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
            if (token.encoded < 0) return token;

            branch->scopes.push_back(newScope);
            branch->expressions.push_back(newOperand);
            NodeRegistry::branchExpressions.push_back(newOperand);
        
        }

    }

    Lex::Token parseSwitchStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);
    
        Lex::TokenValue tokenVal;
        Lex::Token token;

        SwitchCase* switchCase = new SwitchCase;
        switchCase->scope = scope;
        switchCase->elseCase = NULL;

        Variable* var = new Variable;
        var->scope = scope;

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
                cmpExp = new Variable();
                cmpExp->scope = scope;
                token = parseExpression(&lspan, cmpExp, INVALID_POS, End { Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN });
                if (token.encoded < 0) return token;
            } else {
                if (!atLeastOneCase) {
                    Logger::log(Logger::ERROR, "Else case can't be the first case!", &lspan);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }
                token = Lex::nextToken(&lspan);
            }

            Scope* sc = new Scope;
            sc->fcn = currentFunction;
            sc->scope = scope;
            setParentIdx(sc);

            if (token.kind == Lex::TK_STATEMENT_BEGIN) {
                parseScope(&lspan, sc, SC_COMMON, 1); 
            } else if (token.kind == Lex::TK_SCOPE_BEGIN) {
                parseScope(&lspan, sc);
            } else {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            if (elseCase) {
                switchCase->elseCase = sc;
                break;
            }

            switchCase->casesExp.push_back(cmpExp);
            switchCase->cases.push_back(sc);

        }

        NodeRegistry::switchCases.push_back(switchCase);
        scope->children.push_back(switchCase);

        switchCase->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseForLoop(Span* const span, Scope* const scope) {
        
        SpanEx lspan = markSpanStart(span);

        // TODO : something about empty expressions, maybe make them null or something...
        Lex::Token token;
        Lex::TokenValue tokenVal;

        ForLoop* loop = new ForLoop();

        Scope* outerScope = new Scope();
        outerScope->fcn = currentFunction;
        outerScope->scope = scope;
        setParentIdx(outerScope);

        Variable* initEx = new Variable(outerScope);

        // can be either variable initialization or expression
        Pos startPos = lspan.end;
        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind == Lex::TK_KEYWORD && Lex::isDtype(token)) {
            token = parseVariableDefinition(&lspan, scope, { token, tokenVal }, End { Lex::TK_STATEMENT_END }, NULL_FLAG, NULL);
        } else {
            token = parseExpression(&lspan, initEx, startPos, End { Lex::TK_STATEMENT_END }, EMPTY_EXPRESSION_ALLOWED);
        }
        if (token.encoded < 0) return token;

        Scope* bodyScope = new Scope();
        bodyScope->fcn = currentFunction;
        bodyScope->scope = outerScope;
        setParentIdx(bodyScope);

        Variable* conditionEx = new Variable(outerScope);

        token = parseExpression(&lspan, conditionEx, INVALID_POS, End { Lex::TK_STATEMENT_END }, EMPTY_EXPRESSION_ALLOWED);
        if (token.kind < 0) return token;

        // can be assignment
        Variable* actionEx = new Variable(outerScope);

        token = parseExpression(&lspan, actionEx, INVALID_POS, End { Lex::TK_SCOPE_BEGIN, Lex::TK_STATEMENT_BEGIN }, EMPTY_EXPRESSION_ALLOWED);
        if (token.encoded < 0) return token;

        SyntaxNode* tmpLoop = currentLoop;
        currentLoop = loop;

        if (token.kind == Lex::TK_SCOPE_BEGIN || token.kind == Lex::TK_STATEMENT_BEGIN) {
            token = parseScope(&lspan, bodyScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
            if (token.kind < 0) return token;
        } else {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        currentLoop = tmpLoop;

        loop->scope = scope;
        loop->bodyScope = bodyScope;
        loop->initEx = initEx;
        loop->conditionEx = conditionEx;
        loop->actionEx = actionEx;

        outerScope->children.push_back(loop);
        scope->children.push_back(outerScope);

        if (conditionEx->expression) {
            NodeRegistry::branchExpressions.push_back(conditionEx);
        } else {
            loop->conditionEx = NULL;
            freeSpanStamp(conditionEx->span);
            delete conditionEx;
        }

        loop->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseWhileLoop(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        WhileLoop* loop = new WhileLoop();

        Scope* newScope = new Scope();
        newScope->fcn = currentFunction;
        newScope->scope = scope;
        setParentIdx(newScope);

        Variable* newOperand = new Variable(newScope);

        token = parseExpression(&lspan, newOperand, INVALID_POS, End { Lex::TK_SCOPE_BEGIN, Lex::TK_STATEMENT_BEGIN });
        if (token.kind < 0) return token;

        SyntaxNode* tmpLoop = currentLoop;
        currentLoop = loop;
        
        token = parseScope(&lspan, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
        if (token.kind < 0) return token;
        
        currentLoop = tmpLoop;

        loop->scope = scope;
        loop->bodyScope = newScope;
        loop->expression = newOperand;

        scope->children.push_back(loop);

        loop->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseForEachLoop(Span* const span, Scope* const scope) {
        
        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(span, &tokenVal);

        Loop* const loop = new Loop();

        Scope* outerScope = new Scope();
        outerScope->fcn = currentFunction;
        outerScope->scope = scope;
        setParentIdx(outerScope);

        loop->scope = outerScope;

        Variable* newVar = new Variable(loop->scope, DT_UNDEFINED, span);
        token = parseExpression(&lspan, newVar, INVALID_POS, End { Lex::TK_STATEMENT_END }, USE_KEYWORD_AS_END);
        if (token.kind < 0) return token;

        if (token.kind != Lex::TK_KEYWORD && token.detail != KW_USING) {
            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_VARIABLE_NAME), span, "Variable name is matching key word name!");
            return Lex::toToken(Err::INVALID_VARIABLE_NAME);
        }

        loop->array = newVar;

        // var or var def (only basic dtypes)
        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind == Lex::TK_KEYWORD) {
            // var def
            
            if (!Lex::isInt((Lex::Keyword) token.detail)) {
                Logger::log(Logger::ERROR, "Only integral datatypes allowed!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            loop->idx = NULL;
            loop->idxDef = Utils::createEmptyVariableDefinition();
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
            loop->idx = new Variable(outerScope);
            loop->idx->name = tokenVal.str->buff;
            loop->idx->nameLen = tokenVal.str->len;

            token = Lex::nextToken(&lspan);
        
        }

        loop->bodyScope = new Scope();
        loop->bodyScope->fcn = currentFunction;
        loop->bodyScope->scope = loop->scope;
        setParentIdx(loop->bodyScope);

        SyntaxNode* tmpLoop = currentLoop;
        currentLoop = loop;
        
        token = parseScope(&lspan, loop->bodyScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
        if (token.encoded < 0) return token;
        
        currentLoop = tmpLoop;

        NodeRegistry::loops.push_back(loop);
        scope->children.push_back(loop);

        loop->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseGotoStatement(Span* const span, Scope* const scope) {

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(span, &tokenVal);
        
        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(Logger::ERROR, "Identifier expected!", span);
            return token.encoded < 0 ? token : Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        GotoStatement* gt = new GotoStatement();
        gt->span = getSpanStamp(span);
        gt->scope = scope;
        gt->name = tokenVal.str->buff;
        gt->nameLen = tokenVal.str->len;
        gt->label = NULL;

        scope->children.push_back(gt);
        scope->gotos.push_back(gt);
        NodeRegistry::gotos.push_back(gt);

        return token;

    }

    Lex::Token parseContinueStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_STATEMENT_END) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (!currentLoop) {
            Logger::log(Logger::ERROR, "Continue statement used outside of the loop!");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        ContinueStatement* stmt = new ContinueStatement();
        stmt->scope = scope;

        scope->children.push_back(stmt);
        
        stmt->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseBreakStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_STATEMENT_END) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (!currentLoop) {
            Logger::log(Logger::ERROR, "Break statement used outside of the loop!");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        BreakStatement* stmt = new BreakStatement();
        stmt->scope = scope;

        scope->children.push_back(stmt);

        stmt->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseNamespace(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(&lspan, &tokenVal);

        Namespace* nsc = new Namespace();
        nsc->type = NT_NAMESPACE;
        nsc->scope = scope;
        nsc->name = tokenVal.str->buff;
        nsc->nameLen = tokenVal.str->len;
        setParentIdx(nsc);
        
        token = Lex::nextToken(&lspan);
        if (token.kind != Lex::TK_SCOPE_BEGIN && token.kind != Lex::TK_STATEMENT_BEGIN) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = parseScope(&lspan, nsc, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
        if (token.encoded < 0) return token;

        scope->children.push_back(nsc);
        scope->namespaces.push_back(nsc);

        nsc->span = finalizeSpan(&lspan, span);
        return token;

    }

    Lex::Token parseAllocStatement(Span* const span, Scope* const scope) {

        return Lex::Token { Lex::TK_END };

    }

    Lex::Token parseFreeStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        Function* const fcn = internalFunctions + (IF_FREE - 1);

        FunctionCall* fcnCall = new FunctionCall();
        fcnCall->fptr = NULL;
        fcnCall->fcn = fcn;
        fcnCall->name = fcn->name;
        fcnCall->nameLen = fcn->nameLen;
        fcnCall->outArg = new Variable();
        fcnCall->outArg->cvalue.dtypeEnum = DT_VOID;
        
        Variable* inVar = new Variable();
        token = parseExpression(&lspan, inVar, INVALID_POS, End { Lex::TK_STATEMENT_END });
        if (token.encoded < 0) return token;

        fcnCall->inArgsCnt = 1;
        fcnCall->inArgs.push_back(inVar);

        Variable* wrapper = new Variable(scope);
        wrapper->expression = fcnCall;
        wrapper->span = getSpanStamp(&lspan);

        Statement* st = new Statement();
        st->scope = scope;
        st->op = wrapper;

        NodeRegistry::fcnCalls.push_back(wrapper);
        scope->children.push_back(st);

        token = Lex::nextToken(&lspan);
        return token;

    }

    Lex::Token parseReturnStatement(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;

        ReturnStatement* ret = new ReturnStatement();
        ret->fcn = currentFunction;
        ret->scope = scope;

        ret->var = NULL;
        ret->err = NULL;

        ret->idx = currentFunction->returns.size();
        currentFunction->returns.push_back(ret);

        scope->children.push_back(ret);
        NodeRegistry::returnStatements.push_back(ret);

        Pos startPos = lspan.end;

        token = Lex::nextToken(&lspan);
        if (token.kind == Lex::TK_STATEMENT_END) return token;
        
        if (token.kind != Lex::TK_SKIP) {

            Variable* newVar = new Variable(scope, DT_I64, &lspan);

            token = parseExpression(&lspan, newVar, startPos, End { Lex::TK_LIST_SEPARATOR, Lex::TK_STATEMENT_END });
            if (token.encoded < 0) return token;

            ret->var = newVar;
        
        } else {
            
            token = Lex::nextToken(&lspan);
        
        }

        if (token.kind == Lex::TK_STATEMENT_END) return token;

        if (token.kind != Lex::TK_LIST_SEPARATOR) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, Lex::toStr(Lex::TK_LIST_SEPARATOR));
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        // error case
        Variable* newVar = new Variable(scope, DT_I64, &lspan);

        token = parseExpression(&lspan, newVar, INVALID_POS, End { Lex::TK_STATEMENT_END });
        if (token.kind < 0) return token;

        ret->err = newVar;

        ret->span = finalizeSpan(&lspan, span);
        return token;
        
    }

    Lex::Token parseEnumDefinition(Span* const span, Scope* const scope) {

        // enum <name> : <type> { .. }

        SpanEx lspan = markSpanStart(span);

        Lex::TokenValue tokenVal;
        Lex::Token token;

        Enumerator* enumerator = new Enumerator();
        enumerator->dtype = DT_INT;

        token = Lex::nextToken(&lspan, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(Logger::ERROR, "Identifier expected!", &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        enumerator->name = tokenVal.str->buff;
        enumerator->nameLen = tokenVal.str->len;

        token = Lex::nextToken(&lspan);
        if (token.kind == Lex::TK_STATEMENT_BEGIN) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind != Lex::TK_KEYWORD) {
                Logger::log(Logger::ERROR, "Data type expected!", &lspan);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            if (!Lex::isInt((Lex::Keyword) token.detail)) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_DATA_TYPE), &lspan);
                return Lex::toToken(Err::UNKNOWN_DATA_TYPE);
            }

            enumerator->dtype = Lex::toDtype((Lex::Keyword) token.detail);

            token = Lex::nextToken(&lspan);

        }

        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(Logger::ERROR, "'{' is expected!", &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        enumerator->span = getSpanStamp(&lspan);

        enumerator->id = defId;
        defId++;

        ASSIGN_ID(enumerator);

        uint64_t lastValue = -1; //((uint64_t) 0) - ((uint64_t) 1);
        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            VariableDefinition* newVarDef = new VariableDefinition();
            Variable* newVar = new Variable(scope, enumerator->dtype, &lspan);

            newVarDef->var = newVar;
            newVarDef->var->cvalue.hasValue = 0;
            newVarDef->var->cvalue.dtypeEnum = enumerator->dtype;
            newVarDef->span = getSpanStamp(&lspan);
            newVarDef->flags = IS_CMP_TIME;

            newVar->def = newVarDef;
            newVar->name = tokenVal.str->buff;
            newVar->nameLen = tokenVal.str->len;
            newVar->unrollExpression = 1;

            ASSIGN_ID(newVar);

            enumerator->vars.push_back(newVar);

            token = Lex::nextToken(&lspan);
            if (token.kind == Lex::TK_EQUAL) {

                token = parseExpression(&lspan, newVar, INVALID_POS, End { Lex::TK_LIST_SEPARATOR, Lex::TK_SCOPE_END }, 0);
                if (token.encoded < 0) return token;

                newVarDef->var->cvalue.hasValue = 1;

            }

            if (token.kind == Lex::TK_LIST_SEPARATOR) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;

            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, "',' or '}'");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        
        }

        NodeRegistry::enumerators.push_back(enumerator);
        scope->children.push_back(enumerator);
        scope->enums.push_back(enumerator);

        enumerator->span = finalizeSpan(&lspan, span);

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
                Logger::log(Logger::ERROR, "Unexpected symbol!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }
            
            token = Lex::nextToken(&lspan);
        
        } else {
            
            keyword = Lex::KW_STRUCT;
        
        }

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(Logger::ERROR, "Identifier expected!", &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        TypeDefinition* newTypeDefinition;
        if (keyword == Lex::KW_STRUCT) {
            newTypeDefinition = new TypeDefinition();
        } else {
            newTypeDefinition = new Union();
        }

        newTypeDefinition->scope = scope;
        newTypeDefinition->name = tokenVal.str->buff;
        newTypeDefinition->nameLen = tokenVal.str->len;
        newTypeDefinition->parentIdx = NodeRegistry::customDataTypes.size();
        newTypeDefinition->span = getSpanStamp(&lspan);

        newTypeDefinition->id = defId;
        defId++;

        token = Lex::nextToken(&lspan);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        NodeRegistry::customDataTypes.push_back(newTypeDefinition);
        scope->customDataTypes.push_back(newTypeDefinition);
        scope->children.push_back(newTypeDefinition);

        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            VariableDefinition* varDef;
            token = parseVariableDefinition(&lspan, scope, { token, tokenVal }, End { Lex::TK_SCOPE_END, Lex::TK_STATEMENT_END }, NULL_FLAG, &varDef);
            if (token.kind < 0) return token;
            
            newTypeDefinition->vars.push_back(varDef->var);
            
            if (token.kind == Lex::TK_STATEMENT_END) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;
                
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        newTypeDefinition->span = finalizeSpan(&lspan, span);

        return token;

    }

    Lex::Token parseErrorDeclaration(Span* const span, Scope* scope) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        VariableDefinition* def = Utils::createEmptyVariableDefinition();
        def->var->name = tokenVal.str->buff;
        def->var->nameLen = tokenVal.str->len;
        def->var->cvalue.dtypeEnum = DT_ERROR;
        def->var->span = getSpanStamp(span);
        def->scope = scope;
        setParentIdx(def->var);

        auto res = def->scope->defSearch.insert({std::string_view(def->var->name, def->var->nameLen), def->var});
        if (!res.second) {
            Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), span);
            return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
        }

        ASSIGN_ID(def->var);

        token = Lex::nextToken(span, NULL);
        if (token.kind == Lex::TK_EQUAL) {

            token = parseExpression(span, def->var, INVALID_POS, End { Lex::TK_STATEMENT_END, Lex::TK_NONE });
            if (token.kind < 0) return token;

        } else if (token.kind != Lex::TK_STATEMENT_END) {

            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        NodeRegistry::variableDefinitions.push_back(def);
        scope->defs.push_back(def->var);
        scope->children.push_back(def);

        return token;

    }

    Lex::Token parseErrorDefinition(Span* const span, Scope* const scope) {

        SpanEx lspan = markSpanStart(span);

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(&lspan, &tokenVal);

        ErrorSet* errorSet = new ErrorSet();
        errorSet->scope = scope;
        errorSet->name = tokenVal.str->buff;
        errorSet->nameLen = tokenVal.str->len;
        errorSet->value = errId;
        errId++;

        ASSIGN_ID(errorSet);

        token = Lex::nextToken(&lspan, NULL);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), &lspan, Lex::toStr(Lex::TK_SCOPE_BEGIN));
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        while (1) {

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            Variable* var = new Variable();
            var->scope = scope;
            var->name = tokenVal.str->buff;
            var->nameLen = tokenVal.str->len;
            var->span = getSpanStamp(&lspan);
            var->cvalue.hasValue = 0;
            var->cvalue.dtypeEnum = DT_ERROR;
            var->cvalue.u64 = errId;
            errId++;

            errorSet->vars.push_back(var);

            token = Lex::nextToken(&lspan, &tokenVal);
            if (token.kind == Lex::TK_STATEMENT_END) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;

            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        NodeRegistry::customErrors.push_back(errorSet);
        scope->customErrors.push_back(errorSet);
        scope->children.push_back(errorSet);

        errorSet->span = finalizeSpan(&lspan, span);
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

        ImportStatement* import = new ImportStatement();
        // import->root = fileRootScope;
        import->scope = scope;

        ImportNode* importNode = new ImportNode;
        importNode->import = import;
        importNode->parent = importCurrent;

        token = Lex::tryKeyword(&lspan, Lex::KW_FROM);
        if (token.kind == Lex::TK_KEYWORD) {
            
            import->fname = *tokenVal.str;
            import->keyWord = (KeyWordType) -1;
            importCurrent->children.push_back(importNode);

        } else {

            token = Lex::nextFileName(&lspan, &tokenVal);
            
            import->fname = *tokenVal.str;
            import->keyWord = (KeyWordType) -1;
            importCurrent->children.push_back(importNode);

        }

        token = Lex::nextToken(&lspan, &tokenVal);
        if (!Lex::isKeyword(token, Lex::KW_AS)) {
            Logger::log(Logger::ERROR, "Keyword 'as' expected!", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind != Lex::TK_KEYWORD) {
            Logger::log(Logger::ERROR, "Unsupported value is used! Use one of the following : 'namespace', 'fcn', 'scope'\n", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (token.detail != KW_NAMESPACE && token.detail != KW_SCOPE && token.detail != KW_FUNCTION) {
            Logger::log(Logger::ERROR, "Unsupported value is used! Use one of the following : 'namespace', 'fcn', 'scope'\n", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        import->keyWord = (KeyWordType) token.detail;

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(Logger::ERROR, "Identifier expected!", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = Lex::nextToken(span, &tokenVal);
        if (token.kind != Lex::TK_STATEMENT_END) {
            Logger::log(Logger::ERROR, "End of statement expected!", span);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        importCurrent->children.push_back(importNode);

        return token;

    }

    Lex::Token parseDirective(Span* const span, Scope* const scope, Lex::Directive directive, Flags param) { 
        
        switch (directive) {

            case Lex::CD_TEST: {
                return Lex::toToken(Lex::TK_STATEMENT_END);
            }

        }

        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

    Lex::Token parseList(Span* const span, Lex::TokenKind separator, Lex::TokenKind end, std::vector<Variable*>& args) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        int first = 1;
        while (1) {

            Variable* newVar = new Variable();
            token = parseExpression(span, newVar, INVALID_POS, End { separator, end }, first ? EMPTY_EXPRESSION_ALLOWED : 0);
            if (token.encoded < 0) return token;
            
            if (first && (token.kind == end)) return token;

            args.push_back(newVar);

            if (token.kind == end) break;
            if (token.kind != separator) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            first = 0;

        }

        return Lex::Token { Lex::TK_END };

    }

    Lex::Token parseCatch(Span* const span, Variable* var) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        
        
        Variable* errVar = new Variable();
        errVar->scope = var->scope;
        errVar->cvalue.dtypeEnum = DT_ERROR;

        Scope* newScope = new Scope();
        newScope->scope = var->scope;
        setParentIdx(newScope);

        Catch* cex = new Catch();
        cex->call = (FunctionCall*) var->expression;
        
        var->expression = cex;



        token = Lex::nextToken(span, &tokenVal);
        
        if (Lex::isKeyword(token, Lex::KW_RETURN)) {
            // basically just emulates following
            // .. .. catch err { return _, err; }
            
            ReturnStatement* ret = new ReturnStatement();
            ret->err = errVar;
            ret->var = NULL;
            ret->scope = newScope;
            ret->fcn = currentFunction;

            newScope->children.push_back(ret);
            NodeRegistry::returnStatements.push_back(ret);

            token = Lex::nextToken(span, &tokenVal);
            if (token.kind != Lex::TK_STATEMENT_END) {
                Logger::log(Logger::ERROR, "'catch return' expression has to be terminated with ';'!", span);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            return token;
        }

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span, "error identifier");
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

            assignQualifiedName(errVar, errName);
            
            NodeRegistry::variables.push_back(var);

            cex->err = errVar;
            cex->scope = NULL;

            return token;
        
        }



        // 'catch err { ... }' case
        VariableDefinition* errDef = new VariableDefinition(errVar, 0);
        errDef->scope = newScope;
        errDef->var->scope = newScope;
        errDef->var->cvalue.dtypeEnum = DT_ERROR;
        errDef->var->span = getSpanStamp(span);
        errDef->var->parentIdx = -1;

        assignQualifiedName(errDef->var, errName);
        
        ASSIGN_ID(errDef->var);

        newScope->defs.push_back(errDef->var);
        NodeRegistry::variableDefinitions.push_back(errDef);

        auto res = newScope->defSearch.insert({std::string_view(errName->name, errName->nameLen), errDef->var});
        if (!res.second) {
            Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), errDef->var->span);
            return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
        }
        
        cex->err = errDef->var;
        cex->scope = newScope;

        return parseScope(span, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);

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

            UnaryExpression* uex = new UnaryExpression();
            uex->operType = op;
            
            var->expression = uex;
            var = new Variable();
        
        }

        switch (token.kind) {

            case Lex::TK_IDENTIFIER: {

                token = Lex::nextToken(span, &tokenVal);
                if (token.kind == Lex::TK_PARENTHESIS_BEGIN) {

                    FunctionCall* call = new FunctionCall();
                    call->fptr = NULL;
                    call->fcn = NULL;

                    token = parseList(span, Lex::TK_LIST_SEPARATOR, Lex::TK_PARENTHESIS_END, call->inArgs);
                    if (token.encoded < 0) return token;

                    assignQualifiedName(call, (QualifiedName*) tokenVal.any);

                    var->cvalue.hasValue = 0;
                    var->expression = call;
                
                } else {

                    assignQualifiedName(var, (QualifiedName*) tokenVal.any);
                    consumeToken = 0;

                }
                
                break;

            }

            case Lex::TK_NUMBER: {

                var->name = NULL;
                var->nameLen = 0;
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

                StringInitialization* init = new StringInitialization(tokenVal.str);
                var->expression = init;

                break;

            }

            case Lex::TK_CHAR: {

                var->cvalue.dtypeEnum = DT_I64;
                var->cvalue.i64 = tokenVal.ival;
            
                break;

            }

            case Lex::TK_ARRAY_BEGIN: {

                ArrayInitialization* init = new ArrayInitialization();
                token = parseArrayInitialization(span, scope, &init);
                var->expression = init;

                break;
            
            }

            case Lex::TK_SCOPE_BEGIN: {

                TypeInitialization* init = new TypeInitialization();
                token = parseTypeInitialization(span, scope, &init);
                var->expression = init;
                var->span = getSpanStamp(span);

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

            UnaryExpression* uex = new UnaryExpression();
            uex->operType = op;
            
            var->expression = uex;
            var = new Variable();
            
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

                Slice* slice = new Slice();

                slice->bidx = new Variable();
                slice->bidx->expression = var->expression;

                slice->eidx = new Variable();
                token = parseExpressionRecursive(span, slice->eidx, NULL, OP_NONE, lastDefIdx);

                var->expression = slice;

                if (token.kind != Lex::TK_ARRAY_END) {
                    tokenToErrorBuff(Lex::TK_ARRAY_END);
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span, errBuff.c_str());
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                token = nextToken(span, &tokenVal);

            } else {
                
                endTokenToErrorBuff({ Lex::TK_ARRAY_END, Lex::TK_SLICE });
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), span, errBuff.c_str());
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);

            }

        } else {
            token = parseExpressionNode(span, var->scope, var, lastDefIdx);
        }

        if (token.encoded < 0) return token;

        if (Lex::isOperator(token)) {

            OperatorEnum op = Lex::toBinaryOperator(token);
            
            while (op != OP_NONE) {

                if (prevOp == OP_NONE || operators[prevOp].rank > operators[op].rank) {
                    
                    BinaryExpression* bex = new BinaryExpression();
                    bex->operandA = new Variable();
                    bex->operandB = new Variable();
                    bex->operType = op;

                    copyValue(bex->operandA, var);
                    //nullValue(var);

                    var->expression = bex;

                    if (prevBex) {
                        prevBex->operandB->expression = bex;
                        // prevOp = op;
                    }

                    token = parseExpressionRecursive(span, bex->operandB, bex, op, lastDefIdx);
                    if (token.encoded < 0) return token;

                    op = Lex::toBinaryOperator(token);
                    if (op < 0) return token;
                
                } else {

                    prevBex->operandB = var;
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
                Logger::log(Logger::ERROR, "Yet only 'pure' function call expressions are allowed to be caught 🐱.", span);
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
        
        Logger::log(Logger::ERROR, "Blablbalba", span);            
        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

}
