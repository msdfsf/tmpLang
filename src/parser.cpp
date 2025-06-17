
// Each 'parse' function should return the last token it encauntered or error.
// Each 'parse' function that parses closure-like block should begin parsing with 'begin' closure already consumed.
// Each 'parse' function will take Location as first argument and Scope as second

// TODO : scope names as part of lexer
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

    Lex::Token parseScope(Location* const loc, Scope* scope, const ScopeType global = SC_COMMON, const char endWithStatement = 0);  
    Lex::Token parseForeignScope(Location* const loc, Scope* scope);
    Lex::Token parseLabel(Location* const loc, Scope* scope);
    Lex::Token parseKeywordStatement(Location* const loc, Scope* scope, const Lex::Keyword keyword, uint64_t param = 0);
    Lex::Token parseDirective(Location* const loc, Scope* scope, const Lex::Directive directive, uint64_t param = 0);
    Lex::Token parsePrintLiteral(Location* const loc, Scope* const scope, Lex::TokenValue* const startTokenValue);
    Lex::Token parseVariableAssignment(Location* const loc, Scope* scope, uint64_t endToken);
    Lex::Token parseVariableDefinition(Location* const loc, Scope* scope, uint64_t param, uint64_t endToken, VariableDefinition** outVarDef);
    Lex::Token parseVariableDefinition(Location* const loc, Scope* scope, DataTypeEnum basicDtype, uint64_t param, uint64_t endToken, VariableDefinition** outVarDef);
    Lex::Token parseDataType(Location* const loc, Scope* scope, const int expectQualifier, VariableDefinition* def);
    Lex::Token parseKnownDataType(Location* const loc, Scope* scope, DataTypeEnum dtype, char* dtypeName, const int dtypeNameLen, VariableDefinition* def);
    Lex::Token parseLanguageTag(Location* loc, String* tag);
    Lex::Token parseFunctionPointer(Location* const loc, Scope* scope, FunctionPrototype** fcnOut);
    Lex::Token parseDataTypeDecorators(Location* const loc, Scope* scope, Variable* var, Pointer** lastPointer, int include);
    Lex::Token parseFunctionPointer(Location* const loc, Scope* scope, FunctionPrototype** fcnOut);
    Lex::Token parseImport(Location* const loc, Scope* sc);
    Lex::Token parseDefinitionAssignment(Location* const loc, Scope* scope, VariableDefinition* const def, uint64_t endToken, int includeToScope);
    Lex::Token parseTypeInitialization(Location* const loc, Scope* scope, TypeInitialization** dtypeInit);
    Lex::Token parseRValue(Location* const loc, Scope* scope, Variable* outVar, uint16_t endChar);
    Lex::Token parseIfStatement(Location* const loc, Scope* const scope);
    Lex::Token parseSwitchStatement(Location* const loc, Scope* const scope);
    Lex::Token parseForLoop(Location* const loc, Scope* const scope);
    Lex::Token parseWhileLoop(Location* const loc, Scope* const scope);
    Lex::Token parseGotoStatement(Location* const loc, Scope* const scope);
    Lex::Token parseReturnStatement(Location* const loc, Scope* const scope);
    Lex::Token parseContinueStatement(Location* const loc, Scope* const scope);
    Lex::Token parseBreakStatement(Location* const loc, Scope* const scope);
    Lex::Token parseForEachLoop(Location* const loc, Scope* const scope);
    Lex::Token parseNamespace(Location* const loc, Scope* const scope);
    Lex::Token parseEnumInitialization(Location* const loc, Scope* const scope);
    Lex::Token parseError(Location* const loc, Scope* const scope);
    Lex::Token parseAllocStatement(Location* const loc, Scope* const scope);
    Lex::Token parseFreeStatement(Location* const loc, Scope* const scope);
    Lex::Token parseUnion(Location* const loc);
    Lex::Token parseTypeInitialization(Location* const loc, Scope* const scope);

    int parseExpression(Variable* var, char* const str, Location* const loc, const uint16_t endChar = STATEMENT_END, const int useKeyWordAsEnd = 0, const int emptyExpressionAllowed = 0, const int defIdx = -1);
    
    // TODO : to lexer
    int skipScopeNames(char* const str, Location* const loc);
    




    void copy(Variable* dest, Variable* src);
    void copy(Variable* dest, Variable* src);
    void copy(Scope* scA, Scope* scB);

    Namespace* getCopy(Namespace* nspace);
    Function* getCopy(Function* nspace);

    void appendScope(Scope* scA, Scope* scB);
    void appendPrefixScope(Scope* scA, Scope* scB);

    void stripWrapperExpressions(Variable** op);

    inline void offsetParentIdx(std::vector<SyntaxNode*> vec, const int offset);

    VariableDefinition* createEmptyVariableDefinition();
    
    int isArrayLHS(char* const str, Location* loc);



    struct End {
        Lex::TokenKind a;
        Lex::TokenKind b;
    };

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
    

    




    // it's a bit weird, but let's keep functions here as it will be 
    // easier to adjust them in the future. There is an option to use
    // maps, but they are dynamic, so switch case will be the ?better?

    OperatorEnum findUnaryOperator(uint32_t word) {

        switch (word) {

            case operators[OP_UNARY_PLUS].word  : return OP_UNARY_PLUS;
            case operators[OP_UNARY_MINUS].word : return OP_UNARY_MINUS;
            case Lex::POINTER                   : return OP_GET_VALUE;
            case Lex::ADDRESS                   : return OP_GET_ADDRESS;
            case operators[OP_INCREMENT].word   : return OP_INCREMENT;
            case operators[OP_DECREMENT].word   : return OP_DECREMENT;
            case operators[OP_NEGATION].word    : return OP_NEGATION;
            default                             : return OP_NONE;
        
        }

    }

    OperatorEnum findPostfixUnaryOperator(uint32_t word) {

        switch (word) {

            case operators[OP_INCREMENT].word   : return OP_INCREMENT;
            case operators[OP_DECREMENT].word   : return OP_DECREMENT;
            default                             : return OP_NONE;
        
        }

    }

    OperatorEnum findBinaryOperator(uint32_t word) {

        switch (word) {

            case operators[OP_ADDITION].word            : return OP_ADDITION;
            case operators[OP_SUBTRACTION].word         : return OP_SUBTRACTION;
            case operators[OP_MULTIPLICATION].word      : return OP_MULTIPLICATION;
            case operators[OP_DIVISION].word            : return OP_DIVISION;
            case operators[OP_MODULO].word              : return OP_MODULO;
            case operators[OP_LESS_THAN].word           : return OP_LESS_THAN;
            case operators[OP_GREATER_THAN].word        : return OP_GREATER_THAN;
            case operators[OP_LESS_THAN_OR_EQUAL].word  : return OP_LESS_THAN_OR_EQUAL;
            case operators[OP_GREATER_THAN_OR_EQUAL].word   : return OP_GREATER_THAN_OR_EQUAL;
            case operators[OP_EQUAL].word               : return OP_EQUAL;
            case operators[OP_NOT_EQUAL].word           : return OP_NOT_EQUAL;
            case operators[OP_BOOL_AND].word            : return OP_BOOL_AND;
            case operators[OP_BOOL_OR].word             : return OP_BOOL_OR;
            case operators[OP_CONCATENATION].word       : return OP_CONCATENATION;
            case operators[OP_MEMBER_SELECTION].word    : return OP_MEMBER_SELECTION;
            case operators[OP_BITWISE_AND].word         : return OP_BITWISE_AND;
            case operators[OP_BITWISE_OR].word          : return OP_BITWISE_OR;
            case operators[OP_BITWISE_XOR].word         : return OP_BITWISE_XOR;
            case operators[OP_BITWISE_NEGATION].word    : return OP_BITWISE_NEGATION;
            case operators[OP_SHIFT_RIGHT].word         : return OP_SHIFT_RIGHT;
            case operators[OP_SHIFT_LEFT].word          : return OP_SHIFT_LEFT;
            default     : return OP_NONE;
        
        }

    }






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
    
    // abstracted, so we can change the way we identify file
    struct FileId {
        uint64_t size;
        std::filesystem::file_time_type time;

        //FileId() = default;
        //FileId(const FileId&) = default;
        //FileId& operator=(const FileId&) = default;
        //FileId(FileId&&) noexcept = default;
        //FileId& operator=(FileId&&) noexcept = default;

        bool operator<(const FileId& other) const {
            return std::tie(size, time) < std::tie(other.size, other.time);
        }
    };

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
            Logger::log(Logger::PLAIN, " -> %.*s", NULL, 0, node->import->fname.len, node->import->fname.buff);
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
            return 1;
        }

        std::filesystem::path absPath = std::filesystem::absolute({flname});
        Location location = {
            new File { absPath, NULL, flname, buffer },
            buffer,
            1,
            0,
            1,
            0
        };

        importCurrent = import;

        // fileRootScope = fileScope ? fileScope : scope;
        return parseScope(&location, import->fileScope, SC_GLOBAL).encoded;

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
                Logger::log(Logger::ERROR, "File %.*s does not exists!", import->loc, import->fname.len, import->fname.len, import->fname.buff);
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
                Logger::log(Logger::ERROR, ERR_STR(Err::CIRCULAR_IMPORT), import->loc, import->fname.len);
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
                        Logger::log(Logger::ERROR, "Aprepritee symbol not found! Note, only namespaces and functions can be exported.", import->loc, import->param.len);
                        return Err::UNEXPECTED_SYMBOL;
                    }

                    switch (symbolType) {
                        
                        case NT_NAMESPACE : {

                            Namespace* sc = getCopy((Namespace*) symbol);

                            sc->scope = root;
                            sc->parentIdx = 0;
        
                            Utils::pushFornt<SyntaxNode*>(root->children, sc);
                            Utils::pushFornt<Namespace*>(root->namespaces, sc);
                            
                            break;

                        }

                        case NT_FUNCTION : {
                            
                            Function* fcn = getCopy((Function*) symbol);

                            fcn->scope = root;
                            fcn->parentIdx = 0;

                            Utils::pushFornt<SyntaxNode*>(root->children, fcn);
                            Utils::pushFornt<Function*>(root->fcns, fcn); 
                            
                            break;

                        }
                    
                        default:
                            Logger::log(Logger::ERROR, "Aprepritee symbol not found! Note, only namespaces and functions can be exported.", import->loc, import->param.len);
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
                    fcn->outArg = createEmptyVariableDefinition();
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
        int dirLen = Utils::stripDir(flname);

        Logger::log(Logger::INFO, "Parsing...\n");

        SyntaxNode::root = new Scope;
        SyntaxNode::root->fcn = NULL;
        SyntaxNode::root->scope = NULL;
        SyntaxNode::root->parentIdx = 0;
        SyntaxNode::dir = new INamed(dirStr, dirLen);

        std::filesystem::path flpath(flname);
        importRoot->fileId = genFileId(&flpath);
        if (!importRoot->fileId) {
            Logger::log(Logger::ERROR, ERR_STR(Err::FILE_DOES_NOT_EXISTS), NULL, 0, flname);
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




























    Lex::Token parseScope(Location* const loc, Scope* const scope, const ScopeType scopeType, const char endAsStatement) {

        syncStartToEnd(loc);

        int prevIdx = loc->sidx;
        int prevLine = loc->sln;
        Lex::Token prevToken = Lex::toToken(Lex::TK_NONE);

        while (1) {

            Lex::TokenValue tokenVal;
            Lex::Token token = Lex::nextToken(loc, &tokenVal);
            if (token.encoded < 0) {
                // ERROR
            }

            switch (token.kind) {

                case Lex::TK_STATEMENT_END : continue;
                
                case Lex::TK_END : {

                    if (scopeType == SC_GLOBAL) return Lex::toToken(Err::OK);
                    
                    // ERROR 
                    loc->eidx = prevIdx;
                    loc->eln = prevLine;
                    Logger::log(Logger::ERROR, "Unexpected end of file! Showing the start of the relevant section.", loc, 1);
                    return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
                
                }

                case Lex::TK_SCOPE_BEGIN : {

                    Scope* newScope = new Scope;
                    newScope->fcn = currentFunction;
                    newScope->scope = scope;
                    setParentIdx(newScope);

                    const Lex::Token err = parseScope(loc, newScope, SC_COMMON, 0);
                    if (err.encoded < 0) return err;

                    scope->children.push_back(newScope);

                }
                
                case Lex::TK_SCOPE_END : {

                    if (scopeType != SC_GLOBAL) return Lex::toToken(Err::OK);

                    // ERROR
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1, "");
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);

                }

                case Lex::TK_STRING : {

                    token = parsePrintLiteral(loc, scope, &tokenVal);
                    if (token.encoded < 0) return token;

                }

                case Lex::TK_KEYWORD : {

                    token = parseKeywordStatement(loc, scope, (Lex::Keyword) token.detail);
                    if (token.encoded < 0) return token;

                }

                case Lex::TK_DIRECTIVE : {

                    token = parseDirective(loc, scope, (Lex::Directive) token.detail);
                    if (token.encoded < 0) return token;

                }

                case Lex::TK_ARRAY_BEGIN : {

                    token = parseForeignScope(loc, scope);
                    if (token.encoded < 0) return token;

                }

                case Lex::TK_STATEMENT_BEGIN : {

                    token = parseLabel(loc, scope);
                    if (token.encoded < 0) return token;

                }

                case Lex::TK_IDENTIFIER : {
                    // assignment or custom data type definition
                    
                    token = Lex::nextTokenSkipDecorators(loc, &tokenVal);
                    if (token.kind == Lex::TK_IDENTIFIER) {
                        token = parseVariableDefinition(loc, scope, 0, Lex::TK_STATEMENT_END, NULL);
                    } else {
                        token = parseVariableAssignment(loc, scope, Lex::TK_STATEMENT_END);
                    }

                    if (token.encoded < 0) return token;

                }

                default : {
                    // assignment or error

                    token = parseVariableAssignment(loc, scope, Lex::TK_STATEMENT_END);
                    if (token.encoded < 0) return token;

                }

            }

            if (endAsStatement) {
                if (prevToken.kind == Lex::TK_STATEMENT_END) return Lex::toToken(Err::OK);
                // ERROR
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1, "");
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            prevIdx = loc->eidx;
            prevLine = loc->eln;
            prevToken = token;

        }
    
    }

    Lex::Token parsePrintLiteral(Location* const loc, Scope* const scope, Lex::TokenValue* const startTokenValue) {

        Lex::Token token;

        internalFunctionUsed = internalFunctionUsed | (1 << (IF_PRINTF - 1));

        Function* const fcn = internalFunctions + (IF_PRINTF - 1);

        FunctionCall* fcnCall = new FunctionCall();
        fcnCall->fptr = NULL;
        fcnCall->fcn = fcn;
        fcnCall->name = fcn->name;
        fcnCall->nameLen = fcn->nameLen;

        Variable* format = new Variable(scope, DT_STRING, loc);
        format->name = startTokenValue->str->buff;
        format->nameLen = startTokenValue->str->len;

        fcnCall->inArgs.push_back(format);

        Variable* callWrapper = new Variable;
        callWrapper->scope = scope;
        callWrapper->expression = fcnCall;
        callWrapper->unrollExpression = 1;
        callWrapper->cvalue.str = NULL;

        SyntaxNode::fcnCalls.push_back(callWrapper);

        while (1) {

            token = Lex::nextToken(loc);
            if (token.kind == Lex::TK_END) {
                return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
            }

            if (token.kind == Lex::TK_STATEMENT_END) {
                break;
            }

            Variable* newOperand = new Variable(scope, DT_UNDEFINED, loc);

            token = parseExpression(newOperand, str, loc, Lex::packTokens(Lex::TK_LIST_SEPARATOR, Lex::TK_STATEMENT_END));
            if (token.encoded < 0) return token;

            fcnCall->inArgs.push_back(newOperand);

            if (token.kind == Lex::TK_LIST_SEPARATOR) {
                continue;
            } else if (token.kind == Lex::TK_STATEMENT_END) {
                break;
            } else {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        }

        callWrapper->loc = getLocationStamp(loc);
        scope->children.push_back(callWrapper);

        return token;

    }

    Lex::Token parseLanguageTag(Location* loc, String* tag) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind != Lex::TK_ARRAY_END) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        return token;

    }

    Lex::Token parseForeignScope(Location* const loc, Scope* scope) {

        String tag;
        Lex::Token token;
        
        token = parseLanguageTag(loc, &tag);
        if (token.encoded < 0) return token;

        token = Lex::nextToken(loc, NULL);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        char* const codeStr = loc->str + loc->eidx;
        const int codeLen = Lex::findBlockEnd(loc, Lex::SCOPE_BEGIN, Lex::SCOPE_END);
        if (codeLen < 0) {
            // ERROR
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE), loc);
            return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
        }

        CodeBlock* codeBlock = new CodeBlock();
        codeBlock->tagStr = tag.buff;
        codeBlock->tagLen = tag.len;
        codeBlock->codeStr = codeStr;
        codeBlock->codeLen = codeLen;

        SyntaxNode::codeBlocks.push_back(codeBlock);

    }

    Lex::Token parseLabel(Location* const loc, Scope* scope) {

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(loc, &tokenVal);

        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(Logger::ERROR, "Identifier expected!", loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        Label* label = new Label();
        label->loc = getLocationStamp(loc);
        label->scope = scope;
        label->name = tokenVal.str->buff;
        label->nameLen = tokenVal.str->len;
        setParentIdx(label);

        scope->children.push_back(label);
        scope->labels.push_back(label);
        SyntaxNode::labels.push_back(label);

        auto res = scope->defSearch.insert({std::string_view(label->name, label->nameLen), label});
        if (!res.second) {
            Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), label->loc, label->nameLen);
            return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
        }

        token = Lex::nextToken(loc, NULL);
        if (token.kind != Lex::TK_LABEL_END) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        return token;

    }

    Lex::Token parseVariableAssignment(Location* const loc, Scope* scope, uint64_t endToken) {

        Lex::Token token;

        VariableAssignment* const varAssignment = new VariableAssignment(loc);
        varAssignment->scope = scope;
        varAssignment->lvar = new Variable(scope);
        varAssignment->rvar = new Variable(scope);
        varAssignment->rvar->loc = getLocationStamp(loc);

        token = parseExpression(loc, varAssignment->lvar, Lex::TK_EQUAL);
        if (token.encoded < 0) return token;

        token = parseRValue(varAssignment->rvar, str, loc, scope, endToken);
        if (token.encoded < 0) return token;

        if (varAssignment->lvar->cvalue.dtypeEnum == DT_ARRAY) {
            SyntaxNode::arraysAllocations.push_back(varAssignment);
        }

        SyntaxNode::variableAssignments.push_back(varAssignment);
        scope->children.push_back(varAssignment);

    }

    /*
    int parseVariableDefinition(Location* const loc, Location* const startLoc) {

        VariableDefinition* varDef = new VariableDefinition(startLoc);

        varDef->dtype = new INamedVar();
        parseScopeNames(varDef->dtype, str, startLoc);
        varDef->loc->idx -= varDef->dtype->nameLen + 1;

        err = processDataType(DT_CUSTOM, scope, str, loc, 0, STATEMENT_END, &varDef, 1, 0);
        if (err < 0) return err;

        SyntaxNode::customDataTypesReferences.push_back(varDef);

    }
    */
    
    Lex::Token parseKnownDataType(Location* const loc, Scope* scope, DataTypeEnum dtype, char* dtypeName, const int dtypeNameLen, VariableDefinition* def) {

        Lex::Token token;

        if (dtype == DT_UNDEFINED) {
            
            if (Utils::cmp(Lex::KWS_FCN, dtypeName, dtypeNameLen)) {
                
                FunctionPrototype* fptr;
                token = parseFunctionPointer(loc, scope, &fptr);
                if (token.encoded < 0) return token;

                def->var->cvalue.fcn = fptr;
                def->var->cvalue.dtypeEnum = (DataTypeEnum) DT_FUNCTION;

            } else {

                if (dtype == DT_UNDEFINED) {
                    // dt_custom case
                    def->var->cvalue.dtypeEnum = DT_CUSTOM;
                    def->var->cvalue.any = NULL;
                    def->dtype = new INamedVar();
                    def->dtype->name = dtypeName;
                    def->dtype->nameLen = dtypeNameLen;

                    SyntaxNode::customDataTypesReferences.push_back(def);
                } else {
                    def->var->cvalue.dtypeEnum = (DataTypeEnum) dtype;
                    def->var->cvalue.any = NULL;
                }
            
            }
        
        } else {

            def->var->cvalue.dtypeEnum = dtype;
            def->var->cvalue.any = (dtype == DT_CUSTOM) ? NULL : dataTypes + dtype;
        
        }

        Pointer* lastPtr = NULL;
        token = parseDataTypeDecorators(loc, scope, def->var, &lastPtr, 1);
        if (token.encoded < 0) return token;

        def->lastPtr = lastPtr;
        def->var->loc = getLocationStamp(loc);

        return token;

    }

    // def->flags will be rewritten
    Lex::Token parseDataType(Location* loc, Scope* scope, const int expectQualifier, VariableDefinition* def) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        def->loc = getLocationStamp(loc);

        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind == Lex::TK_KEYWORD && (token.detail == Lex::KW_CONST || token.detail == Lex::KW_EMBED)) {
            
            if (!expectQualifier) {
                Logger::log(Logger::ERROR, "Qualifier not expected here!", loc, 1);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            def->flags = ((token.kind == KW_CONST) ? KW_CONST : KW_CMP_TIME);
            token = Lex::nextToken(loc, &tokenVal);

        } else {
            def->flags = 0;
        }

        def->var = new Variable(scope);
        def->var->def = def;
        def->var->unrollExpression = 0;
        def->var->expression = NULL;
        def->scope = scope;

        if (token.kind == Lex::TK_IDENTIFIER) {
            
            return parseKnownDataType(loc, scope, DT_CUSTOM, tokenVal.str->buff, tokenVal.str->len, def);

        } else if (token.kind == Lex::TK_KEYWORD) {

            if (Lex::isDtype(token)) {
                Logger::log(Logger::ERROR, "Data type expected!", loc, 1);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            return parseKnownDataType(loc, scope, Lex::toDtype(token), NULL, 0, def);

        }

    }

    Lex::Token parseFunctionPointer(Location* const loc, Scope* scope, FunctionPrototype** fcnOut) {

        Lex::Token token;
        VariableDefinition* def;
        FunctionPrototype* fcn = new FunctionPrototype;

        token = Lex::nextToken(loc, NULL);
        if (token.kind != Lex::TK_PARENTHESIS_BEGIN) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);            
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }
        
        token = Lex::nextToken(loc, NULL);
        if (token.kind == Lex::TK_ARROW) {
            goto fOutArgs;
        }

        while (1) {
            
            def = new VariableDefinition;
            fcn->inArgs.push_back(def);

            token = parseDataType(loc, scope, 1, def);
            if (token.encoded < 0) return token;
            
            token = Lex::nextToken(loc, NULL);
            if (token.kind == Lex::TK_LIST_SEPARATOR) {
                continue;
            } else if (token.kind == Lex::TK_ARROW) {
                break;
            } else {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        }

        fOutArgs:

        token = Lex::nextToken(loc, NULL);
        if (token.kind != Lex::TK_PARENTHESIS_END) {
            
            def = new VariableDefinition;
            token = parseDataType(loc, scope, 0, def);
            if (token.encoded < 0) return token;

            token = Lex::nextToken(loc, NULL);
            if (token.kind != Lex::TK_PARENTHESIS_END) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

        } else {

            def = createEmptyVariableDefinition();
        
        }

        fcn->outArg = def;
        *fcnOut = fcn;

        return token;

    }

    // part starting with variable name in definition
    // ex: const int^ x ... from x
    Lex::Token parseDefinitionAssignment(Location* const loc, Scope* scope, VariableDefinition* const def, uint64_t endToken, int includeToScope) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        Lex::nextToken(loc, &tokenVal);

        def->var->name = tokenVal.str->buff;
        def->var->nameLen = tokenVal.str->len;
        
        ASSIGN_ID(def->var);

        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind == Lex::TK_EQUAL) {

            token = parseRValue(def->var, str, loc, scope, endToken);
            if (token.encoded < 0) return token;

            if (def->flags & IS_CMP_TIME) SyntaxNode::cmpTimeVars.push_back(def->var);
            else SyntaxNode::initializations.push_back(def);

        } else if (token.kind == endToken.a || token.kind == endToken.b) {

            def->var->expression = NULL;
        
        } else {            
            // ERROR    
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (includeToScope) {
            //scope->children.push_back(def);
            // pushDefLike(scope->defSearch, def->var);
            
            setParentIdx(def->var);

            auto res = scope->defSearch.insert({std::string_view(def->var->name, def->var->nameLen), def->var});
            if (!res.second) {
                Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), def->var->loc, def->var->nameLen);
                return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
            }

            scope->children.push_back(def);
            SyntaxNode::variableDefinitions.push_back(def);
            scope->defs.push_back(def->var);
        }

        return token;

    }

    Lex::Token parseVariableDefinition(Location* const loc, Scope* scope, uint64_t param, uint64_t endToken, VariableDefinition** outVarDef) {       

        Lex::Token token;

        VariableDefinition* varDef = new VariableDefinition;
        token = parseDataType(loc, scope, 1, varDef);
        if (token.encoded < 0) return token;

        token = parseDefinitionAssignment(loc, scope, varDef, endToken, 0);
        if (token.encoded < 0) return token;

        *outVarDef = varDef;
        return token;

    }

    
    Lex::Token parseVariableDefinition(Location* const loc, Scope* scope, DataTypeEnum basicDtype, uint64_t param, uint64_t endToken, VariableDefinition** outVarDef) {       

        VariableDefinition* def = new VariableDefinition;
        Lex::Token token = parseKnownDataType(loc, scope, basicDtype, NULL, 0, def);
        if (token.encoded < 0) return token;

        token = parseDefinitionAssignment(loc, scope, def, endToken, 0);
        if (token.encoded < 0) return token;

        *outVarDef = def;
        return token;

    }

    Lex::Token parseStringLiteral(Location* const loc, String* str, StringInitialization** initOut) {

        Lex::Token token = Lex::nextToken(loc, NULL);
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

    // already without starting '['
    int parseArrayInitialization(char* const str, Location* loc, ArrayInitialization** initOut) {

        *initOut = new ArrayInitialization();

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;
        const int startIdx = loc->idx;

        while (1) {

            Variable* var = new Variable();

            const int err = parseExpression(var, str, loc, toDoubleChar(',', ']'));
            if (err < 0) return err;

            (*initOut)->attributes.push_back(var);

            if (str[loc->idx - 1] == ARRAY_END) break;

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        }

        return Err::OK;
    
    }

    Lex::Token parseTypeInitialization(Location* const loc, Scope* scope, TypeInitialization** dtypeInit) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        *dtypeInit = new TypeInitialization();

        token = Lex::nextToken(loc, &tokenVal);
        Lex::Token nextToken = Lex::nextToken(loc, NULL);

        if (nextToken.kind == Lex::TK_STATEMENT_BEGIN || token.kind == Lex::TK_THE_REST) {
            // names assumed

            int hasFillVar = 0;
            (*dtypeInit)->fillVar = NULL;

            while (1) {

                Variable* var = new Variable(scope);
                var->loc = getLocationStamp(loc);

                if (token.kind == Lex::TK_THE_REST) {
                    
                    if (hasFillVar) {
                        Logger::log(Logger::ERROR, "Fill the rest symbol can be used only once per initialization!", loc, 2);
                        return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                    }

                    (*dtypeInit)->fillVar = var;
                    hasFillVar = 1;

                } else if (token.kind == Lex::TK_IDENTIFIER) {

                    var->name = tokenVal.str->buff;
                    var->nameLen = tokenVal.str->len;
                    (*dtypeInit)->attributes.push_back(var);
                
                } else {
                    
                    // ERROR
                    Logger::log(Logger::ERROR, "Attribute name expected!", loc);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                
                }

                token = Lex::nextToken(loc, NULL);
                if (token.kind != Lex::TK_STATEMENT_BEGIN) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                token = parseExpression(var, str, loc, toDoubleChar(',', '}'));
                if (token.encoded < 0) return token;

                if (token.kind == Lex::TK_SCOPE_END) break;
                token = Lex::nextToken(loc, &tokenVal);

            }

        } else {

            while (1) {
                
                Variable* var = new Variable();

                token = parseExpression(var, str, loc, toDoubleChar(',', '}'));
                if (token.encoded < 0) return token;

                (*dtypeInit)->attributes.push_back(var);

                if (token.kind == Lex::TK_SCOPE_END) break;
                
            }

        }

        (*dtypeInit)->idxs = (int*) malloc((*dtypeInit)->attributes.size() * sizeof(int));
        if (!(*dtypeInit)->idxs) {
            Logger::log(Logger::ERROR, Err::str[-Err::MALLOC]);
            return Lex::toToken(Err::MALLOC);
        }

        return token;
    
    }

    // either alloc or expression
    // alloc [DtypeName, omitted if mainDtype >= 0] ['[' Expression defining length ']'] [:] [DtypeInit]
    Lex::Token parseRValue(Location* const loc, Scope* scope, Variable* outVar, uint16_t endChar) {
        
        Lex::Token token;
        Lex::TokenValue tokenVal;

        DataTypeEnum mainDtype = outVar->cvalue.dtypeEnum;
        if (mainDtype == DT_POINTER) {
            mainDtype = outVar->cvalue.ptr->pointsToEnum;
        } else if (mainDtype == DT_ARRAY) {
            mainDtype = outVar->cvalue.arr->pointsToEnum;
        }

        token = Lex::nextToken(loc, NULL);
        if (token.kind == Lex::TK_KEYWORD && token.detail == Lex::TD_KW_ALLOC) {
        
            VariableDefinition* varDef = new VariableDefinition();
            varDef->scope = scope;
            varDef->loc = getLocationStamp(loc);

            Variable* var = new Variable();
            var->def = varDef;

            varDef->var = var;

            token = Lex::nextToken(loc, &tokenVal);
            if (token.kind == Lex::TK_IDENTIFIER) {
                
                var->cvalue.dtypeEnum = DT_CUSTOM;
                var->cvalue.any = NULL;

                varDef->dtype = new INamedVar();
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
                    varDef->dtype = new INamedVar();
                    varDef->dtype = outVar->def->dtype;
                }
            
            }

            Function* const fcn = internalFunctions + (IF_ALLOC - 1);

            FunctionCall* fcnCall = new FunctionCall();
            fcnCall->fptr = NULL;
            fcnCall->fcn = fcn;
            fcnCall->name = fcn->name;
            fcnCall->nameLen = fcn->nameLen;
            fcnCall->outArg = new Variable();
            fcnCall->outArg->cvalue.dtypeEnum = DT_POINTER;

            Pointer* lastPtr = NULL;
            parseDataTypeDecorators(loc, scope, var, &lastPtr, 0);
            varDef->lastPtr = lastPtr;

            token = Lex::nextToken(loc, NULL);
            if (token.kind == Lex::TK_STATEMENT_BEGIN) {
                token = parseExpression(loc, var, loc, endChar);
                if (token.encoded < 0) return token;
            } else if (token.kind != Lex::TK_STATEMENT_END) {
                Logger::log(Logger::ERROR, "TODO error: parseRValue unexpected symbol!");
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }
            
            fcnCall->inArgs.push_back(var);

            outVar->expression = fcnCall;
            SyntaxNode::fcnCalls.push_back(outVar);
            // initializations.push_back(varDef);
            
            // ...
            SyntaxNode::customDataTypesReferences.push_back(varDef);

            outVar->snFlags |= IS_ALLOCATION;

        } else {
            
            token = parseExpression(outVar, str, loc, endChar);
            if (token.encoded < 0) return token;

        }

        return token;

    }
    

    // pointers can ocur only before arrays
    // for now only one array
    Lex::Token parseDataTypeDecorators(Location* const loc, Scope* scope, Variable* var, Pointer** lastPointer, int include) {

        Lex::Token token;
        int wasArray = 0;

        Pointer* mainPtr = NULL;
        while (1) {

            token = Lex::nextToken(loc, NULL);
            if (token.kind == Lex::TK_POINTER) {

                if (wasArray) {
                    Logger::log(Logger::ERROR, "Pointer can't be used after array declaration!", loc, 1);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }
            
                Pointer* ptr = new Pointer();
                if (!mainPtr) *lastPointer = ptr;
                else mainPtr->parentPointer = ptr;

                ptr->parentPointer = mainPtr;
                ptr->pointsTo = var->cvalue.any;
                ptr->pointsToEnum = var->cvalue.dtypeEnum;

                var->cvalue.dtypeEnum = DT_POINTER;
                var->cvalue.ptr = ptr;

                mainPtr = ptr;

            } else if (token.kind == Lex::ARRAY_BEGIN) {
                // either const / embed or expression
                
                if (wasArray) {
                    Logger::log(Logger::ERROR, "Multidimensional arrays not allowed!", loc, 1);
                    return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                }

                wasArray = 1;

                Array* arr = new Array;
                if (!mainPtr) *lastPointer = arr;
                else mainPtr->parentPointer = arr;

                arr->pointsTo = var->cvalue.any;
                arr->pointsToEnum = var->cvalue.dtypeEnum;

                Variable* lenVar = new Variable(loc);
                lenVar->scope = scope;
                lenVar->nameLen = 0;
                
                token = Lex::nextToken(loc, NULL);
                if (token.kind == Lex::TK_KEYWORD) {

                    const int keyword = token.detail;

                    token = Lex::nextToken(loc, NULL);
                    if (token.kind != Lex::ARRAY_END) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL));
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

                    token = parseExpression(lenVar, str, loc, Lex::ARRAY_END, 0, 1);
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

                if (include) SyntaxNode::arrays.push_back(var);

            } else {

                break;
            
            }

        }

        return token;

    }





    Lex::Token parseKeywordStatement(Location* const loc, Scope* const scope, const Lex::Keyword keyword, uint64_t param) {

        switch (keyword) {

            case Lex::KW_INT: 
                return parseVariableDefinition(loc, scope, DT_INT, param, Lex::TK_STATEMENT_END, NULL);
            case Lex::KW_I8:
                return parseVariableDefinition(loc, scope, DT_I8, param, Lex::TK_STATEMENT_END, NULL);
            case Lex::KW_I16:
                return parseVariableDefinition(loc, scope, DT_I16, param, Lex::TK_STATEMENT_END, NULL);
            case Lex::KW_I32:
                return parseVariableDefinition(loc, scope, DT_I32, param, Lex::TK_STATEMENT_END, NULL);
            case Lex::KW_I64:
                return parseVariableDefinition(loc, scope, DT_I64, param, Lex::TK_STATEMENT_END, NULL);
            case Lex::KW_U8:
                return parseVariableDefinition(loc, scope, DT_U8, param, Lex::TK_STATEMENT_END, NULL);
            case Lex::KW_U16:
                return parseVariableDefinition(loc, scope, DT_U16, param, Lex::TK_STATEMENT_END, NULL);
            case Lex::KW_U32:
                return parseVariableDefinition(loc, scope, DT_U32, param, Lex::TK_STATEMENT_END, NULL);
            case Lex::KW_U64:
                return parseVariableDefinition(loc, scope, DT_U64, param, Lex::TK_STATEMENT_END, NULL);
            case Lex::KW_F32:
                return parseVariableDefinition(loc, scope, DT_F32, param, Lex::TK_STATEMENT_END, NULL);
            case Lex::KW_F64:
                return parseVariableDefinition(loc, scope, DT_F64, param, Lex::TK_STATEMENT_END, NULL);
            case KW_CMP_TIME:
                param = param | IS_CMP_TIME;
            case KW_CONST:
                param = param | IS_CONST;
                return parseVariableDefinition(loc, scope, param, Lex::TK_STATEMENT_END, NULL);
            case KW_FUNCTION:
                return parseFunction(loc, scope, param);
            case KW_IF:
                return parseIfStatement(loc, scope);
            case KW_SWITCH_CASE:
                return parseSwitchStatement(loc, scope);
            case KW_FOR:
                return parseForLoop(loc, scope);
            case KW_WHILE:
                return parseWhileLoop(loc, scope);
            case KW_GOTO:
                return parseGotoStatement(loc, scope);
            case KW_ENUM:
                return parseEnumDefinition(loc, scope);
            case KW_TYPE_DEF:
                return parseTypeDefinition(loc, scope);
            case KW_RETURN:
                return parseReturnStatement(loc, scope);
            case KW_CONTINUE:
                return parseContinueStatement(loc, scope);
            case KW_BREAK:
                return parseBreakStatement(loc, scope);
            case KW_LOOP:
                return parseForEachLoop(loc, scope);
            case KW_NAMESPACE:
                return parseNamespace(loc, scope);
            case KW_ALLOC:
                return parseAllocStatement(loc, scope);
            case KW_FREE:
                return parseFreeStatement(loc, scope);
            case KW_IMPORT:
                return parseImport(loc, scope);
            case KW_ERROR:
                return parseError(loc, scope);

        }

        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

    // basic data types only
    /*
    int parseBasicVariableDefinition(Location* const loc, Scope* const scope, const DataTypeEnum dtype, 
        const uint64_t param, 
        const uint16_t endChar,
        VariableDefinition** outVarDef,
        const int include,
        const int alloc
    ) {

        Lex::Token token;

        VariableDefinition* def;
        if (alloc) {
            def = new VariableDefinition(loc);
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
        token = parseDataTypeDecorators(loc, scope, def->var, &lastPtr, 1);
        if (token.encoded < 0) return token;

        def->lastPtr = lastPtr;
        def->var->loc = getLocationStamp(loc);

        token = parseDefinitionAssignment(loc, scope, def, endChar, include);
        if (token.encoded < 0) return token;

        if (outVarDef) *outVarDef = def;
        return Err::OK;

    }
    */

    Lex::Token parseFunction(Location* const loc, Scope* const scope, uint64_t param) {

        Lex::TokenValue tokenVal;       
        Lex::Token token;

        // first test if it could bee fcn pointer
        token = Lex::nextToken(loc);
        if (token.kind == Lex::TK_PARENTHESIS_BEGIN) {
            
            VariableDefinition* def;
            //Lex::Token tmp = { .kind: Lex::TK_IDENTIFIER, .detail: Lex::TD_FCN };
            token = parseVariableDefinition(loc, scope, param, Lex::TK_STATEMENT_END, &def);
            if (token.encoded < 0) return token;
            
            // pushDefLike(scope->defSearch, def->var);
            auto res = scope->defSearch.insert({std::string_view(def->var->name, def->var->nameLen), def->var});
            if (!res.second) {
                Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), def->var->loc, def->var->nameLen);
                return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
            }

            setParentIdx(def->var);

            scope->children.push_back(def);
            SyntaxNode::variableDefinitions.push_back(def);
            scope->defs.push_back(def->var);

            return token;
        
        }

        Function* fcn;

        Scope* newScope = new Scope();
        newScope->fcn = currentFunction;
        newScope->scope = scope;
        setParentIdx(newScope);

        int foreignLang = 0;
        token = Lex::nextToken(loc, NULL);
        if (token.kind == Lex::TK_ARRAY_BEGIN) {
                
            foreignLang = 1;
            fcn = new ForeignFunction();

            Location* tagLoc = getLocationStamp(loc);
            
            String tag;
            token = parseLanguageTag(loc, &tag);
            if (token.encoded < 0) return token;

            ((ForeignFunction*) fcn)->tagLen = tag.len;
            ((ForeignFunction*) fcn)->tagStr = tag.buff;
            ((ForeignFunction*) fcn)->tagLoc = tagLoc;
            
            ASSIGN_ID(fcn);

        } else {
            
            fcn = new Function();
        
        }

        fcn->inArgsCnt = 0;
        fcn->scope = scope;
        fcn->errorSetName = NULL;
        fcn->outArg = createEmptyVariableDefinition();
        fcn->loc = getLocationStamp(loc);

        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(Logger::ERROR, "Function name expected!", loc, 1);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        fcn->name = tokenVal.str->buff;
        fcn->nameLen = tokenVal.str->len;

        ASSIGN_ID(fcn);

        token = Lex::nextToken(loc);
        if (token.kind != Lex::TK_PARENTHESIS_BEGIN) {

            if (token.kind == Lex::TK_SCOPE_BEGIN) goto fcnParseScope;

            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        
        }

        // parse input
        while (1) {

            token = Lex::nextToken(loc);
            if (token.kind == Lex::TK_PARENTHESIS_BEGIN) break;

            fcn->inArgsCnt++;

            VariableDefinition* varDef;
            token = parseVariableDefinition(loc, scope, param, Lex::packTokens(Lex::TK_PARENTHESIS_END, Lex::TK_LIST_SEPARATOR), &varDef);
            if (token.encoded < 0) return token;

            // :)
            varDef->var->parentIdx = -1;
            varDef->parentIdx = -1;

            auto res = newScope->defSearch.insert({std::string_view(varDef->var->name, varDef->var->nameLen), varDef->var});
            if (!res.second) {
                Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), varDef->var->loc, varDef->var->nameLen);
                return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
            }
            
            SyntaxNode::variableDefinitions.push_back(varDef);
            newScope->defs.push_back(varDef->var);

            fcn->inArgs.push_back(varDef);

            token = Lex::nextToken(loc, NULL);
            if (token.kind == Lex::TK_PARENTHESIS_END) break;
        
        }

        // [using 'Error Set']
        token = Lex::tryKeyword(loc, Lex::KW_USING);
        if (token.kind != Lex::TK_NONE) {

            fcn->errorSetName = new INamedVar();
            token = parseScopeNames(loc, fcn->errorSetName);
            if (token.encoded < 0) return token;

            Using* tmp = new Using();
            tmp->var = fcn;
            
            newScope->usings.push_back(tmp);

        }

        // ->
        token = Lex::tryToken(loc, Lex::TK_ARROW);
        if (token.kind != Lex::TK_ARROW) {
            
            if (token.kind == Lex::TK_SCOPE_BEGIN) goto fcnParseScope;

            // LOOK AT : maybe better name for this situation, something like "unexpected char sequence"
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 2);
            Logger::log(Logger::HINT, "'->' expected\n");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        // parse output
        token = parseDataType(loc, scope, 0, fcn->outArg);
        if (token.encoded < 0) return token;

        // scope
        token = Lex::nextToken(loc);
        if (token.kind != Lex::TK_SCOPE_BEGIN || token.kind != Lex::TK_STATEMENT_BEGIN) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        fcnParseScope:
        
        if (foreignLang) {
            
            ForeignFunction* ffcn = (ForeignFunction*) fcn;
            
            ffcn->codeStr = loc->str + loc->sidx;
            ffcn->codeLen = Lex::findBlockEnd(loc, Lex::SCOPE_BEGIN, Lex::SCOPE_END);
            if (ffcn->codeLen < 0) {
                return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
            }

            ffcn->internalIdx = -3443431;
            scope->fcns.push_back(ffcn);
            SyntaxNode::foreignFunctions.push_back(ffcn);

            return token;
        
        }

        fcn->internalIdx = 0;

        //Scope* newScope = new Scope;
        //newScope->fcn = currentFunction;
        //newScope->scope = outerScope;

        currentFunction = fcn;
        token = parseScope(loc, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
        if (token.kind < 0) return token;
        currentFunction = NULL;

        fcn->bodyScope = newScope;

        scope->children.push_back(fcn);
        scope->fcns.push_back(fcn);
        SyntaxNode::fcns.push_back(fcn);

    }

    Lex::Token parseIfStatement(Location* const loc, Scope* const scope) {

        syncStartToEnd(loc);

        Lex::TokenValue tokenVal;
        Lex::Token token;
        const int parentIdx = scope->children.size();

        Scope* newScope = new Scope();
        newScope->fcn = currentFunction;
        newScope->scope = scope;
        newScope->parentIdx = parentIdx;

        Variable* newOperand = new Variable(scope);
        token = parseExpression(newOperand, str, loc, Lex::packTokens(Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN));
        if (token.encoded < 0) return token;
        
        newOperand->loc = getLocationStamp(loc);

        token = parseScope(loc, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
        if (token.encoded < 0) return token;

        Branch* branch = new Branch();
        branch->scope = scope;
        branch->scopes.push_back(newScope);
        branch->expressions.push_back(newOperand);

        scope->children.push_back(branch);
        SyntaxNode::branchExpressions.push_back(newOperand);

        while (1) {

            token = Lex::nextToken(loc, &tokenVal);
            if (token.kind != Lex::TK_KEYWORD && token.detail != Lex::TD_KW_ELSE) {
                return token;
            }
            
            if (token.kind == Lex::TK_SCOPE_BEGIN || token.kind == Lex::TK_STATEMENT_BEGIN) {
                // else case

                Scope* newScope = new Scope();
                newScope->fcn = currentFunction;
                newScope->scope = scope;
                newScope->parentIdx = parentIdx;

                token = parseScope(loc, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
                if (token.encoded < 0) return token;

                branch->scopes.push_back(newScope);

                return token;

            } else if (token.kind == Lex::TK_KEYWORD && token.detail == Lex::TD_KW_IF) {
                // else if case

                Variable* newOperand = new Variable(scope);
                newOperand->loc = getLocationStamp(loc);

                token = parseExpression(newOperand, str, loc, CHAR_CAT(STATEMENT_BEGIN, SCOPE_BEGIN));
                if (token.encoded < 0) return token;

                Scope *newScope = new Scope();
                newScope->fcn = currentFunction;
                newScope->scope = scope;
                newScope->parentIdx = parentIdx;

                token = parseScope(loc, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
                if (token.encoded < 0) return token;

                branch->scopes.push_back(newScope);
                branch->expressions.push_back(newOperand);
                SyntaxNode::branchExpressions.push_back(newOperand);
            
            }
        
        }

    }

    Lex::Token parseSwitchStatement(Location* const loc, Scope* const scope) {

        syncStartToEnd(loc);
    
        Lex::TokenValue tokenVal;
        Lex::Token token;

        SwitchCase* switchCase = new SwitchCase;
        switchCase->scope = scope;
        switchCase->elseCase = NULL;

        Variable* var = new Variable;
        var->scope = scope;

        token = parseExpression(var, str, loc, Lex::packTokens(Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN));
        if (token.encoded < 0) return token;

        var->loc = getLocationStamp(loc);
        switchCase->switchExp = var;

        if (token.kind != Lex::TK_STATEMENT_BEGIN && token.kind != Lex::TK_SCOPE_BEGIN) {
            // ERROR
            Logger::log(Logger::ERROR, "TODO");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        int elseCase = 0;
        int atLeastOneCase = 0;
        while (1) {

            token = Lex::nextToken(loc, &tokenVal);

            if (token.kind == Lex::TK_END) {
                if (atLeastOneCase) return token;
                // ERROR
                Logger::log(Logger::ERROR, "TODO: unexpected end of file!", loc, 1);
                return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
            }

            if (token.kind != Lex::TK_KEYWORD || token.detail != Lex::TD_KW_WHEN && token.detail != Lex::TD_KW_ELSE)) {
                break;
            }

            elseCase = token.detail == Lex::TK_KW_ELSE;
            atLeastOneCase = 1;
            
            Variable* cmpExp;
            if (!elseCase) {
                cmpExp = new Variable();
                cmpExp->scope = scope;
                token = parseExpression(cmpExp, str, loc, Lex::packTokens(Lex::TK_STATEMENT_BEGIN, Lex::TK_SCOPE_BEGIN));
                if (token.encoded < 0) return token;
            } else {
                token = Lex::nextToken(loc);
            }

            Scope* sc = new Scope;
            sc->fcn = currentFunction;
            sc->scope = scope;
            setParentIdx(sc);

            if (token.kind == Lex::TK_STATEMENT_BEGIN) {
                parseScope(loc, sc, SC_COMMON, 1); 
            } else if (token.kind == Lex::TK_SCOPE_BEGIN) {
                parseScope(loc, sc);
            } else {
                // ERROR
                Logger::log(Logger::ERROR, "TODO");
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            if (elseCase) {
                switchCase->elseCase = sc;
                break;
            }

            switchCase->casesExp.push_back(cmpExp);
            switchCase->cases.push_back(sc);

        }

        SyntaxNode::switchCases.push_back(switchCase);
        scope->children.push_back(switchCase);
        
        switchCase->loc = getLocationStamp(loc);

    }

    Lex::Token parseForLoop(Location* const loc, Scope* const scope) {
        
        // TODO : something about empty expressions, maybe make them null or something...
        Lex::Token token;

        ForLoop *loop = new ForLoop();

        Scope* outerScope = new Scope();
        outerScope->fcn = currentFunction;
        outerScope->scope = scope;
        setParentIdx(outerScope);

        Variable* initEx = new Variable(outerScope);

        // can be either variable initialization or expression
        token = Lex::nextToken(loc);
        if (token.kind == Lex::TK_KEYWORD && Lex::isDtype(token)) {
            token = parseVariableDefinition(loc, scope, Lex::toDtype(token), Lex::TK_STATEMENT_END, NULL);
        } else {
            token = parseExpression(loc, initEx, Lex::TK_STATEMENT_END, 0, 1);
        }
        if (token.kind < 0) return token;

        Scope* bodyScope = new Scope();
        bodyScope->fcn = currentFunction;
        bodyScope->scope = outerScope;
        setParentIdx(bodyScope);

        Variable* conditionEx = new Variable(outerScope);

        token = parseExpression(conditionEx, str, loc, STATEMENT_END, 0, 1);
        if (token < 0) return token;
        
        conditionEx->loc = getLocationStamp(loc);

        // can be assignment
        Variable* actionEx = new Variable(outerScope);

        token = parseExpression(loc, actionEx, CHAR_CAT(EQUAL_SYMBOL, STATEMENT_END_SYMBOL));
        if (token < 0) return token;

        int assignment = 0;
        if (token.kind == TK_EQUAL) {
            
            VariableAssignment* ass = new VariableAssignment();
            ass->lvar = actionEx;
            ass->lvar->parentIdx = 0;

            actionEx = new Variable(outerScope);
            
            token = parseVariableAssignment(loc, outerScope, tmp, ass);
            if (token < 0) return token;

            outerScope->children.push_back(ass);
            outerScope->variableAssignments.push_back(ass);
            outerScope->variables.push_back(ass->lvar);

        }

        SyntaxNode* tmpLoop = currentLoop;
        currentLoop = loop;

        token = Lex::nextToken(loc);
        if (token.kind == Lex::TK_SCOPE_BEGIN || token.kind == Lex::TK_STATEMENT_BEGIN) {
            token = parseScope(loc, bodyScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
            if (token.kind < 0) return token;
        } else {
            // ERROR
            Logger::log(Logger::ERROR, "Unexpected symbol!", loc, 1);
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
            SyntaxNode::branchExpressions.push_back(conditionEx);
        } else {
            loop->conditionEx = NULL;
            freeLocationStamp(conditionEx->loc);
            delete conditionEx;
        }

    }

    Lex::Token parseWhileLoop(Location* const loc, Scope* const scope) {

        Lex::Token token;
        syncStartToEnd(loc);

        WhileLoop* loop = new WhileLoop();

        Scope* newScope = new Scope();
        newScope->fcn = currentFunction;
        newScope->scope = scope;
        setParentIdx(newScope);

        Variable* newOperand = new Variable(newScope);

        token = parseExpression(loc, newOperand, Lex::packTokens(Lex::TK_SCOPE_BEGIN, Lex::TK_STATEMENT_BEGIN));
        if (token.kind < 0) return token;

        SyntaxNode* tmpLoop = currentLoop;
        currentLoop = loop;
        
        token = parseScope(loc, newScope, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
        if (token.kind < 0) return token;
        
        currentLoop = tmpLoop;

        loop->scope = scope;
        loop->bodyScope = newScope;
        loop->expression = newOperand;

        scope->children.push_back(loop);

    }

    Lex::Token parseForEachLoop(Location* const loc, Scope* const scope) {
        
        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(loc, &tokenVal);

        Loop* const loop = new Loop();
        loop->loc = getLocationStamp(loc);

        Scope* outerScope = new Scope();
        outerScope->fcn = currentFunction;
        outerScope->scope = scope;
        setParentIdx(outerScope);

        loop->scope = outerScope;

        Variable* newVar = new Variable(loop->scope, DT_UNDEFINED, loc);
        token = parseExpression(loc, newVar, Lex::TK_STATEMENT_END, 1);
        if (token.kind < 0) return token;

        if (token.kind != Lex::TK_KEYWORD || token.detail != KW_USING) {
            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_VARIABLE_NAME), loc, 1, "Variable name is matching key word name!");
            return Lex::toToken(Err::INVALID_VARIABLE_NAME);
        }

        loop->array = newVar;

        // var or var def (only basic dtypes)
        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind == Lex::TK_KEYWORD) {
            // var def
            
            if (!Lex::isInt((Lex::Keyword) token.detail)) {
                Logger::log(Logger::ERROR, "Only integral datatypes allowed!", loc, 1);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            loop->idx = NULL;
            loop->idxDef = createEmptyVariableDefinition();
            loop->idxDef->var->cvalue.dtypeEnum = Lex::toDtype(token);
            
            token = parseVariableDefinition(loc, outerScope, 0, Lex::TK_STATEMENT_END, &(loop->idxDef));
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
        
        }

        loop->bodyScope = new Scope();
        loop->bodyScope->fcn = currentFunction;
        loop->bodyScope->scope = loop->scope;
        setParentIdx(loop->bodyScope);

        SyntaxNode* tmpLoop = currentLoop;
        currentLoop = loop;
        
        token = parseScope(loc, loop->bodyScope);
        if (token.kind < 0) return token;
        
        currentLoop = tmpLoop;

        SyntaxNode::loops.push_back(loop);
        scope->children.push_back(loop);

    }

    Lex::Token parseGotoStatement(Location* const loc, Scope* const scope) {

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(loc, &tokenVal);
        
        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(Logger::ERROR, "Identifier expected!", loc);
            return token.encoded < 0 ? token : Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        GotoStatement* gt = new GotoStatement();
        gt->loc = getLocationStamp(loc);
        gt->scope = scope;
        gt->name = tokenVal.str->buff;
        gt->nameLen = tokenVal.str->len;
        gt->label = NULL;

        scope->children.push_back(gt);
        scope->gotos.push_back(gt);
        SyntaxNode::gotos.push_back(gt);

    }

    Lex::Token parseContinueStatement(Location* const loc, Scope* const scope) {

        if (!currentLoop) {
            Logger::log(Logger::ERROR, "Continue statement used outside of the loop!");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        ContinueStatement* cnt = new ContinueStatement();
        cnt->loc = getLocationStamp(loc);
        cnt->scope = scope;

        scope->children.push_back(cnt);

        return Lex::toToken(Lex::TD_KW_CONTINUE);

    }

    Lex::Token parseBreakStatement(Location* const loc, Scope* const scope) {

        if (!currentLoop) {
            Logger::log(Logger::ERROR, "Break statement used outside of the loop!");
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        BreakStatement* cnt = new BreakStatement();
        cnt->loc = getLocationStamp(loc);
        cnt->scope = scope;

        scope->children.push_back(cnt);

        return Lex::toToken(Lex::TD_KW_BREAK);

    }

    Lex::Token parseNamespace(Location* const loc, Scope* const scope) {

        if (currentFunction) {
            Logger::log(Logger::ERROR, "Namespace cannot be declared inside function!");
            return Lex::toToken(Err::INVALID_DECLARATION_PLACE);
        }

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(loc, &tokenVal);

        Namespace* nsc = new Namespace();
        nsc->type = NT_NAMESPACE;
        nsc->scope = scope;

        nsc->loc = getLocationStamp(loc);
        nsc->name = tokenVal.str->buff;
        nsc->nameLen = tokenVal.str->len;
        setParentIdx(nsc);
        
        token = Lex::nextToken(loc);
        if (token.kind != Lex::TK_SCOPE_BEGIN && token.kind != Lex::TK_STATEMENT_BEGIN) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = parseScope(loc, nsc, SC_COMMON, token.kind == Lex::TK_STATEMENT_BEGIN);
        if (token.kind) return token;

        scope->children.push_back(nsc);
        scope->namespaces.push_back(nsc);

    }

    Lex::Token parseAllocStatement(Location* const loc, Scope* const scope) {

    }

    Lex::Token parseFreeStatement(Location* const loc, Scope* const scope) {

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
        token = parseExpression(loc, inVar, Lex::TK_STATEMENT_END);
        if (token.encoded < 0) return token;

        fcnCall->inArgsCnt = 1;
        fcnCall->inArgs.push_back(inVar);

        Variable* wrapper = new Variable(scope);
        wrapper->loc = getLocationStamp(loc);
        wrapper->expression = fcnCall;

        Statement* st = new Statement();
        st->scope = scope;
        st->op = wrapper;

        SyntaxNode::fcnCalls.push_back(wrapper);
        scope->children.push_back(st);

    }

    Lex::Token parseReturnStatement(Location* const loc, Scope* const scope) {

        Lex::Token token;

        ReturnStatement* ret = new ReturnStatement();
        ret->fcn = currentFunction;
        ret->loc = getLocationStamp(loc);
        ret->scope = scope;

        ret->var = NULL;
        ret->err = NULL;

        ret->idx = currentFunction->returns.size();
        currentFunction->returns.push_back(ret);

        scope->children.push_back(ret);
        SyntaxNode::returnStatements.push_back(ret);

        token = Lex::nextToken(loc);
        if (token.kind == Lex::TK_STATEMENT_END) return token;

        // var case
        if (token.kind == Lex::TK_END) {
            // ERROR
            
            Logger::log(Logger::ERROR, "Unexpected end of file!", loc, 1);
            return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
        
        } else if (token != Lex::TK_SKIP) {

            Variable* newVar = new Variable(scope, DT_I64, loc);

            token = parseExpression(loc, newVar, Lex::packTokens(Lex::TK_LIST_SEPARATOR, Lex::TK_STATEMENT_END));
            if (token.encoded < 0) return token;

            ret->var = newVar;
        
        }

        token = Lex::nextToken(loc);
        if (token.kind == Lex::TK_STATEMENT_END) return token;

        if (token.kind != Lex::TK_LIST_SEPARATOR) {
            // ERROR    
            Logger::log(Logger::ERROR, "Unexpected symbol!", loc, 1);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        // error case
        Variable* newVar = new Variable(scope, DT_I64, loc);

        token = parseExpression(loc, newVar, Lex::TK_STATEMENT_END);
        if (token.kind < 0) return token;

        ret->err = newVar;
        
    }

    Lex::Token parseUnion(Location* const loc) {

        Logger::log(Logger::ERROR, "TODO error : Use 'def union' to define union!", loc, 1);
        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

    Lex::Token parseEnumInitialization(Location* const loc, Scope* const scope) {

        // enum <name> : <type> { .. }

        Lex::TokenValue tokenVal;
        Lex::Token token;

        Enumerator* enumerator = new Enumerator();
        enumerator->dtype = DT_INT;

        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(Logger::ERROR, "Identifier expected!", loc, 1);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        enumerator->name = tokenVal.str->buff;
        enumerator->nameLen = tokenVal.str->len;

        token = Lex::nextToken(loc);
        if (token.kind == Lex::TK_STATEMENT_BEGIN) {

            token = Lex::nextToken(loc, &tokenVal);
            if (token.kind != Lex::TK_KEYWORD) {
                Logger::log(Logger::ERROR, "Data type expected!", loc, 1);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }

            if (!Lex::isInt((Lex::Keyword) token.detail)) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_DATA_TYPE), loc);
                return Lex::toToken(Err::UNKNOWN_DATA_TYPE);
            }

            enumerator->dtype = Lex::toDtype((Lex::Keyword) token.detail);

            token = Lex::nextToken(loc);

        }

        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(Logger::ERROR, "'{' is expected!", loc, 1);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        enumerator->loc = getLocationStamp(loc);

        enumerator->id = defId;
        defId++;

        ASSIGN_ID(enumerator);

        uint64_t lastValue = -1; //((uint64_t) 0) - ((uint64_t) 1);
        while (1) {

            token = Lex::nextToken(loc, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            VariableDefinition* newVarDef = new VariableDefinition();
            Variable* newVar = new Variable(scope, enumerator->dtype, loc);

            newVarDef->var = newVar;
            newVarDef->var->cvalue.hasValue = 0;
            newVarDef->var->cvalue.dtypeEnum = enumerator->dtype;
            newVarDef->loc = getLocationStamp(loc);
            newVarDef->flags = IS_CMP_TIME;

            newVar->def = newVarDef;
            newVar->name = tokenVal.str->buff;
            newVar->nameLen = tokenVal.str->len;
            newVar->unrollExpression = 1;

            ASSIGN_ID(newVar);

            enumerator->vars.push_back(newVar);

            token = Lex::nextToken(loc);
            if (token.kind == Lex::TK_EQUAL) {

                token = parseExpression(loc, newVar, Lex::packTokens(Lex::TK_LIST_SEPARATOR, Lex::TK_SCOPE_END));
                if (token.encoded < 0) return token;

                newVarDef->var->cvalue.hasValue = 1;

            }

            if (token.kind == Lex::TK_LIST_SEPARATOR) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;

            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        
        }

        SyntaxNode::enumerators.push_back(enumerator);
        scope->children.push_back(enumerator);
        scope->enums.push_back(enumerator);
    
    }
    
    Lex::Token parseTypeInitialization(Location* const loc, Scope* const scope) {

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(loc, &tokenVal);

        Location* defLoc = getLocationStamp(loc);

        Lex::Keyword keyword;
        if (token.kind == Lex::TK_KEYWORD) { 

            if (token.detail == Lex::TD_KW_UNION || token.detail == Lex::TD_KW_STRUCT) {
                keyword = (Lex::Keyword) token.detail;
            } else {
                Logger::log(Logger::ERROR, "Unexpected symbol!", loc, 1);
                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
            }
            
            token = Lex::nextToken(loc);
        
        } else {
            
            keyword = Lex::KW_STRUCT;
        
        }

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(Logger::ERROR, "Identifier expected!", loc, 1);
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
        newTypeDefinition->parentIdx = SyntaxNode::customDataTypes.size();
        newTypeDefinition->loc = defLoc;

        newTypeDefinition->id = defId;
        defId++;

        token = Lex::nextToken(loc);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        SyntaxNode::customDataTypes.push_back(newTypeDefinition);
        scope->customDataTypes.push_back(newTypeDefinition);
        scope->children.push_back(newTypeDefinition);

        while (1) {

            token = Lex::nextToken(loc);
            if (token.kind == Lex::TK_SCOPE_END) break;

            VariableDefinition* varDef;
            token = parseVariableDefinition(loc, scope, 0, Lex::packTokens(Lex::TK_SCOPE_END, Lex::TK_STATEMENT_END), &varDef);
            if (token.kind < 0) return token;
            
            newTypeDefinition->vars.push_back(varDef->var);
            
            token = Lex::nextToken(loc);
            if (token.kind == Lex::TK_STATEMENT_END) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;
                
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

    }

    Lex::Token parseErrorDeclaration(Location* const loc, Scope* scope) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            // ERROR
            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        VariableDefinition* def = createEmptyVariableDefinition();
        def->var->name = tokenVal.str->buff;
        def->var->nameLen = tokenVal.str->len;
        def->var->cvalue.dtypeEnum = DT_ERROR;
        def->var->loc = getLocationStamp(loc);
        def->scope = scope;
        setParentIdx(def->var);

        auto res = def->scope->defSearch.insert({std::string_view(def->var->name, def->var->nameLen), def->var});
        if (!res.second) {
            Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), loc, nameLen);
            return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
        }

        ASSIGN_ID(def->var);

        token = Lex::nextToken(loc, NULL);
        if (token.kind == Lex::TK_EQUAL) {

            token = parseExpression(loc, def->var, Lex::TK_STATEMENT_END);
            if (token.kind < 0) return token;

        } else if (token.kind != Lex::TK_STATEMENT_END) {

            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc, 1);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        SyntaxNode::variableDefinitions.push_back(def);
        scope->defs.push_back(def->var);
        scope->children.push_back(def);

    }

    Lex::Token parseErrorDefinition(Location* const loc, Scope* const scope) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        token = Lex::nextToken(loc, NULL);

        ErrorSet* errorSet = new ErrorSet();
        errorSet->scope = scope;
        errorSet->name = tokenVal.str->buff;
        errorSet->nameLen = tokenVal.str->len;
        errorSet->value = errId;
        errId++;

        ASSIGN_ID(errorSet);

        while (1) {

            token = Lex::nextToken(loc, &tokenVal);
            if (token.kind == Lex::TK_SCOPE_END) break;

            Variable* var = new Variable();
            var->scope = scope;
            var->name = tokenVal.str->buff;
            var->nameLen = tokenVal.str->len;
            var->loc = getLocationStamp(loc);
            var->cvalue.hasValue = 0;
            var->cvalue.dtypeEnum = DT_ERROR;
            var->cvalue.u64 = errId;
            errId++;

            errorSet->vars.push_back(var);

            token = Lex::nextToken(loc, &tokenVal);
            if (token.kind == Lex::TK_STATEMENT_END) continue;
            if (token.kind == Lex::TK_SCOPE_END) break;

            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);

        }

        SyntaxNode::customErrors.push_back(errorSet);
        scope->customErrors.push_back(errorSet);
        scope->children.push_back(errorSet);

    }

    Lex::Token parseError(Location* const loc, Scope* const scope) {

        Lex::TokenValue tokenVal;
        Lex::Token token = Lex::nextToken(loc, &tokenVal);

        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(Logger::ERROR, ERR_STR(Err::MISSING_VARIABLE_NAME), loc, 1);
            return Lex::toToken(Err::MISSING_VARIABLE_NAME);
        }

        token = Lex::nextToken(loc);
        if (token.kind != Lex::TK_SCOPE_BEGIN) {
            
            token = parseErrorDeclaration(loc, scope);
            if (token.encoded < 0) return token;
        
        }

        token = parseErrorDefinition(loc, scope);
        if (token.encoded < 0) return token;
        
    }

    Lex::Token parseImport(Location* const loc, Scope* const scope) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        ImportStatement* import = new ImportStatement();
        // import->root = fileRootScope;
        import->scope = scope;

        ImportNode* importNode = new ImportNode;
        importNode->import = import;
        importNode->parent = importCurrent;

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Lex::toToken(Err::UNEXPECTED_END_OF_FILE);
        import->loc = getLocationStamp(loc);

        token = Lex::nextToken(loc, &tokenVal);
        
        if (token.kind == Lex::TK_FILE_NAME) {

            import->fname = *tokenVal.str;
            import->keyWord = (KeyWordType) -1;
            importCurrent->children.push_back(importNode);

        } else if (token.kind == Lex::TK_KEYWORD && token.detail == Lex::TD_KW_FROM) {

            import->fname = *tokenVal.str;
            import->keyWord = (KeyWordType) -1;
            importCurrent->children.push_back(importNode);

        }

        token = Lex::nextToken(loc, &tokenVal);
        if (!Lex::isKeyword(token, Lex::KW_AS)) {
            Logger::log(Logger::ERROR, "Keyword 'as' expected!", loc, Utils::findVarEnd(str + loc->idx));
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind != Lex::TK_KEYWORD) {
            Logger::log(Logger::ERROR, "Unsupported value is used! Use one of the following : 'namespace', 'fcn', 'scope'\n", loc, Utils::findVarEnd(str + loc->idx));
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        if (token.detail != KW_NAMESPACE && token.detail != KW_SCOPE && token.detail != KW_FUNCTION) {
            Logger::log(Logger::ERROR, "Unsupported value is used! Use one of the following : 'namespace', 'fcn', 'scope'\n", loc, Utils::findVarEnd(str + loc->idx));
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        import->keyWord = (KeyWordType) token.detail;

        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind != Lex::TK_IDENTIFIER) {
            Logger::log(Logger::ERROR, "Identifier expected!", loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        token = Lex::nextToken(loc, &tokenVal);
        if (token.kind != Lex::TK_STATEMENT_END) {
            Logger::log(Logger::ERROR, "End of statement expected!", loc);
            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
        }

        importCurrent->children.push_back(importNode);

        return token;

    }

    Lex::Token parseDirective(Location* const loc, Scope* const scope, Lex::Directive directive, uint64_t param) { 
        
        switch (directive) {

            case Lex::CD_TEST: {
                return Lex::toToken(Lex::TK_STATEMENT_END);
            }

        }

        return Lex::toToken(Err::UNEXPECTED_SYMBOL);

    }

    Lex::Token parseList(Location* const loc, ) {

    }

    enum {
        EP_NONE,
        EP_VARIABLE,
        EP_STRING_LITERAL,
        EP_NUMBER_LITERAL,
        EP_OPERATOR,
        EP_CATCH,
        EP_SUB_EXPRESSION,
    };

    // expression is something like this
    // x = 10 * 1 + y + z * sin(u) + tmp[0];
    // states
    // 0 -> unary operator or value/var is expected
    // 1 -> binary operator or value/var is expected
    // expresion has to have at least one operator, otherwise it has to be NULL and value is represented directly in variable
    // if second half (endChar >> 8) of endChar is 1 'exclusive end' is enabled -> in this case only first half of endChar is used as end char
    // in case that 1 is requaried as end check, can be used in first part
    // if useKeyWordAsEnd is true, then possitive return value is KeyWordType of the found key word
    Lex::Token parseExpression(Location* const loc, Variable* operand, const End endToken, const int useKeywordAsEnd, const int emptyExpressionAllowed, const int defIdx) {

        Lex::Token token;
        Lex::TokenValue tokenVal;

        const int lastDefIdx = (defIdx < 0) ? operand->scope->children.size() : defIdx;

        int lowestOperatorRank = -1;
        Variable* lowestBinaryOperand = NULL;
        

        int lastOperatorRank = -1;
        UnaryExpression* lastUnaryExpression = NULL;
        BinaryExpression* lastBinaryExpression = NULL;
        int lastType = EP_NONE;
        int lastOperator = -1;
        int lastOperandType = DT_UNDEFINED;
        Variable* lastVariable = NULL;
        FunctionCall* lastFunctionCall = NULL;

        while (1) {

            token = Lex::nextToken(loc, &tokenVal);

            if (token.kind == endToken.a || token.kind == endToken.b) {
                // end of expression

                if (!emptyExpressionAllowed && lastType == EP_NONE) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_EXPRESSION), loc, 1);
                    return Lex::toToken(Err::UNEXPECTED_END_OF_EXPRESSION);
                }

                if (lastType == EP_OPERATOR) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_EXPRESSION), loc);
                    return Lex::toToken(Err::UNEXPECTED_END_OF_EXPRESSION);
                }

                return token;
            
            } else if (lastType == EP_CATCH) {
                
                Logger::log(Logger::ERROR, "TODO error : catch expression cannot be used within other expression!", loc, strlen(KWS_CATCH));
                return Err::UNEXPECTED_SYMBOL;
            
            } else if (token.kind == Lex::TK_PARENTHESIS_BEGIN) {
                // subexpression start

                Variable* newOperand = new Variable();
                newOperand->cvalue.dtypeEnum = DT_UNDEFINED; // TODO : exchange for UNDEFINED or whatever!!!!!
                newOperand->scope = operand->scope;
                newOperand->unrollExpression = 1;
                newOperand->cvalue.ptr = NULL;

                const int err = parseExpression(newOperand, str, loc, ')', 0, 0, lastDefIdx);
                if (err < 0) return err;

                if (lastType == EP_NONE) {

                    // TODO: typecheck
                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = newOperand;
                    operand->expression = ex;

                } else {

                    if (lastUnaryExpression) lastUnaryExpression->operand = newOperand;
                    else lastBinaryExpression->operandB = newOperand;

                }

                loc->idx--;
                lastType = EP_VARIABLE;

            } else if (token.kind == Lex::STRING_LITERAL) {

                Variable* newOperand = new Variable();
                newOperand->cvalue.dtypeEnum = DT_STRING;
                newOperand->unrollExpression = 0;
                
                newOperand->id = arrId;
                arrId++;

                if (lastType == EP_NONE) {
                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = newOperand;
                    operand->expression = ex;
                } else {
                    if (lastUnaryExpression) lastUnaryExpression->operand = newOperand;
                    else lastBinaryExpression->operandB = newOperand;
                }

                loc->idx--;
                lastType = EP_VARIABLE;

            } else if (token.kind == Lex::CHAR_LITERAL) {

                Variable* var = new Variable();
                var->unrollExpression = 0;
                var->cvalue.dtypeEnum = dtype;
                var->cvalue.i64 = charInt;
                var->cvalue.hasValue = 1;
                var->expression = NULL;

                // TODO : refactor so can be used generaly
                if (lastType == EP_NONE) {
                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = var;
                    operand->expression = ex;
                } else {
                    if (lastUnaryExpression) lastUnaryExpression->operand = var;
                    else lastBinaryExpression->operandB = var;
                }

                lastType = EP_VARIABLE;

            } else if (token.kind == Lex::ARRAY_BEGIN && lastType != EP_VARIABLE) {

                Variable* newOperand = new Variable();
                newOperand->cvalue.dtypeEnum = DT_ARRAY;
                newOperand->unrollExpression = 0;

                newOperand->id = arrId;
                arrId++;

                const int err = parseArrayInitialization(str, loc, (ArrayInitialization**) &(newOperand->expression));
                if (err < 0) return err;

                if (lastType == EP_NONE) {
                    // TODO: typecheck

                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = newOperand;
                    operand->expression = ex;

                }
                else {

                    if (lastUnaryExpression) lastUnaryExpression->operand = newOperand;
                    else lastBinaryExpression->operandB = newOperand;

                }

                lastType = EP_VARIABLE;
            
            } else if (token.kind == Lex::SCOPE_BEGIN) {
                // custom type

                Variable* newOperand = new Variable();
                // newOperand->expression = typeInit;
                newOperand->cvalue.dtypeEnum = DT_CUSTOM;
                newOperand->unrollExpression = 0;

                const int err = parseTypeInitialization(operand->scope, str, loc, (TypeInitialization **)&(newOperand->expression));
                if (err < 0) return err;

                if (lastType == EP_NONE) {
                    // TODO: typecheck
                    
                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = newOperand;
                    operand->expression = ex;

                } else {

                    if (lastUnaryExpression) lastUnaryExpression->operand = newOperand;
                    else lastBinaryExpression->operandB = newOperand;
                
                }

                lastType = EP_VARIABLE;
            
            } else if (token.kind == Lex::TK_IDENTIFIER) {
                
                Location* startLocation = getLocationStamp(loc);

                token = Lex::nextToken(loc, NULL);
                if (token.kind != Lex::TK_PARENTHESIS_BEGIN) {

                    Variable* var = new Variable();
                    var->scope = operand->scope;
                    var->loc = startLocation;
                    var->flags = 0;
                    var->unrollExpression = 0;
                    var->scopeNames = scopeNames;

                    var->name = tokenVal.str->buff;
                    var->nameLen = tokenVal.str->len;

                    if (lastOperator != OP_MEMBER_SELECTION) { 
                        lastVariable = var;
                        var->parentIdx = lastDefIdx;
                        SyntaxNode::variables.push_back(var);
                    } else {
                        var->cvalue.dtypeEnum = DT_MEMBER;
                        var->id = 0;
                    }

                    if (lastType == EP_NONE) {

                        WrapperExpression* ex = new WrapperExpression();
                        ex->operand = var;
                        operand->expression = ex;
                    
                    } else {

                        if (lastUnaryExpression) lastUnaryExpression->operand = var;
                        else if (lastBinaryExpression) lastBinaryExpression->operandB = var;
                        else {
                            Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_SYMBOL), var->loc, var->nameLen);
                            return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                        }
                    
                    }
                
                } else {

                    FunctionCall* fcnCall = new FunctionCall;
                    fcnCall->fptr = NULL;
                    fcnCall->fcn = NULL;
                    fcnCall->name = tokenVal.str->buff;
                    fcnCall->nameLen = tokenVal.str->len;
                    fcnCall->scopeNames = scopeNames;

                    lastFunctionCall = fcnCall;

                    token = parseList(loc, scope, Lex::TK_LIST_SEPARATOR, Lex::TK_PARENTHESIS_END, fcnCall->inArgs);
                    if (token.encoded < 0) {
                        // ERROR
                    }
                    
                    fcnCall->inArgsCnt = fcnCall->inArgs.size();

                    Variable* newOperand = new Variable;
                    newOperand->cvalue.dtypeEnum = DT_UNDEFINED;
                    newOperand->expression = fcnCall;
                    newOperand->scope = operand->scope;
                    newOperand->unrollExpression = 1;
                    newOperand->cvalue.ptr = NULL;
                    newOperand->loc = startLocation;
                    newOperand->parentIdx = lastDefIdx;

                    if (lastType == EP_NONE) {

                        WrapperExpression* ex = new WrapperExpression();
                        ex->operand = newOperand;
                        operand->expression = ex;
                    
                    } else {

                        if (lastUnaryExpression) lastUnaryExpression->operand = newOperand;
                        else lastBinaryExpression->operandB = newOperand;
                    
                    }

                    SyntaxNode::fcnCalls.push_back(newOperand);
                
                }

                lastType = EP_VARIABLE;

            } else if (token.kind == Lex::TK_KEYWORD) {

                Location* startLocation = getLocationStamp(loc);

                if (token.detail == Lex::TD_KW_CATCH) {
                    
                    if (lastUnaryExpression || lastBinaryExpression) {
                        Logger::log(Logger::ERROR, "TODO error : catch expression cannot be used within other expression!", loc, strlen(KWS_CATCH));
                        return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                    }

                    if (!lastFunctionCall) {
                        Logger::log(Logger::ERROR, "TODO error : 'catch' keyword can only be used after function call!", loc, strlen(KWS_CATCH));
                        return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                    }

                    int returnErr = 0;

                    Catch* cex = new Catch();
                    cex->call = (FunctionCall*) lastFunctionCall;

                    Location* varLoc = getLocationStamp(loc);

                    token = Lex::nextToken(loc, &tokenVal);
                    if (Lex::isKeyword(token, Lex::KW_RETURN)) {
                        returnErr = 1;
                    } else if (token.kind == Lex::TK_IDENTIFIER) {
                        token = Lex::nextToken(loc, NULL);
                    } else {
                        // ERROR
                    }

                    if (token.kind == Lex::TK_STATEMENT_END) {

                        Variable* var = new Variable(operand->scope, DT_ERROR, loc);
                        var->name = tokenVal.str->buff;
                        var->nameLen = tokenVal.str->len;
                        var->loc = varLoc;
                        var->parentIdx = lastDefIdx;

                        SyntaxNode::variables.push_back(var);

                        cex->err = var;
                        cex->scope = NULL;

                    } else if (token.kind == Lex::TK_SCOPE_BEGIN || returnErr) {

                        Scope* newScope = new Scope();
                        newScope->scope = operand->scope;
                        setParentIdx(newScope);
                        //pushDefLike(operand->scope->defSearch, newScope);

                        VariableDefinition* errDef = createEmptyVariableDefinition();
                        errDef->scope = newScope;
                        errDef->var->name = tokenVal.str->buff;
                        errDef->var->nameLen = tokenVal.str->len;
                        errDef->var->scope = newScope;
                        errDef->var->cvalue.dtypeEnum = DT_ERROR;
                        errDef->var->loc = varLoc;
                        errDef->var->parentIdx = -1;

                        ASSIGN_ID(errDef->var);

                        newScope->defs.push_back(errDef->var);
                        SyntaxNode::variableDefinitions.push_back(errDef);

                        auto res = newScope->defSearch.insert({std::string_view(tokenVal.str->buff, tokenVal.str->len), errDef->var});
                        if (!res.second) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), varLoc, tokenVal.str->len);
                            return Lex::toToken(Err::SYMBOL_ALREADY_DEFINED);
                        }

                        cex->err = errDef->var;
                        cex->scope = newScope;

                        operand->expression = cex;

                        if (returnErr) {
                            // basically just emulate following
                            // .. .. catch err { return _, err; }
                            
                            ReturnStatement* ret = new ReturnStatement();
                            ret->err = errDef->var;
                            ret->var = NULL;
                            ret->scope = newScope;
                            ret->fcn = currentFunction;

                            newScope->children.push_back(ret);
                            SyntaxNode::returnStatements.push_back(ret);

                            cex->err = errDef->var;
                            cex->scope = newScope;

                            operand->expression = cex;

                            token = Lex::nextToken(loc, NULL);
                            if (token.kind != Lex::TK_STATEMENT_END) {
                                Logger::log(Logger::ERROR, "'catch return' expression has to be terminated with ';'!", startLocation);
                                return Lex::toToken(Err::UNEXPECTED_SYMBOL);
                            }

                            return token;

                        }

                        token = parseScope(loc, newScope, SC_COMMON);

                    }

                    operand->expression = cex;
                    return token;                    

                }

                if (useKeywordAsEnd) {
                
                    if (lastType == EP_OPERATOR) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_EXPRESSION), loc);
                        return Lex::toToken(Err::UNEXPECTED_END_OF_EXPRESSION);
                    }

                    return token;

                } else {

                    Logger::log(Logger::ERROR, "Variable name is matching key word name!", loc, wordLen);
                    return Err::INVALID_VARIABLE_NAME;

                }

            } else if (token.kind == Lex::TK_NUMBER) {

                if (lastType != EP_OPERATOR && lastType != EP_NONE) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_NUMBER_LITERAL), loc);
                    return Lex::toToken(Err::INVALID_NUMBER_LITERAL);
                }

                Variable* newVar = new Variable(operand->scope);
                newVar->loc = getLocationStamp(loc);
                newVar->cvalue.i64 = tokenVal.ival;
                newVar->cvalue.hasValue = 1;
                newVar->cvalue.dtypeEnum = Lex::toDtype(token);

                if (lastType == EP_NONE) {

                    newVar->unrollExpression = 1;

                    WrapperExpression* ex = new WrapperExpression();
                    ex->operand = newVar;
                    operand->expression = ex;
                
                } else {

                    newVar->unrollExpression = 0; // LOOK AT : do we have to set it?

                    if (lastUnaryExpression) lastUnaryExpression->operand = newVar;
                    else lastBinaryExpression->operandB = newVar;
                
                }

                lastType = EP_NUMBER_LITERAL;

            } else if (token.detail == Lex::TK_ARRAY_BEGIN) {
                // lets keep it hardcoded for now
                    
                // either slice or single access
            
                if (lastType == EP_NONE || lastType == EP_OPERATOR) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_USAGE_OF_OPERATOR), loc, 1, 5, "[...]");
                    return Lex::toToken(Err::INVALID_USAGE_OF_OPERATOR);
                }

                Variable* idxOperand = new Variable();
                idxOperand->cvalue.dtypeEnum = DT_UNDEFINED;
                idxOperand->scope = operand->scope;
                idxOperand->unrollExpression = 1;
                idxOperand->cvalue.ptr = NULL;

                token = parseExpression(loc, idxOperand, Lex::packTokens(Lex::TK_ARRAY_END, Lex::TK_STATEMENT_BEGIN), 0, 1, lastDefIdx);
                if (token.encoded < 0) return token;

                Variable* operandB = idxOperand;
            
                // TODO : merge
                if (token.kind == Lex::TK_STATEMENT_BEGIN) {
                    // slice

                    Variable* idxOperand2 = new Variable();
                    idxOperand2->cvalue.dtypeEnum = DT_UNDEFINED;
                    idxOperand2->scope = operand->scope;
                    idxOperand2->unrollExpression = 1;
                    idxOperand2->cvalue.ptr = NULL;

                    token = parseExpression(loc, idxOperand2, ARRAY_END, 0, 1, lastDefIdx);
                    if (token.encoded < 0 && token.encoded != Err::UNEXPECTED_END_OF_EXPRESSION) return err;

                    Slice* slice = new Slice;
                    slice->bidx = idxOperand;
                    slice->eidx = idxOperand2;

                    Variable* newOperand = new Variable();
                    newOperand->cvalue.dtypeEnum = DT_UNDEFINED;
                    newOperand->scope = operand->scope;
                    newOperand->unrollExpression = 1;
                    newOperand->expression = slice;
                    
                    newOperand->id = arrId;
                    arrId++;

                    Variable* arr;
                    if (lastUnaryExpression) {

                        arr = lastUnaryExpression->operand;
                        lastUnaryExpression->operand = newOperand;
                    
                    } else if (lastBinaryExpression) {
                        
                        arr = lastBinaryExpression->operandB;
                        ((BinaryExpression*) (operand->expression))->operandB = newOperand;
                    
                    } else {
                        
                        arr = ((WrapperExpression*) (operand->expression))->operand;
                        ((WrapperExpression*) (operand->expression))->operand = newOperand;
                    
                    }

                    slice->arr = arr;

                    // SyntaxNode::slices.push_back(slice);

                    lastVariable = newOperand;
                    lastType = EP_VARIABLE;

                    continue;

                }
            
                BinaryExpression* bEx = new BinaryExpression;
                bEx->operandB = idxOperand;
                bEx->operType = OP_SUBSCRIPT;
                // bEx->oper = operators + OP_SUBSCRIPT;

                Variable* newOperand = new Variable();
                newOperand->cvalue.dtypeEnum = DT_UNDEFINED;
                newOperand->scope = operand->scope;
                newOperand->unrollExpression = 1;
                newOperand->cvalue.ptr = NULL;
                newOperand->expression = bEx;

                if (lastUnaryExpression) {

                    bEx->operandA = lastUnaryExpression->operand;
                    lastUnaryExpression->operand = newOperand;
                
                } else if (lastBinaryExpression) {
                    
                    // for now allways treated as highest priority
                    bEx->operandA = lastBinaryExpression->operandB;
                    lastBinaryExpression->operandB = newOperand;
                
                } else {

                    bEx->operandA = ((WrapperExpression*) (operand->expression))->operand;
                    ((WrapperExpression*) (operand->expression))->operand = newOperand;
                
                }

                // operand->expression = bEx;

                lastVariable = newOperand;
                lastType = EP_VARIABLE;

            } else {
                // operator

                // operator can be merged with other word...
                uint32_t word = ch;
                OperatorEnum opType;
                
                if (lastType == EP_NONE) {

                    opType = findUnaryOperator(word);
                    if (opType <= OP_NONE) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_OPERATOR), loc, wordLen, wordLen, &word);
                        return Err::INVALID_OPERATOR;
                    }

                        UnaryExpression* uEx = new UnaryExpression;
                        uEx->operand = NULL;
                        uEx->operType = opType;
                        // uEx->oper = operators + opType;

                        operand->expression = uEx;
                        lastUnaryExpression = uEx;
                        lastType = EP_OPERATOR;

                    } else if (lastType == EP_OPERATOR) {

                        opType = findUnaryOperator(word);
                        if (!opType <= OP_NONE) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_OPERATOR), loc, wordLen, wordLen, &word);
                            return Err::INVALID_OPERATOR;
                        }

                        UnaryExpression* uEx = new UnaryExpression;
                        uEx->operand = NULL;
                        uEx->operType = opType;
                        // uEx->oper = operators + opType;

                        Variable* newVar = new Variable(loc);
                        newVar->scope = operand->scope;
                        newVar->expression = uEx;

                        if (lastUnaryExpression) {
                            lastUnaryExpression->operand = newVar;
                        } else {
                            lastBinaryExpression->operandB = newVar;
                        }

                        lastUnaryExpression = uEx;
                    
                    } else if (opType = findPostfixUnaryOperator(word), opType > OP_NONE) {

                        Variable* newVar = new Variable(loc);
                        newVar->scope = operand->scope;
                        newVar->expression = operand->expression;

                        UnaryExpression* uEx = new UnaryExpression;
                        uEx->operand = newVar;
                        uEx->operType = opType;
                        // uEx->oper = operators + opType;

                        operand->expression = uEx;

                        lastUnaryExpression = uEx;
                        lastType = EP_VARIABLE;
                    
                    } else {

                        opType = findBinaryOperator(word);
                        if (opType <= OP_NONE) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_OPERATOR), loc, wordLen, wordLen, &word);
                            return Err::INVALID_OPERATOR;
                        }

                        Operator op = operators[opType];

                        lastUnaryExpression = NULL;

                        BinaryExpression* bEx = new BinaryExpression;

                        Variable* newVar = new Variable(loc);
                        newVar->scope = operand->scope;

                        // LOOK_AT: maybe change design to allways somehow have root as expression, so we dont have to check or something
                        // lower rank value has higher precedence
                        // if next operator has higher precedence
                        //      : new expression is created and previous operand takes its at 'a' slot
                        //      : this expresion is then expresion of the 'b' slot of current expression
                        //      : new expression is then capsulated from previous
                        // if nex operand has lower precedence
                        //      : whole expression is capsulated
                        //      : new expression is new root with 'a' as previous one
                        //      : new expression is then executed as last
                        const int operatorRank = op.rank;
                        if (lastOperatorRank > operatorRank) {
                            // higher precedence

                            Variable* tmpVar = lastBinaryExpression->operandB;
                            lastBinaryExpression->operandB = newVar;
                            
                            bEx->operandA = tmpVar;
                            bEx->operandB = NULL;
                            bEx->operType = opType;

                            newVar->expression = bEx; 

                        } else {
                            // lower precedence
                            
                            if (lowestOperatorRank == -1) {
                                // initial
                                newVar->unrollExpression = operand->unrollExpression;
                                newVar->cvalue = operand->cvalue;
                                newVar->expression = operand->expression;

                                bEx->operandA = newVar;
                                bEx->operandB = NULL;
                                bEx->operType = opType;

                                operand->expression = bEx;

                                lowestOperatorRank = operatorRank;
                                lowestBinaryOperand = operand;
                            
                            } else if (lowestOperatorRank > operatorRank) {
                                
                                BinaryExpression* const tmpBEx = (BinaryExpression*) (lowestBinaryOperand->expression);
                                Variable* tmpVar = tmpBEx->operandB;
                                tmpBEx->operandB = newVar;

                                bEx->operandA = tmpVar;
                                bEx->operandB = NULL;
                                bEx->operType = opType;

                                newVar->expression = bEx;

                                lowestBinaryOperand = newVar;
                                lowestOperatorRank = operatorRank;
                            
                            } else {

                                Variable* const op = lowestBinaryOperand;

                                newVar->def = op->def;
                                newVar->unrollExpression = op->unrollExpression;
                                newVar->cvalue = op->cvalue;
                                newVar->expression = op->expression;

                                bEx->operandA = newVar;
                                bEx->operandB = NULL;
                                bEx->operType = opType;

                                op->expression = bEx;

                                lowestOperatorRank = operatorRank;
                                lowestBinaryOperand = op;
                            
                            }
                            //BinaryExpression* const tmpBEx = (BinaryExpression*) (lowestBinaryOperand->expression);
                            //Variable* tmpVar = tmpBEx->operandB;
                            //tmpBEx->operandB = newVar;

                            //bEx->operandA = tmpVar;
                            //bEx->operandB = NULL;
                            // bEx->operType = opType;

                            //newVar->expression = bEx;

                            //lowestBinaryOperand = newVar;
                            //lowestOperatorRank = operatorRank;
                            // bEx = lastBinaryExpression;

                        }

                        lastOperatorRank = operatorRank;
                        lastBinaryExpression = bEx;
                        lastType = EP_OPERATOR;

                    }

                    lastOperator = opType; // LOOK AT : is it any usefull
                    // lastType = EP_OPERATOR;

                    loc->idx += wordLen - 1;

                }
            
            }

            loc->idx++; // LOOK AT : get rid of it???
        
        }
    
    }

}