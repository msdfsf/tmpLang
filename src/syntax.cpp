#include <cstddef>
#include <cstdint>
#include <cstring>

#include "syntax.h"
#include "allocator.h"
#include "array_list.h"
#include "data_types.h"
#include "globals.h"
#include "keywords.h"
#include "logger.h"
#include "operators.h"
#include "string.h"
#include "diagnostic.h"



const Logger::Type logErr = { .level = Logger::ERROR, .tag = stageTag };

Variable Ast::Internal::variables[Ast::Internal::IV_COUNT];
Function Ast::Internal::functions[Ast::Internal::IF_COUNT];

Scope* SyntaxNode::root = NULL;
INamed SyntaxNode::dir = { NULL, 0 };

AstContext* Ast::init() {

    AstContext* ctx = (AstContext*) alloc(alc, sizeof(AstContext));



    // === Registry
    //

    constexpr int initSize = 1024;

    ctx->reg = (AstRegistry*) alloc(alc, sizeof(AstRegistry));
    for (int i = 0; i < AstRegistry::dataSize; i++) {
        DArray::init(ctx->reg->data + i, initSize, sizeof(void*));
        (ctx->reg->data + i)->constCoef = 0;
        (ctx->reg->data + i)->constTerm = (ctx->reg->data + i)->allocSize;
    }



    // === Internals
    //

    ctx->usedFunctionMask = 0;

    Function* fPrintf = Internal::functions + Internal::IF_PRINTF;
    fPrintf->base.scope = SyntaxNode::root;
    fPrintf->name.buff = (char*) Internal::IFS_PRINTF;
    fPrintf->name.len = sizeof(Internal::IFS_PRINTF) - 1;
    fPrintf->internalIdx = Internal::IF_PRINTF;

    fPrintf->prototype.inArgs = (VariableDefinition**) alloc(alc, 2 * sizeof(VariableDefinition*));

    VariableDefinition* fPrintArg1 = (VariableDefinition*) nalloc(alc, NT_VARIABLE_DEFINITION);
    fPrintArg1->var = (Variable*) nalloc(alc, NT_VARIABLE);
    fPrintArg1->var->base.scope = SyntaxNode::root;
    fPrintArg1->var->value.typeKind = Type::DT_STRING;
    fPrintArg1->var->value.any = NULL;
    fPrintArg1->var->value.hasValue = 0;

    VariableDefinition* fPrintArg2 = (VariableDefinition*) nalloc(alc, NT_VARIABLE_DEFINITION);
    fPrintArg2->var = (Variable*) nalloc(alc, NT_VARIABLE);
    fPrintArg2->var->base.scope = SyntaxNode::root;
    fPrintArg2->var->value.typeKind = Type::DT_MULTIPLE_TYPES;
    fPrintArg2->var->value.any = NULL;
    fPrintArg2->var->value.hasValue = 0;

    fPrintf->prototype.inArgs[0] = fPrintArg1;
    fPrintf->prototype.inArgs[1] = fPrintArg2;



    Function* fAlloc = Internal::functions + Internal::IF_ALLOC;
    fAlloc->base.scope = SyntaxNode::root;
    fAlloc->name.buff = (char*) Internal::IFS_ALLOC;
    fAlloc->name.len = sizeof(Internal::IFS_ALLOC) - 1;
    fAlloc->internalIdx = Internal::IF_ALLOC;

    fAlloc->prototype.inArgs = (VariableDefinition**) alloc(alc, sizeof(VariableDefinition*));

    VariableDefinition* fAllocArg1 = (VariableDefinition*) nalloc(alc, NT_VARIABLE_DEFINITION);
    fAllocArg1->var = (Variable*) nalloc(nalc, NT_VARIABLE);
    fAllocArg1->var->base.scope = SyntaxNode::root;
    fAllocArg1->var->value.typeKind = Type::DT_U64;

    fAlloc->prototype.inArgs[0] = fAllocArg1;



    Function* fFree = Internal::functions + Internal::IF_FREE;
    fFree->base.scope = SyntaxNode::root;
    fFree->name.buff = (char*) Internal::IFS_FREE;
    fFree->name.len = sizeof(Internal::IFS_FREE) - 1;
    fFree->internalIdx = Internal::IF_FREE;

    fFree->prototype.inArgs = (VariableDefinition**) alloc(alc, 2 * sizeof(VariableDefinition*));

    VariableDefinition* fFreeArg1 = (VariableDefinition*) nalloc(nalc, NT_VARIABLE_DEFINITION);
    fFreeArg1->var = (Variable*) nalloc(nalc, NT_VARIABLE);
    fFreeArg1->var->base.scope = SyntaxNode::root;
    fFreeArg1->var->value.typeKind = Type::DT_POINTER;

    fFree->prototype.inArgs[0] = fFreeArg1;



    // internal Variables such as null, true, false etc...
    Variable* vNull = Internal::variables + Internal::IV_NULL;
    vNull->value = { Type::DT_POINTER, 1, 0 };
    vNull->base.scope = SyntaxNode::root;
    vNull->name.buff = (char*) Internal::IVS_NULL;
    vNull->name.len = sizeof(Internal::IVS_NULL) - 1;
    vNull->base.flags = IS_CMP_TIME;

    VariableDefinition* vNullDef = (VariableDefinition*) nalloc(nalc, NT_VARIABLE_DEFINITION);
    vNullDef->var = vNull;



    Variable* vTrue = Internal::variables + Internal::IV_TRUE;
    vTrue->base.scope = SyntaxNode::root;
    vTrue->value = { Type::DT_INT, 1, 1 };
    vTrue->name.buff = (char*) Internal::IVS_TRUE;
    vTrue->name.len = sizeof(Internal::IVS_TRUE) - 1;
    vTrue->base.flags = IS_CMP_TIME;

    VariableDefinition* vTrueDef = (VariableDefinition*) nalloc(nalc, NT_VARIABLE_DEFINITION);
    vTrueDef->var = vTrue;



    Variable* vFalse = Internal::variables + Internal::IV_FALSE;
    vFalse->base.scope = SyntaxNode::root;
    vFalse->value = { Type::DT_INT, 1, 1 };
    vFalse->name.buff = (char*) Internal::IVS_FALSE;
    vFalse->name.len = sizeof(Internal::IVS_FALSE) - 1;
    vFalse->base.flags = IS_CMP_TIME;

    VariableDefinition* vFalseDef = (VariableDefinition*) nalloc(nalc, NT_VARIABLE_DEFINITION);
    vFalseDef->var = vFalse;
    return ctx;

}

Variable* unwrap(Variable* var) {

    Expression* ex = var->expression;
    while (ex && ex->type == EXT_UNARY) {

        UnaryExpression* uex = (UnaryExpression*) ex;
        if (uex->base.opType == OP_NONE) {
            ex = uex->operand->expression;
            var = uex->operand;
        } else {
            return var;
        }

    }

    return var;

}



// whatever
//

int validateScopeNames(Scope* sc, INamedLoc** names, uint32_t namesCount, Namespace** nspace, ErrorSet** eset) {

    if (namesCount == 0) return 0;

    Namespace* tmpNspace = NULL;

    int i = 0;
    const int len = namesCount;

    for (; i < len; i++) {

        //MemberOffset arrOff = getMemberOffset(Scope, namespaces);
        //MemberOffset nameOff = getMemberOffset(scope);
        String name = *((String*) (names + i));
        tmpNspace = Ast::Find::inScopeNamespace(sc, &name);

        if (!tmpNspace) {

            ErrorSet* tmpEset = Ast::Find::inScopeErrorSet(sc, &name);
            if (!tmpEset) {

                // check implicit errors
                /*
                if (sc->fcn && sc->fcn->errorSet) {
                    tmpEset = sc->fcn->errorSet;
                } else {
                    Logger::log(logErr, Err::str(Err::UNKNOWN_NAMESPACE), sc->base.span, name.len, name.buff);
                    return Err::UNKNOWN_NAMESPACE;
                }
                */

            }

            // *eset = tmpEset;

            // need to test that other fields align with error set
            i++;
            for (; i < len; i++) {

                Variable* tmp = Ast::Find::inArray(tmpEset->vars, tmpEset->varCount, &name);
                if (!tmp) {
                    Logger::log(logErr, Err::str(Err::UNKNOWN_NAMESPACE));
                    return Err::UNKNOWN_NAMESPACE;
                }

                if (tmp->value.typeKind == Type::DT_ERROR && !(tmp->value.hasValue)) {
                    tmpEset = tmp->value.err;
                } else if (i + 1 < len) {
                    Logger::log(logErr, Err::str(Err::UNKNOWN_ERROR_SET), tmp->base.span);
                    return Err::UNKNOWN_ERROR_SET;
                }

            }

            *eset = tmpEset;

            break;

        }

        sc = (Scope*) tmpNspace;

    }

    *nspace = (Namespace*) tmpNspace;
    return Err::OK;

}

int findMember(INamed* member, TypeDefinition* dtype) {

    return 0;

}



template<uintptr_t offset>
void* _findGeneric(void* arr, uint32_t len, String* name) {

    for (uint32_t i = 0; i < len; i++) {

        char* item = (char*) arr + i;
        if (!item) continue;

        String* itemName = (String*)(item + offset);
        if (cstrcmp(itemName, name)) return item;

    }

    return NULL;

}

template<uintptr_t offset>
void* _findGeneric(void** arr, uint32_t len, String* name) {

    for (uint32_t i = 0; i < len; i++) {

        char* item = (char*) arr[i];
        if (!item) continue;

        String* itemName = (String*)(item + offset);
        if (cstrcmp(itemName, name)) return item;

    }

    return NULL;

}

template<uintptr_t offset>
void* _findGeneric(DArray::Container* arr, String* name) {

    for (int i = 0; i < arr->size; i++) {

        char* item = *(char**) DArray::get(arr, i);
        if (!item) continue;

        String* itemName = (String*)(item + offset);
        if (cstrcmp(itemName, name)) return item;

    }

    return NULL;

}

template<uintptr_t ptrOffset, uintptr_t nameOffset>
void* _findGenericNestedPtr(void** arr, uint32_t len, String* name) {

    for (int i = 0; i < len; i++) {

        char* item = (char*) arr[i];
        if (!item) continue;

        char* nestedObj = *(char**)(item + ptrOffset);
        if (!nestedObj) continue;

        String* itemName = (String*)(nestedObj + nameOffset);
        if (cstrcmp(itemName, name)) return item;

    }

    return NULL;

}

template<uintptr_t ptrOffset, uintptr_t nameOffset>
void* _findGenericNestedPtr(DArray::Container* arr, String* name) {

    for (int i = 0; i < arr->size; i++) {

        char* item = *(char**)DArray::get(arr, i);
        if (!item) continue;

        char* nestedObj = *(char**)(item + ptrOffset);
        if (!nestedObj) continue;

        String* itemName = (String*)(nestedObj + nameOffset);
        if (cstrcmp(itemName, name)) return item;

    }

    return NULL;

}

template<uintptr_t ptrOffset, uintptr_t nameOffset>
int _findGenericIdxNestedPtr(void** arr, uint32_t len, String* name, void** out) {

    for (int i = 0; i < len; i++) {

        char* item = (char*) arr[i];
        if (!item) continue;

        char* nestedObj = *(char**) (item + ptrOffset);
        if (!nestedObj) continue;

        String* itemName = (String*) (nestedObj + nameOffset);
        if (cstrcmp(itemName, name)) {
            if (out) *out = item;
            return i;
        }

    }

    if (out) *out = NULL;
    return -1;

}

template<uintptr_t ptrOffset, uintptr_t nameOffset>
int _findGenericIdxNestedPtr(DArray::Container* arr, String* name, void** out) {

    for (int i = 0; i < arr->size; i++) {

        char* item = *(char**) DArray::get(arr, i);
        if (!item) continue;

        char* nestedObj = *(char**) (item + ptrOffset);
        if (!nestedObj) continue;

        String* itemName = (String*) (nestedObj + nameOffset);
        if (cstrcmp(itemName, name)) {
            if (out) *out = item;
            return i;
        }

    }

    if (out) *out = NULL;
    return -1;

}

template<uintptr_t offset>
int _findGenericIdx(void** arr, uint32_t len, String* name, void** out) {

    for (uint32_t i = 0; i < len; i++) {

        char* item = (char*) arr[i];
        if (!item) continue;

        String* itemName = (String*) (item + offset);
        if (cstrcmp(itemName, name)) {
            if (out) *out = item;
            return (int)i;
        }

    }

    if (out) *out = NULL;
    return -1;

}

template<uintptr_t offset>
int _findGenericIdx(DArray::Container* arr, String* name, void** out) {

    for (int i = 0; i < arr->size; i++) {

        char* item = *(char**) DArray::get(arr, i);
        if (!item) continue;

        String* itemName = (String*) (item + offset);
        if (cstrcmp(itemName, name)) {
            if (out) *out = item; // FIXED: Set out BEFORE returning
            return i;
        }

    }

    if (out) *out = NULL;
    return -1;

}

template<uintptr_t arrayOffset, uintptr_t nameOffset>
void* _findGenericInScope(Scope* scope, String* name) {

    while (scope) {
        DArray::Container* items = (DArray::Container*)((char*)scope + arrayOffset);
        void* item = _findGeneric<nameOffset>(items, name);
        if (item) return item;
        scope = scope->base.scope;
    }

    return NULL;

}

// TODO
SyntaxNode* Ast::Find::inArray(SyntaxNode** arr, uint32_t len, const String* const name) {

    for (int i = 0; i < len; i++) {

        SyntaxNode* node = arr[0];

        String* nodeName;
        switch (node->type) {
            case NT_VARIABLE: {
                nodeName = (String*) &((Variable*) node)->name;
            }

        }

        if (cstrcmp(*name, *nodeName)) {
            return node;
        }

    }

    return NULL;

}

#define _defineInPtr(Type, Member) \
Type* Ast::Find::inArray(Type* arr, uint32_t len, String* name) { \
    return (Type*) _findGeneric<offsetof(Type, Member)>((void*) arr, len, name); \
}

#define _defineInArray(Type, Member) \
Type* Ast::Find::inArray(Type** arr, uint32_t len, String* name) { \
    return (Type*) _findGeneric<offsetof(Type, Member)>((void**) arr, len, name); \
}

#define _defineInArrayIdx(Type, Member) \
int Ast::Find::inArray(Type** arr, uint32_t len, String* name, Type** out) { \
    return _findGenericIdx<offsetof(Type, Member)>((void**) arr, len, name, (void**) out); \
}

#define _defineInArrayIdxPtr(Type, SubType, PtrMember, NameMember) \
int Ast::Find::inArray(Type** arr, uint32_t len, String* name, Type** out) { \
    return _findGenericIdxNestedPtr<offsetof(Type, PtrMember), offsetof(SubType, NameMember)>((void**) arr, len, name, (void**) out); \
}

#define _defineInScope(Type, NameMember, ScopeArrayMember) \
Type* Ast::Find::inScope##Type(Scope* scope, String* name) { \
    return NULL; \
}

_defineInPtr(Variable, name);

_defineInArray(Variable, name);
_defineInArray(Function, name);
_defineInArray(Enumerator, name);
_defineInArray(Namespace, name);
_defineInArray(Union, base.name);
_defineInArray(ErrorSet, name);
_defineInArray(TypeDefinition, name);
_defineInArray(Label, name);
_defineInArray(GotoStatement, name);
_defineInArray(ImportStatement, fname);
_defineInArray(CodeBlock, code.tagStr);
_defineInArray(ForeignFunction, fcn.name);

_defineInArrayIdx(Variable, name);
_defineInArrayIdx(Function, name);
_defineInArrayIdx(Enumerator, name);
_defineInArrayIdx(Namespace, name);
_defineInArrayIdx(Union, base.name);
_defineInArrayIdx(ErrorSet, name);
_defineInArrayIdx(TypeDefinition, name);
_defineInArrayIdx(Label, name);
_defineInArrayIdx(GotoStatement, name);
_defineInArrayIdx(ImportStatement, fname);
_defineInArrayIdx(CodeBlock, code.tagStr);
_defineInArrayIdx(ForeignFunction, fcn.name);
_defineInArrayIdxPtr(VariableDefinition, Variable, var, name);

_defineInScope(Variable, name, defs);
_defineInScope(Function, name, fcns);
_defineInScope(Union, base.name, unions);
_defineInScope(Label, name, labels);
_defineInScope(ErrorSet, name, customErrors);
_defineInScope(TypeDefinition, name, customDataTypes);
_defineInScope(Enumerator, name, enums);
_defineInScope(Namespace, name, namespaces);
_defineInScope(GotoStatement, name, gotos);



#define _defineMake(T, E) T* Ast::Node::make##T() { \
    T* node = (T*) nalloc(nalc, E); \
    init(node); \
    return node; \
} \

void init(SyntaxNode* node) {
    node->ogNode = NULL;
    node->definitionIdx = 0;
    node->scope = NULL;
    node->flags = 0;
    node->span = NULL;
}

void init(Expression* node) {
}

void init(IForeignCode* node) {
    node->tagLoc = NULL;
    node->codeStr = String(NULL, 0);
    node->tagStr = String(NULL, 0);
}

void Ast::Node::init(Scope* node) {
    node->definitions = NULL;
    node->definitionCount = 0;
    node->children = NULL;
    node->childrenCount = 0;

    ::init(&node->base);
    node->base.type = NT_SCOPE;
}
_defineMake(Scope, NT_SCOPE);

void Ast::Node::init(Namespace* node) {
    init(&node->scope);
    ::init(&node->name);
    node->scope.base.type = NT_NAMESPACE;
}
_defineMake(Namespace, NT_NAMESPACE);

void Ast::Node::init(Using* node) {
    node->var = NULL;
    ::init(&node->base);
    node->base.type = NT_USING;
}
_defineMake(Using, NT_USING);

void Ast::Node::init(CodeBlock* node) {
    ::init(&node->code);
    ::init(&node->base);
    node->base.type = NT_CODE_BLOCK;
}
_defineMake(CodeBlock, NT_CODE_BLOCK);

void Ast::Node::init(Enumerator* node) {
    node->dtype = Type::DT_VOID;
    node->vars = NULL;
    node->varCount = 0;

    ::init(&node->name);
    ::init(&node->base);
    node->base.type = NT_ENUMERATOR;
}
_defineMake(Enumerator, NT_ENUMERATOR);

void Ast::Node::init(Statement* node) {
    node->operand = NULL;
    ::init(&node->base);
    node->base.type = NT_STATEMENT;
}
_defineMake(Statement, NT_STATEMENT);

void Ast::Node::init(VariableDefinition* node) {
    node->var = makeVariable();
    node->var->def = node;
    node->lastPtr = NULL;
    node->dtype = NULL;
    ::init(&node->base);
    node->base.type = NT_VARIABLE_DEFINITION;
}
_defineMake(VariableDefinition, NT_VARIABLE_DEFINITION);

void Ast::Node::init(VariableAssignment* node) {
    node->lvar = NULL;
    node->rvar = NULL;
    ::init(&node->base);
    node->base.type = NT_VARIABLE_ASSIGNMENT;
}
_defineMake(VariableAssignment, NT_VARIABLE_ASSIGNMENT);

void Ast::Node::init(Variable* node) {
    ::init(&node->base);
    node->base.type = NT_VARIABLE;
    // TODO
}
_defineMake(Variable, NT_VARIABLE);

void Ast::Node::init(Function* node) {
    node->bodyScope = NULL;
    node->errorSet = NULL;
    node->errorSetName = NULL;
    node->internalIdx = 0;
    node->returns = NULL;
    node->returnCount = 0;

    ::init(&node->name);
    init(&node->prototype);
    ::init(&node->base);
    node->base.type = NT_FUNCTION;
}
_defineMake(Function, NT_FUNCTION);

void Ast::Node::init(ForeignFunction* node) {
    ::init(&node->code);
    init(&node->fcn);
}
_defineMake(ForeignFunction, NT_FUNCTION);

void Ast::Node::init(Branch* node) {
    node->expressions = NULL;
    node->expressionCount = 0;
    node->scopes = NULL;
    node->scopeCount = 0;

    ::init(&node->base);
    node->base.type = NT_BRANCH;
}
_defineMake(Branch, NT_BRANCH);

void Ast::Node::init(SwitchCase* node) {
    node->elseCase = NULL;
    node->switchExp = NULL;
    node->cases = NULL;
    node->casesExp = NULL;
    node->caseCount = 0;
    node->caseExpCount = 0;

    ::init(&node->base);
    node->base.type = NT_SWITCH_CASE;
}
_defineMake(SwitchCase, NT_SWITCH_CASE);

void Ast::Node::init(WhileLoop* node) {
    node->bodyScope = NULL;
    node->expression = NULL;
    ::init(&node->base);
    node->base.type = NT_WHILE_LOOP;
}
_defineMake(WhileLoop, NT_WHILE_LOOP);

void Ast::Node::init(ForLoop* node) {
    node->initEx = NULL;
    node->actionEx = NULL;
    node->bodyScope = NULL;
    node->initEx = NULL;
    ::init(&node->base);
    node->base.type = NT_FOR_LOOP;
}
_defineMake(ForLoop, NT_FOR_LOOP);

void Ast::Node::init(Loop* node) {
    node->array = NULL;
    node->bodyScope = NULL;
    node->idx = NULL;
    node->idxDef = NULL;
    ::init(&node->base);
    node->base.type = NT_LOOP;
}
_defineMake(Loop, NT_LOOP);

void Ast::Node::init(ReturnStatement* node) {
    node->err = NULL;
    node->fcn = NULL;
    node->var = NULL;
    node->idx = 0;
    ::init(&node->base);
    node->base.type = NT_RETURN_STATEMENT;
}
_defineMake(ReturnStatement, NT_RETURN_STATEMENT);

void Ast::Node::init(ContinueStatement* node) {
    ::init(&node->base);
    node->base.type = NT_CONTINUE_STATEMENT;
}
_defineMake(ContinueStatement, NT_CONTINUE_STATEMENT);

void Ast::Node::init(BreakStatement* node) {
    ::init(&node->base);
    node->base.type = NT_BREAK_STATEMENT;
}
_defineMake(BreakStatement, NT_BREAK_STATEMENT);

void Ast::Node::init(GotoStatement* node) {
    node->label = NULL;
    node->span = NULL;
    node->name = String{ NULL, 0 };
    ::init(&node->base);
    node->base.type = NT_GOTO_STATEMENT;
}
_defineMake(GotoStatement, NT_GOTO_STATEMENT);

void Ast::Node::init(Label* node) {
    node->span = NULL;
    ::init(&node->name);
    ::init(&node->base);
    node->base.type = NT_LABEL;
}
_defineMake(Label, NT_LABEL);

void Ast::Node::init(TypeDefinition* node) {
    node->typeInfo = NULL;
    node->vars = NULL;
    node->varCount = 0;
    node->base.type = NT_TYPE_DEFINITION;
    // TODO
}
_defineMake(TypeDefinition, NT_TYPE_DEFINITION);

void Ast::Node::init(Union* node) {
    init(&node->base);
    node->base.base.type = NT_UNION;
}
_defineMake(Union, NT_UNION);

void Ast::Node::init(ErrorSet* node) {
    node->value = 0;
    node->vars = NULL;
    node->varCount = 0;

    ::init(&node->name);
    ::init(&node->base);
    node->base.type = NT_ERROR;
}
_defineMake(ErrorSet, NT_ERROR);

void Ast::Node::init(ImportStatement* node) {
    node->fname = String(NULL, 0);
    node->keyword = KW_VOID;
    node->param = String(NULL, 0);
    ::init(&node->base);
    node->base.type = NT_IMPORT;
}
_defineMake(ImportStatement, NT_IMPORT);

void Ast::Node::init(FunctionCall* node) {
    node->fcn = NULL;
    node->fptr = NULL;
    node->inArgs = NULL;
    node->inArgCount = 0;
    node->outArg = NULL;

    init(&node->name);
    ::init(&node->base);
    node->base.type = EXT_FUNCTION_CALL;
}
_defineMake(FunctionCall, AT_EXT_FUNCTION_CALL);

void Ast::Node::init(OperationExpression* node) {
    node->opType = OP_NONE;
    ::init(&node->base);
    node->base.type = EXT_OPERATION;
}
_defineMake(OperationExpression, AT_EXT_OPERATION);

void Ast::Node::init(UnaryExpression* node) {
    node->operand = NULL;
    init(&node->base);
    node->base.base.type = EXT_UNARY;
}
_defineMake(UnaryExpression, AT_EXT_UNARY);

void Ast::Node::init(BinaryExpression* node) {
    node->left = NULL;
    node->right = NULL;
    init(&node->base);
    node->base.base.type = EXT_BINARY;
}
_defineMake(BinaryExpression, AT_EXT_BINARY);

void Ast::Node::init(TernaryExpression* node) {
    init(&node->base);
    node->base.base.type = EXT_TERNARY;
}
_defineMake(TernaryExpression, AT_EXT_TERNARY);

void Ast::Node::init(QualifiedName* node) {
    node->path = NULL;
    node->pathSize = 0;
    ::init((INamedEx*) node);
}
_defineMake(QualifiedName, AT_QUALIFIED_NAME);

void Ast::Node::init(FunctionPrototype* node) {
    node->inArgCount = 0;
    node->inArgs = NULL;
    node->outArg = NULL;
}
_defineMake(FunctionPrototype, AT_FUNCTION_PROTOTYPE);

void Ast::Node::init(StringInitialization* node) {
    node->rawStr.buff = NULL;
    node->rawStr.len = 0;
    node->wideType = Type::DT_VOID;
    node->wideStr.buff = NULL;
    node->wideStr.len = 0;

    ::init(&node->base);
    node->base.type = EXT_STRING_INITIALIZATION;
}
_defineMake(StringInitialization, AT_EXT_STRING_INITIALIZATION);

void Ast::Node::init(ArrayInitialization* node) {
    node->attributes = NULL;
    node->attributeCount = 0;

    ::init(&node->base);
    node->base.type = EXT_ARRAY_INITIALIZATION;
}
_defineMake(ArrayInitialization, AT_EXT_ARRAY_INITIALIZATION);

void Ast::Node::init(TypeInitialization* node) {
    node->fillVar = NULL;
    node->idxs = NULL;
    node->attributes = NULL;
    node->attributeCount = 0;

    ::init(&node->base);
    node->base.type = EXT_TYPE_INITIALIZATION;
}
_defineMake(TypeInitialization, AT_EXT_TYPE_INITIALIZATION);

void Ast::Node::init(Pointer* node) {
   node->parentPointer = NULL;
   node->pointsTo = NULL;
   node->pointsToKind = Type::DT_VOID;
}
_defineMake(Pointer, AT_POINTER);

void Ast::Node::init(Array* node) {
    node->flags = 0;
    node->length = NULL;
    init(&node->base);
}
_defineMake(Array, AT_ARRAY);

void Ast::Node::init(Slice* node) {
    node->arr = NULL;
    node->bidx = NULL;
    node->eidx = NULL;
    node->len = NULL;
    ::init(&node->base);
    node->base.type = EXT_SLICE;
}
_defineMake(Slice, AT_EXT_SLICE);

void Ast::Node::init(Catch* node) {
    node->call = NULL;
    node->err = NULL;
    node->scope = NULL;
    ::init(&node->base);
    node->base.type = EXT_CATCH;
}
_defineMake(Catch, AT_EXT_CATCH);

void Ast::Node::init(Cast* node) {
    node->target = Type::DT_VOID;
    node->operand = NULL;
    node->base.type = EXT_CAST;
}
_defineMake(Cast, AT_EXT_CAST);

void Ast::Node::init(Alloc* node) {
    ::init(&node->base);
    node->base.type = EXT_ALLOC;
    node->def = makeVariableDefinition();
}
_defineMake(Alloc, AT_EXT_ALLOC);

void Ast::Node::init(Free* node) {
    ::init(&node->base);
    node->var = makeVariable();
}
_defineMake(Free, AT_EXT_FREE);

#define _defineCopy(T, E) \
T* Ast::Node::copy(T* node) { \
    T* tmp = (T*) nalloc(nalc, E); \
    *tmp = *(node); \
    return tmp; \
}

_defineCopy(Scope,               NT_SCOPE);
_defineCopy(Namespace,           NT_NAMESPACE);
_defineCopy(Using,               NT_USING);
_defineCopy(CodeBlock,           NT_CODE_BLOCK);
_defineCopy(Enumerator,          NT_ENUMERATOR);
_defineCopy(Statement,           NT_STATEMENT);
_defineCopy(VariableDefinition,  NT_VARIABLE_DEFINITION);
_defineCopy(VariableAssignment,  NT_VARIABLE_ASSIGNMENT);
_defineCopy(Variable,            NT_VARIABLE);
_defineCopy(Function,            NT_FUNCTION);
_defineCopy(ForeignFunction,     AT_FOREIGN_FUNCTION);
_defineCopy(Branch,              NT_BRANCH);
_defineCopy(SwitchCase,          NT_SWITCH_CASE);
_defineCopy(WhileLoop,           NT_WHILE_LOOP);
_defineCopy(ForLoop,             NT_FOR_LOOP);
_defineCopy(Loop,                NT_LOOP);
_defineCopy(ReturnStatement,     NT_RETURN_STATEMENT);
_defineCopy(ContinueStatement,   NT_CONTINUE_STATEMENT);
_defineCopy(BreakStatement,      NT_BREAK_STATEMENT);
_defineCopy(GotoStatement,       NT_GOTO_STATEMENT);
_defineCopy(Label,               NT_LABEL);
_defineCopy(TypeDefinition,      NT_TYPE_DEFINITION);
_defineCopy(Union,               NT_UNION);
_defineCopy(ErrorSet,            NT_ERROR);
_defineCopy(ImportStatement,     NT_IMPORT);
_defineCopy(FunctionCall,        AT_EXT_FUNCTION_CALL);
_defineCopy(OperationExpression, AT_EXT_OPERATION);
_defineCopy(UnaryExpression,     AT_EXT_UNARY);
_defineCopy(BinaryExpression,    AT_EXT_BINARY);
_defineCopy(TernaryExpression,   AT_EXT_TERNARY);

Variable* Ast::Node::copy(Variable* dest, Variable* src) {

    if (!src) return NULL;
    if (!dest) {
        dest = (Variable*) nalloc(nalc, NT_VARIABLE);
    }

    dest->base = src->base;
    dest->def = src->def;
    dest->value = src->value;
    dest->expression = src->expression;
    dest->name = src->name;

    // deep copy name if it exists
    if (src->name.buff) {
        size_t nameLen = src->name.len;
        dest->name.buff = (char*)malloc(nameLen + 1);
        if (dest->name.buff) {
            memcpy(dest->name.buff, src->name.buff, nameLen);
            dest->name.buff[nameLen] = '\0';
        }
        dest->name.len = nameLen;
        dest->name.id = src->name.id;
    }

    return dest;

}

// TODO: for now here
//       quite strange function with wierd name, as
//       I am not sure if it needs to exist
Variable* Ast::Node::copyRef(Variable* dest, Variable* src) {

    if (!src) return NULL;
    if (!dest) {
        dest = (Variable*) nalloc(nalc, NT_VARIABLE);
    }

    dest->def = src->def;
    dest->value = src->value;
    dest->name = src->name;
    dest->base.flags = src->base.flags;

    // TODO: to a function
    // deep copy name if it exists
    if (src->name.buff) {
        size_t nameLen = src->name.len;
        dest->name.buff = (char*)malloc(nameLen + 1);
        if (dest->name.buff) {
            memcpy(dest->name.buff, src->name.buff, nameLen);
            dest->name.buff[nameLen] = '\0';
        }
        dest->name.len = nameLen;
        dest->name.id = src->name.id;
    }

    return dest;

}

const char* Ast::Node::str(NodeType type) {
    switch (type) {
        case NT_SCOPE:
            return "NT_SCOPE";
        case NT_VARIABLE_DEFINITION:
            return "NT_VARIABLE_DEFINITION";
        case NT_VARIABLE_ASSIGNMENT:
            return "NT_VARIABLE_ASSIGNMENT";
        case NT_TYPE_DEFINITION:
            return "NT_TYPE_DEFINITION";
        case NT_TYPE_INITIALIZATION:
            return "NT_TYPE_INITIALIZATION";
        case NT_ENUMERATOR:
            return "NT_ENUMERATOR";
        case NT_VARIABLE:
            return "NT_VARIABLE";
        case NT_FUNCTION:
            return "NT_FUNCTION";
        case NT_BRANCH:
            return "NT_BRANCH";
        case NT_SWITCH_CASE:
            return "NT_SWITCH_CASE";
        case NT_WHILE_LOOP:
            return "NT_WHILE_LOOP";
        case NT_FOR_LOOP:
            return "NT_FOR_LOOP";
        case NT_LOOP:
            return "NT_LOOP";
        case NT_RETURN_STATEMENT:
            return "NT_RETURN_STATEMENT";
        case NT_CONTINUE_STATEMENT:
            return "NT_CONTINUE_STATEMENT";
        case NT_BREAK_STATEMENT:
            return "NT_BREAK_STATEMENT";
        case NT_GOTO_STATEMENT:
            return "NT_GOTO_STATEMENT";
        case NT_LABEL:
            return "NT_LABEL";
        case NT_NAMESPACE:
            return "NT_NAMESPACE";
        case NT_STATEMENT:
            return "NT_STATEMENT";
        case NT_CODE_BLOCK:
            return "NT_CODE_BLOCK";
        case NT_ERROR:
            return "NT_ERROR";
        case NT_UNION:
            return "NT_UNION";
        case NT_USING:
            return "NT_USING";
        case NT_IMPORT:
            return "NT_IMPORT";
        case NT_COUNT:
            return "NT_COUNT";
        default:
            return "UNKNOWN_NODE_TYPE";
    }
}

const char* Ast::Node::str(ExpressionType type) {
    switch (type) {
        case EXT_FUNCTION_CALL:
            return "EXT_FUNCTION_CALL";
        case EXT_OPERATION:
            return "EXT_OPERATION";
        case EXT_UNARY:
            return "EXT_UNARY";
        case EXT_BINARY:
            return "EXT_BINARY";
        case EXT_TERNARY:
            return "EXT_TERNARY";
        case EXT_TYPE_INITIALIZATION:
            return "EXT_TYPE_INITIALIZATION";
        case EXT_STRING_INITIALIZATION:
            return "EXT_STRING_INITIALIZATION";
        case EXT_ARRAY_INITIALIZATION:
            return "EXT_ARRAY_INITIALIZATION";
        case EXT_SLICE:
            return "EXT_SLICE";
        case EXT_CATCH:
            return "EXT_CATCH";
        case EXT_CAST:
            return "EXT_CAST";
        case EXT_ALLOC:
            return "EXT_ALLOC";
        case EXT_FREE:
            return "EXT_FREE";
        case EXT_GET_LENGTH:
            return "EXT_GET_LENGTH";
        case EXT_GET_SIZE:
            return "EXT_GET_SIZE";
        default:
            return "UNKNOWN_EXPRESSION_TYPE";
    }
}
