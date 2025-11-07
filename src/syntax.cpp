#include <cstring>

#include "syntax.h"
#include "array_list.h"
#include "data_types.h"
#include "error.h"
#include "globals.h"
#include "keywords.h"
#include "logger.h"
#include "operators.h"



const Logger::Type logErr = { .level = Logger::ERROR, .tag = stageTag };



Scope* SyntaxNode::root = NULL;
INamed SyntaxNode::dir = { NULL, 0 };

_RegMemory Reg = {};

void _RegMemory::init() {

    constexpr int initSize = 1024;

    for (int i = 0; i < dataSize; i++) {
        DArray::init(Reg.data + i, initSize, sizeof(void*));
        (Reg.data + i)->constCoef = 0;
        (Reg.data + i)->constTerm = (Reg.data + i)->allocSize;
    }

}

void Internal::init() {

    Function* fPrintf = functions + IF_PRINTF;
    fPrintf->base.scope = SyntaxNode::root;
    fPrintf->name.buff = (char*) IFS_PRINTF;
    fPrintf->name.len = sizeof(IFS_PRINTF) - 1;
    fPrintf->internalIdx = IF_PRINTF;

    DArray::init(&fPrintf->prototype.inArgs.base, 2, sizeof(VariableDefinition*));

    VariableDefinition* fPrintArg1 = (VariableDefinition*) nalloc(alc, NT_VARIABLE_DEFINITION);
    fPrintArg1->var = (Variable*) nalloc(alc, NT_VARIABLE);
    fPrintArg1->var->base.scope = SyntaxNode::root;
    fPrintArg1->var->cvalue.dtypeEnum = DT_STRING;

    VariableDefinition* fPrintArg2 = (VariableDefinition*) nalloc(alc, NT_VARIABLE_DEFINITION);
    fPrintArg2->var = (Variable*) nalloc(alc, NT_VARIABLE);
    fPrintArg2->var->base.scope = SyntaxNode::root;
    fPrintArg2->var->cvalue.dtypeEnum = DT_MULTIPLE_TYPES;

    DArray::push(&fPrintf->prototype.inArgs.base, &fPrintArg1);
    DArray::push(&fPrintf->prototype.inArgs.base, &fPrintArg2);



    Function* fAlloc = functions + IF_ALLOC;
    fAlloc->base.scope = SyntaxNode::root;
    fAlloc->name.buff = (char*) IFS_ALLOC;
    fAlloc->name.len = sizeof(IFS_ALLOC) - 1;
    fAlloc->internalIdx = IF_ALLOC;

    DArray::init(&fAlloc->prototype.inArgs.base, 2, sizeof(VariableDefinition*));
    VariableDefinition* fAllocArg1 = (VariableDefinition*) nalloc(alc, NT_VARIABLE_DEFINITION);
    fAllocArg1->var = (Variable*) nalloc(nalc, NT_VARIABLE);
    fAllocArg1->var->base.scope = SyntaxNode::root;
    fAllocArg1->var->cvalue.dtypeEnum = DT_MULTIPLE_TYPES;



    Function* fFree = functions + IF_FREE;
    fFree->base.scope = SyntaxNode::root;
    fFree->name.buff = (char*) IFS_FREE;
    fFree->name.len = sizeof(IFS_FREE) - 1;
    fFree->internalIdx = IF_FREE;

    DArray::init(&fFree->prototype.inArgs.base, 2, sizeof(VariableDefinition*));
    VariableDefinition* fFree1 = (VariableDefinition*) nalloc(nalc, NT_VARIABLE_DEFINITION);
    fFree1->var = (Variable*) nalloc(nalc, NT_VARIABLE);
    fFree1->var->base.scope = SyntaxNode::root;
    fFree1->var->cvalue.dtypeEnum = DT_POINTER;



    // internal Variables such as null, true, false etc...
    Variable* vNull = variables + IV_NULL;
    vNull->cvalue = { DT_POINTER, 1, 0 };
    vNull->base.scope = SyntaxNode::root;
    vNull->name.buff = (char*) IVS_NULL;
    vNull->name.len = sizeof(IVS_NULL) - 1;
    vNull->base.flags = IS_CMP_TIME;

    VariableDefinition* vNullDef = (VariableDefinition*) nalloc(nalc, NT_VARIABLE_DEFINITION);
    vNullDef->var = vNull;



    Variable* vTrue = variables + IV_TRUE;
    vTrue->base.scope = SyntaxNode::root;
    vTrue->cvalue = { DT_INT, 1, 1 };
    vTrue->name.buff = (char*) IVS_TRUE;
    vTrue->name.len = sizeof(IVS_TRUE) - 1;
    vTrue->base.flags = IS_CMP_TIME;

    VariableDefinition* vTrueDef = (VariableDefinition*) nalloc(nalc, NT_VARIABLE_DEFINITION);
    vTrueDef->var = vTrue;



    Variable* vFalse = variables + IV_FALSE;
    vFalse->base.scope = SyntaxNode::root;
    vFalse->cvalue = { DT_INT, 1, 1 };
    vFalse->name.buff = (char*) IVS_FALSE;
    vFalse->name.len = sizeof(IVS_FALSE) - 1;
    vFalse->base.flags = IS_CMP_TIME;

    VariableDefinition* vFalseDef = (VariableDefinition*) nalloc(nalc, NT_VARIABLE_DEFINITION);
    vFalseDef->var = vFalse;

}



// whatever
//

// assuming size > 0
int validateScopeNames(Scope* sc, DArrayINamedLoc* namesContainer, Namespace** nspace, ErrorSet** eset) {

    Namespace* tmpNspace = NULL;
    INamedLoc* names = (INamedLoc*) namesContainer->base.buffer;

    int i = 0;
    const int len = namesContainer->base.size;

    for (; i < len; i++) {

        //MemberOffset arrOff = getMemberOffset(Scope, namespaces);
        //MemberOffset nameOff = getMemberOffset(scope);
        String name = *((String*) (names + i));
        tmpNspace = Reg.Find.inScopeNamespace(sc, &name);

        if (!tmpNspace) {

            ErrorSet* tmpEset = Reg.Find.inScopeErrorSet(sc, &name);
            if (!tmpEset) {

                // check implicit errors
                if (sc->fcn && sc->fcn->errorSet) {
                    tmpEset = sc->fcn->errorSet;
                } else {
                    Logger::log(logErr, ERR_STR(Err::UNKNOWN_NAMESPACE), sc->base.span, name.len, name.buff);
                    return Err::UNKNOWN_NAMESPACE;
                }

            }

            // *eset = tmpEset;

            // need to test that other fields align with error set
            i++;
            for (; i < len; i++) {

                Variable* tmp = Reg.Find.inArray(&tmpEset->vars, &name);
                if (!tmp) {
                    Logger::log(logErr, ERR_STR(Err::UNKNOWN_NAMESPACE));
                    return Err::UNKNOWN_NAMESPACE;
                }

                if (tmp->cvalue.dtypeEnum == DT_ERROR && !(tmp->cvalue.hasValue)) {
                    tmpEset = tmp->cvalue.err;
                } else if (i + 1 < len) {
                    Logger::log(logErr, ERR_STR(Err::UNKNOWN_ERROR_SET), tmp->base.span);
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



void* _RegMemory::Find::generic(DArray::Container* arr, String* name, MemberOffset mName) {

    for (int i = 0; i < arr->size; i++) {

        void* item = DArray::get(arr, i);
        INamed* itemName = (INamed*) getMember(item, mName);

        if (name->len == itemName->len &&
            !memcmp(name->buff, itemName->buff, name->len)
        ) {
            return item;
        }

    }

    return NULL;

}

void* _RegMemory::Find::generic(Scope* scope, String* name, MemberOffset mName, MemberOffset mArray) {

    while (scope) {

        DArray::Container* items = (DArray::Container*) getMember(scope, mArray);

        void* item = generic(items, name, mName);
        if (item) return item;

        scope = scope->base.scope;

    }

    return NULL;

}

int genericIdx(DArray::Container* arr, String* name, MemberOffset mName, void** out) {

    for (int i = 0; i < arr->size; i++) {

        void* item = DArray::get(arr, i);
        INamed* itemName = (INamed*) getMember(item, mName);

        if (name->len == itemName->len &&
            !memcmp(name->buff, itemName->buff, name->len)
        ) {
            return i;
            if (out) *out = item;
        }

    }

    if (out) *out = NULL;
    return -1;

}

void* genericPreviousOccurrences(Scope* scope, String* name, int idx, MemberOffset mArray) {

    while (scope) {

        DArray::Container* items = (DArray::Container*) getMember(scope, mArray);

        for (int i = 0; i < idx; i++) {
            INamed* item = (INamed*) DArray::get(items, i);

            if (name->len != item->len) continue;
            if (memcmp(name->buff, item->buff, name->len) == 0)
                return item;
        }

        scope = scope->base.scope;
        if (!scope) break;
        idx = scope->base.parentIdx;
    }

    return NULL;

}

Variable* _RegMemory::Find::inArray(Variable* arr, int arrLen, String* name) {

    for (int i = 0; i < arrLen; i++) {

        Variable* item = arr + i;

        if (name->len == item->name.len &&
            !memcmp(name->buff, item->name.buff, name->len)
        ) {
            return item;
        }

    }

    return NULL;

}

#define _defineInArray(Type, MemberName) \
Type* _RegMemory::Find::inArray(DArray##Type* arr, String* name) { \
    return (Type*) generic((DArray::Container*) arr, name, getMemberOffset(Type, MemberName)); \
}

#define _defineInArrayIdx(Type, MemberName) \
int _RegMemory::Find::inArray(DArray##Type* arr, String* name, Type** out) { \
    return genericIdx((DArray::Container*) arr, name, getMemberOffset(Type, MemberName), (void**) out); \
}

#define _defineInScope(Type, MemberName, MemberArray) \
Type* _RegMemory::Find::inScope##Type(Scope* scope, String* name) { \
    return (Type*) generic(scope, name, getMemberOffset(Type, MemberName), getMemberOffset(Scope, MemberArray)); \
}

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
_defineInArray(VariableDefinition, var->name);
// _defineInArray(LangDef, tag);

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
_defineInArrayIdx(VariableDefinition, var->name);
// _defineInArrayIdx(LangDef, tag)

_defineInScope(Variable, name, defs);
_defineInScope(Function, name, fcns);
_defineInScope(Union, base.name, unions);
_defineInScope(Label, name, labels);
_defineInScope(ErrorSet, name, customErrors);
_defineInScope(TypeDefinition, name, customDataTypes);
_defineInScope(Enumerator, name, enums);
_defineInScope(Namespace, name, namespaces);
_defineInScope(GotoStatement, name, gotos);
// _defineInScope(Using, name, usings)



#define _defineInit(T, E) T* _RegMemory::Node::init##T() { \
    T* node = (T*) nalloc(nalc, E); \
    init(node); \
    return node; \
} \

void init(SyntaxNode* node) {
    node->ogNode = NULL;
    node->parentIdx = 0;
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

void _RegMemory::Node::init(Scope* node) {
    OrderedDict::init(&node->defSearch, 8);
    DArray::init(&node->children.base, 8, sizeof(void*));

    int arraysCount = sizeof(node->arrays) / sizeof(DArray::Container);
    for (int i = 0; i < arraysCount; i++) {
        DArray::init(node->arrays + i, 8, sizeof(void*));
    }

    ::init(&node->base);
    node->base.type = NT_SCOPE;
}
_defineInit(Scope, NT_SCOPE);

void _RegMemory::Node::init(Namespace* node) {
    init(&node->scope);
    ::init(&node->name);
    node->scope.base.type = NT_NAMESPACE;
}
_defineInit(Namespace, NT_NAMESPACE);

void _RegMemory::Node::init(Using* node) {
    node->var = NULL;
    ::init(&node->base);
    node->base.type = NT_USING;
}
_defineInit(Using, NT_USING);

void _RegMemory::Node::init(CodeBlock* node) {
    ::init(&node->code);
    ::init(&node->base);
    node->base.type = NT_CODE_BLOCK;
}
_defineInit(CodeBlock, NT_CODE_BLOCK);

void _RegMemory::Node::init(Enumerator* node) {
    node->dtype = DT_VOID;
    DArray::init(&node->vars.base, 8, sizeof(void*));
    ::init(&node->name);
    ::init(&node->base);
    node->base.type = NT_ENUMERATOR;
}
_defineInit(Enumerator, NT_ENUMERATOR);

void _RegMemory::Node::init(Statement* node) {
    node->operand = NULL;
    ::init(&node->base);
    node->base.type = NT_STATEMENT;
}
_defineInit(Statement, NT_STATEMENT);

void _RegMemory::Node::init(VariableDefinition* node) {
    node->var = initVariable();
    node->lastPtr = NULL;
    node->dtype = NULL;
    ::init(&node->base);
    node->base.type = NT_VARIABLE_DEFINITION;
}
_defineInit(VariableDefinition, NT_VARIABLE_DEFINITION);

void _RegMemory::Node::init(VariableAssignment* node) {
    node->lvar = NULL;
    node->rvar = NULL;
    ::init(&node->base);
    node->base.type = NT_VARIABLE_ASSIGNMENT;
}
_defineInit(VariableAssignment, NT_VARIABLE_ASSIGNMENT);

void _RegMemory::Node::init(Variable* node) {
    ::init(&node->base);
    node->base.type = NT_VARIABLE;
    // TODO
}
_defineInit(Variable, NT_VARIABLE);

void _RegMemory::Node::init(Function* node) {
    node->bodyScope = NULL;
    node->errorSet = NULL;
    node->errorSetName = NULL;
    node->icnt = 0;
    node->internalIdx = 0;
    node->istackIdx = 0;
    DArray::init(&node->returns.base, 8, sizeof(void*));
    ::init(&node->name);
    init(&node->prototype);
    ::init(&node->base);
    node->base.type = NT_FUNCTION;
}
_defineInit(Function, NT_FUNCTION);

void _RegMemory::Node::init(ForeignFunction* node) {
    ::init(&node->code);
    init(&node->fcn);
}
_defineInit(ForeignFunction, NT_FUNCTION);

void _RegMemory::Node::init(Branch* node) {
    DArray::init(&node->expressions.base, 8, sizeof(void*));
    DArray::init(&node->scopes.base, 8, sizeof(void*));
    ::init(&node->base);
    node->base.type = NT_BRANCH;
}
_defineInit(Branch, NT_BRANCH);

void _RegMemory::Node::init(SwitchCase* node) {
    node->elseCase = NULL;
    node->switchExp = NULL;
    DArray::init(&node->cases.base, 8, sizeof(void*));
    DArray::init(&node->casesExp.base, 8, sizeof(void*));
    ::init(&node->base);
    node->base.type = NT_SWITCH_CASE;
}
_defineInit(SwitchCase, NT_SWITCH_CASE);

void _RegMemory::Node::init(WhileLoop* node) {
    node->bodyScope = NULL;
    node->expression = NULL;
    ::init(&node->base);
    node->base.type = NT_WHILE_LOOP;
}
_defineInit(WhileLoop, NT_WHILE_LOOP);

void _RegMemory::Node::init(ForLoop* node) {
    node->initEx = NULL;
    node->actionEx = NULL;
    node->bodyScope = NULL;
    node->initEx = NULL;
    ::init(&node->base);
    node->base.type = NT_FOR_LOOP;
}
_defineInit(ForLoop, NT_FOR_LOOP);

void _RegMemory::Node::init(Loop* node) {
    node->array = NULL;
    node->bodyScope = NULL;
    node->idx = NULL;
    node->idxDef = NULL;
    ::init(&node->base);
    node->base.type = NT_LOOP;
}
_defineInit(Loop, NT_LOOP);

void _RegMemory::Node::init(ReturnStatement* node) {
    node->err = NULL;
    node->fcn = NULL;
    node->var = NULL;
    node->idx = 0;
    ::init(&node->base);
    node->base.type = NT_RETURN_STATEMENT;
}
_defineInit(ReturnStatement, NT_RETURN_STATEMENT);

void _RegMemory::Node::init(ContinueStatement* node) {
    ::init(&node->base);
    node->base.type = NT_CONTINUE_STATEMENT;
}
_defineInit(ContinueStatement, NT_CONTINUE_STATEMENT);

void _RegMemory::Node::init(BreakStatement* node) {
    ::init(&node->base);
    node->base.type = NT_BREAK_STATEMENT;
}
_defineInit(BreakStatement, NT_BREAK_STATEMENT);

void _RegMemory::Node::init(GotoStatement* node) {
    node->label = NULL;
    node->span = NULL;
    node->name = String{ NULL, 0 };
    ::init(&node->base);
    node->base.type = NT_GOTO_STATEMENT;
}
_defineInit(GotoStatement, NT_GOTO_STATEMENT);

void _RegMemory::Node::init(Label* node) {
    node->span = NULL;
    ::init(&node->name);
    ::init(&node->base);
    node->base.type = NT_LABEL;
}
_defineInit(Label, NT_LABEL);

void _RegMemory::Node::init(TypeDefinition* node) {
    node->dtype = { .size = 0, .rank = 0 };
    DArray::init(&node->vars.base, 8, sizeof(void*));
    node->base.type = NT_TYPE_DEFINITION;
    // TODO
}
_defineInit(TypeDefinition, NT_TYPE_DEFINITION);

void _RegMemory::Node::init(Union* node) {
    init(&node->base);
    node->base.base.type = NT_UNION;
}
_defineInit(Union, NT_UNION);

void _RegMemory::Node::init(ErrorSet* node) {
    node->value = 0;
    DArray::init(&node->vars.base, 8, sizeof(void*));
    ::init(&node->name);
    ::init(&node->base);
    node->base.type = NT_ERROR;
}
_defineInit(ErrorSet, NT_ERROR);

void _RegMemory::Node::init(ImportStatement* node) {
    node->fname = String(NULL, 0);
    node->keyword = KW_VOID;
    node->param = String(NULL, 0);
    ::init(&node->base);
    node->base.type = NT_IMPORT;
}
_defineInit(ImportStatement, NT_IMPORT);

void _RegMemory::Node::init(FunctionCall* node) {
    node->fcn = NULL;
    node->fptr = NULL;
    node->inArgsCnt = 0;
    node->outArg = NULL;
    DArray::init(&node->inArgs.base, 8, sizeof(void*));
    init(&node->name);
    ::init(&node->base);
    node->base.type = EXT_FUNCTION_CALL;
}
_defineInit(FunctionCall, AT_EXT_FUNCTION_CALL);

void _RegMemory::Node::init(OperationExpression* node) {
    node->opType = OP_NONE;
    ::init(&node->base);
    node->base.type = EXT_OPERATION;
}
_defineInit(OperationExpression, AT_EXT_OPERATION);

void _RegMemory::Node::init(UnaryExpression* node) {
    node->operand = NULL;
    init(&node->base);
    node->base.base.type = EXT_UNARY;
}
_defineInit(UnaryExpression, AT_EXT_UNARY);

void _RegMemory::Node::init(BinaryExpression* node) {
    node->left = NULL;
    node->right = NULL;
    init(&node->base);
    node->base.base.type = EXT_BINARY;
}
_defineInit(BinaryExpression, AT_EXT_BINARY);

void _RegMemory::Node::init(TernaryExpression* node) {
    init(&node->base);
    node->base.base.type = EXT_TERNARY;
}
_defineInit(TernaryExpression, AT_EXT_TERNARY);

void _RegMemory::Node::init(QualifiedName* node) {
    node->path = NULL;
    node->pathSize = 0;
    ::init((INamedEx*) node);
}
_defineInit(QualifiedName, AT_QUALIFIED_NAME);

void _RegMemory::Node::init(FunctionPrototype* node) {
    node->inArgsCnt = 0;
    node->outArg = NULL;
    DArray::init(&node->inArgs.base, 8, sizeof(void*));
}
_defineInit(FunctionPrototype, AT_FUNCTION_PROTOTYPE);

void _RegMemory::Node::init(StringInitialization* node) {
    node->rawPtr = NULL;
    node->rawPtrLen = 0;
    node->rawStr = ""; // TODO
    node->wideDtype = DT_VOID;
    node->wideStr = NULL;
    node->wideLen = 0;
    ::init(&node->base);
    node->base.type = EXT_STRING_INITIALIZATION;
}
_defineInit(StringInitialization, AT_EXT_STRING_INITIALIZATION);

void _RegMemory::Node::init(ArrayInitialization* node) {
    DArray::init(&node->attributes.base, 8, sizeof(void*));
    ::init(&node->base);
    node->base.type = EXT_ARRAY_INITIALIZATION;
}
_defineInit(ArrayInitialization, AT_EXT_ARRAY_INITIALIZATION);

void _RegMemory::Node::init(TypeInitialization* node) {
    node->fillVar = NULL;
    node->idxs = NULL;
    DArray::init(&node->attributes.base, 8, sizeof(void*));
    ::init(&node->base);
    node->base.type = EXT_TYPE_INITIALIZATION;
}
_defineInit(TypeInitialization, AT_EXT_TYPE_INITIALIZATION);

void _RegMemory::Node::init(Pointer* node) {
   node->parentPointer = NULL;
   node->pointsTo = NULL;
   node->pointsToEnum = DT_VOID;
}
_defineInit(Pointer, AT_POINTER);

void _RegMemory::Node::init(Array* node) {
    node->flags = 0;
    node->length = NULL;
    init(&node->base);
}
_defineInit(Array, AT_ARRAY);

void _RegMemory::Node::init(Slice* node) {
    node->arr = NULL;
    node->bidx = NULL;
    node->eidx = NULL;
    node->len = NULL;
    ::init(&node->base);
    node->base.type = EXT_SLICE;
}
_defineInit(Slice, AT_EXT_SLICE);

void _RegMemory::Node::init(Catch* node) {
    node->call = NULL;
    node->err = NULL;
    node->scope = NULL;
    ::init(&node->base);
    node->base.type = EXT_CATCH;
}
_defineInit(Catch, AT_EXT_CATCH);



#define _defineCopy(T, E) \
T* _RegMemory::Node::copy(T* node) { \
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

Variable* _RegMemory::Node::copy(Variable* dest, Variable* src) {
    if (!src) return nullptr;
    if (!dest) {
        dest = (Variable*)nalloc(nalc, NT_VARIABLE);
        if (!dest) return nullptr;
    }

    // Copy base structure while preserving pointers
    dest->base = src->base;
    dest->def = src->def;
    dest->cvalue = src->cvalue;
    dest->ivalue = src->ivalue;
    dest->unrollExpression = src->unrollExpression;
    dest->expression = src->expression;
    dest->name = src->name;

    // Deep copy name if it exists
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
