// #pragma once

#include <cstdint>
#include <cstring>

#include "globals.h"

#include "syntax.h"
#include "interpreter.h"
#include "error.h"
#include "logger.h"
#include "utils.h"
#include "lexer.h"




Scope* SyntaxNode::root = NULL;
INamed* SyntaxNode::dir = NULL;

std::vector<LangDef*> NodeRegistry::langDefs;
std::vector<CodeBlock*> NodeRegistry::codeBlocks;
std::vector<ForeignFunction*> NodeRegistry::foreignFunctions;

std::vector<Variable*> NodeRegistry::variables;
std::vector<Variable*> NodeRegistry::fcnCalls;
std::vector<Function*> NodeRegistry::fcns;
std::vector<VariableDefinition*> NodeRegistry::customDataTypesReferences;
std::vector<VariableAssignment*> NodeRegistry::variableAssignments;
std::vector<Variable*> NodeRegistry::cmpTimeVars;
std::vector<Variable*> NodeRegistry::arrays;
std::vector<Loop*> NodeRegistry::loops;
std::vector<Label*> NodeRegistry::labels;
std::vector<Variable*> NodeRegistry::branchExpressions;
std::vector<Statement*> NodeRegistry::statements;
std::vector<VariableDefinition*> NodeRegistry::initializations;
std::vector<ReturnStatement*> NodeRegistry::returnStatements;
std::vector<SwitchCase*> NodeRegistry::switchCases;
std::vector<VariableDefinition*> NodeRegistry::variableDefinitions;

std::vector<ErrorSet*> NodeRegistry::customErrors;
std::vector<Union*> NodeRegistry::unions;

std::vector<Slice*> NodeRegistry::slices;

std::vector<VariableAssignment*> NodeRegistry::arraysAllocations;

std::vector<ImportStatement*> NodeRegistry::imports;

std::vector<TypeDefinition*> NodeRegistry::customDataTypes;
std::vector<Enumerator*> NodeRegistry::enumerators;

std::vector<GotoStatement*> NodeRegistry::gotos;


Variable* zero = new Variable {
    
};

uint64_t internalFunctionUsed = 0;


// internal Variables such as null, true, false etc...
Variable* internalVariables[] = {
    (new VariableDefinition(new Variable(SyntaxNode::root, { DT_POINTER, 1, 0 }, (char*)IVS_NULL, sizeof(IVS_NULL) - 1), IS_CMP_TIME))->var,
    (new VariableDefinition(new Variable(SyntaxNode::root, { DT_INT, 1, 1 }, (char*)IVS_TRUE, sizeof(IVS_TRUE) - 1), IS_CMP_TIME))->var,
    (new VariableDefinition(new Variable(SyntaxNode::root, { DT_INT, 1, 0 }, (char*)IVS_FALSE, sizeof(IVS_FALSE) - 1), IS_CMP_TIME))->var
};

const int internalVariablesCount = sizeof(internalVariables) / sizeof(Variable*);



// whatever
//

// assuming size > 0
int validateScopeNames(Scope* sc, std::vector<INamedLoc*> names, Namespace** nspace, ErrorSet** eset) {

    Namespace* tmpNspace = NULL;
    
    int i = 0;
    const int len = names.size();

    for (; i < len; i++) {

        INamedLoc* nm = names[i];
        tmpNspace = Utils::find<Namespace>(sc, nm->name, nm->nameLen, &Scope::namespaces);
        if (!tmpNspace) {
            
            ErrorSet* tmpEset = Utils::find<ErrorSet>(sc, nm->name, nm->nameLen, &Scope::customErrors);
            if (!tmpEset) {
                
                // check implicit errors
                if (sc->fcn && sc->fcn->errorSet) {
                    tmpEset = sc->fcn->errorSet;
                } else {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_NAMESPACE), nm->span, nm->nameLen);
                    return Err::UNKNOWN_NAMESPACE;
                }
            
            }

            // *eset = tmpEset;
            
            // need to test that other fields align with error set
            i++;
            for (; i < len; i++) {
                Variable* tmp = Utils::find(tmpEset->vars, names[i]->name, names[i]->nameLen);
                if (!tmp) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_NAMESPACE));
                    return Err::UNKNOWN_NAMESPACE;
                }
                if (tmp->cvalue.dtypeEnum == DT_ERROR && !(tmp->cvalue.hasValue)) {
                    tmpEset = tmp->cvalue.err;
                } else if (i + 1 < len) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_ERROR_SET), tmp->span);
                    return Err::UNKNOWN_ERROR_SET;
                }
            }

            *eset = tmpEset;

            break;
        
        }
        
        sc = tmpNspace;
           
    }

    *nspace = (Namespace*) tmpNspace;
    return Err::OK;

}

int findMember(INamed* member, TypeDefinition* dtype) {

    return 0;

}



// Constructors
//

Operand::Operand() : SyntaxNode(NT_OPERAND) {

    // this->expression = NULL;
    // this->unrollExpression = 1;
    // this->value = NULL;
    // this->dataTypeEnum = DT_UNDEFINED;
    // this->span = NULL;
    // this->hasValue = 0;
    this->def = NULL;

}

Operand::Operand(Scope* scope) : SyntaxNode(NT_OPERAND) {

    this->scope = scope;
    this->expression = NULL;
    this->unrollExpression = 1;
    this->cvalue.ptr = NULL;
    this->cvalue.dtypeEnum = DT_UNDEFINED;
    this->span = NULL;
    this->cvalue.hasValue = 0;
    this->def = NULL;

}

VariableDefinition::VariableDefinition() : SyntaxNode(NT_VARIABLE_DEFINITION) {
    
    this->var = new Variable();
    this->flags = 0;

}

VariableDefinition::VariableDefinition(Span* span) : SyntaxNode(NT_VARIABLE_DEFINITION) {
    
    this->span = getSpanStamp(span);
    if (!this->span) exit(1); // LOOK AT : maybe manage better

}

VariableDefinition::VariableDefinition(Variable* var, int flags) : SyntaxNode(NT_VARIABLE_DEFINITION) {
    
    this->var = var;
    this->flags = flags;
    var->def = this;
    // this->name = NULL;
    // this->nameLen = 0;
    // this->dtypeEnum = var->dataTypeEnum;

}

VariableAssignment::VariableAssignment() : SyntaxNode(NT_VARIABLE_ASSIGNMENT) {

    lvar = NULL;
    rvar = NULL;
    // offsetVar = NULL;

}

VariableAssignment::VariableAssignment(Span* span) : VariableAssignment() {
    
    this->span = getSpanStamp(span);
    if (!this->span) exit(1); // LOOK AT : maybe manage better

}

Variable::Variable() {

    flags = 0;
    unrollExpression = 1;
    scope = root;
    name = NULL;
    nameLen = 0;
    expression = NULL;
    cvalue.dtypeEnum = DT_UNDEFINED;
    cvalue.ptr = NULL;
    type = NT_VARIABLE;
    // parentStruct = NULL;
    // attribute = NULL;

}

Variable::Variable(Span* span) : Variable() {
    
    this->span = getSpanStamp(span);
    if (!this->span) exit(1); // LOOK AT : maybe manage better
    
}

Variable::Variable(Scope* scope) : Variable() {

    this->scope = scope;

}

Variable::Variable(Scope* const sc, DataTypeEnum dtype) : Variable() {
    
    unrollExpression = 1;
    scope = sc;
    name = NULL;
    nameLen = 0;
    expression = NULL;
    cvalue.dtypeEnum = dtype;
    cvalue.ptr = NULL;
    // parentStruct = NULL;
    // attribute = NULL;

}

Variable::Variable(Scope* const sc, DataTypeEnum dtype, Span* span) : Variable(sc, dtype) {
    
    this->span = getSpanStamp(span);
    if (!this->span) exit(1); // LOOK AT : maybe manage better
    
}

Variable::Variable(Scope* const sc, DataTypeEnum dtype, char* name, int nameLen) : Variable(sc, dtype) {
    
    this->name = name;
    this->nameLen = nameLen;

}

Variable::Variable(Scope* const sc, Value value, char* name, int nameLen) : Variable(sc) {
    
    this->cvalue = value;
    this->name = name;
    this->nameLen = nameLen;

}

Variable::Variable(Variable* var) {
    
    memcpy(this, var, sizeof(Variable));

}

Function::Function() : SyntaxNode(NT_FUNCTION) {
    this->errorSet = NULL;
    this->errorSetName = NULL;
    this->name = NULL;
    this->nameLen = 0;
    this->id = 0;
    this->internalIdx = 0;
    this->outArg = NULL;
    this->scope = NULL;
    this->bodyScope = NULL;
    this->snFlags = 0;
    this->parentIdx = 0;
}

Function::Function(Scope* sc, char* name, int nameLen, std::vector<VariableDefinition*> inArgs, VariableDefinition* outArg, int internalIdx) : SyntaxNode(NT_FUNCTION) {
    this->scope = sc;
    this->name = name;
    this->nameLen = nameLen;
    this->inArgs = inArgs;
    this->outArg = outArg;
    this->internalIdx = internalIdx;
    this->bodyScope = NULL;
}


// LOOK AT:
// As in compiler itself we dont really need to free memory
// we dont need this function.
// But for other programs, that may use compiler as library
// as lsp that is being developed, we have to provide such
// function.
//
// The way nodes are allocated and managed isn't really 
// suited for 'clean' freeing, as pointer of freed node 
// may be used somewhere else.
//
// For now, and maybe ever, it doesn't matter as there
// is no need to partially free nodes in the nonsense way
// that will cause troubles for the 'user'.
// If such case will arise, nodes will have to be, for example,
// be allocated in row in array-like structure/s, where their 
// indexes will be used as identifiers and accessors.
// Therefore once freed memory could be marked as some arbitrary
// value representing invalid item (NULL), that could be checked
// before access.
//
// Function will not clean parents references of deleting nodes
// as it will only matter for the very first case. Therefore it's
// pretty reasonable to leave this at caller responsibility if it's
// desired.
void freeNodeRecursively(SyntaxNode* node) {

    if (!node) return;

    delete node->span;

    switch (node->type) {

        case NT_SCOPE: {
            
            Scope* sc = (Scope*) node;
            for (int i = 0; i < sc->children.size(); i++) {
                freeNodeRecursively(sc->children[i]);
            }
            
            delete sc;
            
            break;
        
        }
        
        case NT_VARIABLE_DEFINITION: {
            
            VariableDefinition* def = (VariableDefinition*) node;
            freeNodeRecursively(def->var);
            
            delete def->dtype;
            delete def;
            
            break;
        
        }

        case NT_VARIABLE_ASSIGNMENT: {

            VariableAssignment* ass = (VariableAssignment*) node;
            freeNodeRecursively(ass->lvar);
            freeNodeRecursively(ass->rvar);
            
            delete ass;

            break;
        
        }

        case NT_VARIABLE: {
            
            Variable* var = (Variable*) node;
            
            if (var->cvalue.dtypeEnum >= DT_STRING) {
                delete var->cvalue.ptr;
            }

            delete var->expression;
            delete var;
            
            break;
        
        }

        case NT_FUNCTION: {

            Function* fcn = (Function*) node;
            for (int i = 0; i < fcn->inArgs.size(); i++) {
                freeNodeRecursively(fcn->inArgs[i]);
            }

            freeNodeRecursively(fcn->outArg);
            freeNodeRecursively(fcn->bodyScope);
            
            delete fcn->errorSetName;
            delete fcn;
            
            break;
        
        }

        case NT_RETURN_STATEMENT: {
            
            ReturnStatement* rs = (ReturnStatement*) node;
            freeNodeRecursively(rs->var);
            freeNodeRecursively(rs->err);
            
            delete rs;
            
            break;
        
        }

        default:
            delete node;
    }

}



// expression evaluating stuff
//













/*
int applyUnaryOperatorAddress(Operand* operand) {
    
    Pointer* ptr = new Pointer;
    ptr->pointsTo = operand->cvalue.any;
    ptr->pointsToEnum = operand->cvalue.dtypeEnum;
    
    operand->cvalue.dtypeEnum = DT_POINTER;
    operand->cvalue.ptr = ptr;
    
    return Err::OK;

}

int applyBinaryOperatorSubscript(Operand* a, Operand* b) {
    if (!IS_INT(b->cvalue.dtypeEnum)) return Err::INVALID_DATA_TYPE;
    return (a->cvalue.ptr)->pointsToEnum; // TODO : do general solution!!!
}

int applyBinaryOperatorMemberSelection(Operand* ans, Operand* a, Operand* b) {
    
    if (b->cvalue.dtypeEnum != DT_MEMBER) return Err::INVALID_DATA_TYPE;

    Variable* var = Utils::find(a->cvalue.def->vars, ((Variable*) b)->name, ((Variable*) b)->nameLen);
    if (!var) return Err::INVALID_ATTRIBUTE_NAME;

    b->cvalue.dtypeEnum = var->cvalue.dtypeEnum; // dont know about this one
    b->cvalue.def = var->cvalue.def;
    ((Variable*) b)->id = var->id;
    ans->cvalue.any = var->cvalue.any;

    return Err::OK;

}

int applyBinaryOperatorMemberSelectionEnum(Operand* ans, Operand* a, Operand* b) {
    
    if (b->cvalue.dtypeEnum != DT_MEMBER) return Err::INVALID_DATA_TYPE;

    Variable* var = Utils::find(a->cvalue.enm->vars, ((Variable*) b)->name, ((Variable*) b)->nameLen);
    if (!var) return Err::INVALID_ATTRIBUTE_NAME;

    ans->cvalue.dtypeEnum = var->cvalue.dtypeEnum; // dont know about this one
    ans->cvalue.any = var->cvalue.any;
    ans->cvalue.hasValue = var->cvalue.hasValue;
    ans->def = var->def;
    ans->expression = NULL;
    ans->unrollExpression = 0;

    b->cvalue.dtypeEnum = var->cvalue.dtypeEnum;

    return Err::OK;

}

int applyBinaryOperatorMemberSelectionPointer(Operand* ans, Operand* a, Operand* b) {
    
    if (b->cvalue.dtypeEnum != DT_MEMBER) return Err::INVALID_DATA_TYPE;

    Pointer* ptr = a->cvalue.ptr;
    if (ptr->pointsToEnum != DT_CUSTOM) return Err::INVALID_DATA_TYPE;

    TypeDefinition* td = (TypeDefinition*) (ptr->pointsTo);

    Variable* var = Utils::find(td->vars, ((Variable*) b)->name, ((Variable*) b)->nameLen);
    if (!var) return Err::INVALID_ATTRIBUTE_NAME;

    b->cvalue.dtypeEnum = var->cvalue.dtypeEnum; // dont know about this one
    b->cvalue.any = var->cvalue.any;
    ((Variable*) b)->id = var->id;
    ans->cvalue.any = var->cvalue.any;
    ((BinaryExpression*) (ans->expression))->operType = OP_DEREFERENCE_MEMBER_SELECTION;
    // ((BinaryExpression*) (ans->expression))->oper = operators + OP_DEREFERENCE_MEMBER_SELECTION;

    return Err::OK;

}



// custom dypes
int applyUnaryOperatorPlusCustom(Operand* operand) {
    return Err::OK;
}


int applyUnaryOperatorMinusCustom(Operand* operand) {
    return Err::OK;
}


*/




// on change update size!!!
DataType dataTypes[DATA_TYPES_COUNT] = {
        
    // VOID
    {
        (char*) Lex::KWS_VOID,
        sizeof(Lex::KWS_VOID) - 1,
        0,
        0
    },

    // INT
    {
        (char*) Lex::KWS_INT,
        sizeof(Lex::KWS_INT) - 1,
        4,
        1
    },

    // INT_8
    {
        (char*) Lex::KWS_I8,
        sizeof(Lex::KWS_I8) - 1,
        1,
        1
    },

    // INT_16
    {
        (char*) Lex::KWS_I16,
        sizeof(Lex::KWS_I16) - 1,
        2,
        1
    },

    // INT_32
    {
        (char*) Lex::KWS_I32,
        sizeof(Lex::KWS_I32) - 1,
        4,
        1
    },

    // INT_64
    {
        (char*) Lex::KWS_I64,
        sizeof(Lex::KWS_I64) - 1,
        8,
        2
    },

    // UINT_8
    {
        (char*) Lex::KWS_U8,
        sizeof(Lex::KWS_U8) - 1,
        1,
        1
    },

    // UINT_16
    {
        (char*) Lex::KWS_U16,
        sizeof(Lex::KWS_U16) - 1,
        2,
        1
    },

    // UINT_32
    {
        (char*) Lex::KWS_U32,
        sizeof(Lex::KWS_U32) - 1,
        4,
        1
    },

    // UINT_64
    {
        (char*) Lex::KWS_U64,
        sizeof(Lex::KWS_U64) - 1,
        8,
        2
    },

    // FLOAT_32
    {
        (char*) Lex::KWS_F32,
        sizeof(Lex::KWS_F32) - 1,
        4,
        3
    },

    // FLOAT_64
    {
        (char*) Lex::KWS_F64,
        sizeof(Lex::KWS_F64) - 1,
        8,
        4
    },

    // STRING
    {
        (char*) "string",
        sizeof("string") - 1,
        8 * 2,
        5
    },

    // POINTER
    {
        (char*) "ptr",
        sizeof("ptr") - 1,
        8 * 8,
        5 
    },

    // ARRAY
    {
        (char*) "array",
        5,
        8 * 8,
        5 
    },

    // SLICE
    {
        (char*) "slice",
        5,
        8 * 8,
        5
    },

    // MULTIPLE_TYPES
    {
        (char*) "...",
        3,
        0,
        0
    },

    // CUSTOM
    {
        (char*) "custom",
        6,
        0,
        10
    },

    // MEMBER
    {
        (char*) "member",
        6,
        0,
        0
    },

    // ENUM
    {
        (char*) "enum",
        4,
        0,
        0
    },

    // UNDEFINED
    {
        (char*) "undefined",
        9,
        0,
        0
    }

};
