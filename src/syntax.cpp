// #pragma once

#include <cstdint>
#include <cstring>

#include "globals.h"

#include "syntax.h"
#include "interpreter.h"
#include "error.h"
#include "logger.h"
#include "utils.h"




Scope* SyntaxNode::root = NULL;
INamed* SyntaxNode::dir = NULL;

std::vector<LangDef*> SyntaxNode::langDefs;
std::vector<CodeBlock*> SyntaxNode::codeBlocks;
std::vector<ForeignFunction*> SyntaxNode::foreignFunctions;

std::vector<Variable*> SyntaxNode::variables;
std::vector<Variable*> SyntaxNode::fcnCalls;
std::vector<Function*> SyntaxNode::fcns;
std::vector<VariableDefinition*> SyntaxNode::customDataTypesReferences;
std::vector<VariableAssignment*> SyntaxNode::variableAssignments;
std::vector<Variable*> SyntaxNode::cmpTimeVars;
std::vector<Variable*> SyntaxNode::arrays;
std::vector<Loop*> SyntaxNode::loops;
std::vector<Label*> SyntaxNode::labels;
std::vector<Variable*> SyntaxNode::branchExpressions;
std::vector<Statement*> SyntaxNode::statements;
std::vector<VariableDefinition*> SyntaxNode::initializations;
std::vector<ReturnStatement*> SyntaxNode::returnStatements;
std::vector<SwitchCase*> SyntaxNode::switchCases;
std::vector<VariableDefinition*> SyntaxNode::variableDefinitions;

std::vector<ErrorSet*> SyntaxNode::customErrors;
std::vector<Union*> SyntaxNode::unions;

std::vector<Slice*> SyntaxNode::slices;

std::vector<VariableAssignment*> SyntaxNode::arraysAllocations;

std::vector<ImportStatement*> SyntaxNode::imports;

std::vector<TypeDefinition*> SyntaxNode::customDataTypes;
std::vector<Enumerator*> SyntaxNode::enumerators;

std::vector<GotoStatement*> SyntaxNode::gotos;


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

// helping functions (move to utils?)
// 
Location* getLocationStamp(Location* loc) {

    Location* stamp = (Location*) malloc(sizeof(Location));
    if (!stamp) return NULL;

    stamp->file = loc->file;
    stamp->idx = loc->idx;
    stamp->line = loc->line;

    return stamp;

}



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
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_NAMESPACE), nm->loc, nm->nameLen);
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
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_ERROR_SET), tmp->loc);
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
    // this->loc = NULL;
    // this->hasValue = 0;
    this->def = NULL;

}

Operand::Operand(Scope* scope) : SyntaxNode(NT_OPERAND) {

    this->scope = scope;
    this->expression = NULL;
    this->unrollExpression = 1;
    this->cvalue.ptr = NULL;
    this->cvalue.dtypeEnum = DT_UNDEFINED;
    this->loc = NULL;
    this->cvalue.hasValue = 0;
    this->def = NULL;

}

VariableDefinition::VariableDefinition() : SyntaxNode(NT_VARIABLE_DEFINITION) {
    
    this->flags = 0;

}

VariableDefinition::VariableDefinition(Location* loc) : SyntaxNode(NT_VARIABLE_DEFINITION) {
    
    this->loc = getLocationStamp(loc);
    if (!this->loc) exit(1); // LOOK AT : maybe manage better

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

VariableAssignment::VariableAssignment(Location* loc) : VariableAssignment() {
    
    this->loc = getLocationStamp(loc);
    if (!this->loc) exit(1); // LOOK AT : maybe manage better

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

Variable::Variable(Location* loc) : Variable() {
    
    this->loc = getLocationStamp(loc);
    if (!this->loc) exit(1); // LOOK AT : maybe manage better
    
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

Variable::Variable(Scope* const sc, DataTypeEnum dtype, Location* loc) : Variable(sc, dtype) {
    
    this->loc = getLocationStamp(loc);
    if (!this->loc) exit(1); // LOOK AT : maybe manage better
    
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



// expression evaluating stuff
//














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







// on change update size!!!
DataType dataTypes[DATA_TYPES_COUNT] = {
        
    // VOID
    {
        (char*) KWS_VOID,
        sizeof(KWS_VOID) - 1,
        0,
        0
    },

    // INT
    {
        (char*) KWS_INT,
        sizeof(KWS_INT) - 1,
        4,
        1
    },

    // INT_8
    {
        (char*) KWS_INT_8,
        sizeof(KWS_INT_8) - 1,
        1,
        1
    },

    // INT_16
    {
        (char*) KWS_INT_16,
        sizeof(KWS_INT_16) - 1,
        2,
        1
    },

    // INT_32
    {
        (char*) KWS_INT_32,
        sizeof(KWS_INT_32) - 1,
        4,
        1
    },

    // INT_64
    {
        (char*) KWS_INT_64,
        sizeof(KWS_INT_64) - 1,
        8,
        2
    },

    // UINT_8
    {
        (char*) KWS_UINT_8,
        sizeof(KWS_UINT_8) - 1,
        1,
        1
    },

    // UINT_16
    {
        (char*)KWS_UINT_16,
        sizeof(KWS_UINT_16) - 1,
        2,
        1
    },

    // UINT_32
    {
        (char*)KWS_UINT_32,
        sizeof(KWS_UINT_32) - 1,
        4,
        1
    },

    // UINT_64
    {
        (char*)KWS_UINT_64,
        sizeof(KWS_UINT_64) - 1,
        8,
        2
    },

    // FLOAT_32
    {
        (char*) KWS_FLOAT_32,
        sizeof(KWS_FLOAT_32) - 1,
        4,
        3
    },

    // FLOAT_64
    {
        (char*) KWS_FLOAT_64,
        sizeof(KWS_FLOAT_64) - 1,
        8,
        4
    },

    // STRING
    {
        (char*) KWS_STRING,
        sizeof(KWS_STRING) - 1,
        8 * 2,
        5
    },

    // POINTER
    {
        (char*) KWS_POINTER,
        sizeof(KWS_POINTER) - 1,
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
