#pragma once
#include "data_types.h"
#include "error.h"
#include "syntax.h"

namespace Validator {

    enum Score {
        FOS_IMPLICIT_CAST = 1,
        FOS_SIZE_DECREASE,
        FOS_SIGN_CHANGE,
        FOS_TO_FLOAT,
        FOS_PROMOTION,
        FOS_EXACT_MATCH,
    };

    // TODO : better name?
    struct FunctionScore {
        Function* fcn;
        int score;
    };



    Err::Err validate();

    Err::Err linkAll();
    Err::Err linkDataTypes();
    Err::Err linkErrorSets();
    Err::Err linkVariables();
    Err::Err linkGotos();
    Err::Err linkFunctionCalls();

    Err::Err resolveTypes();
    Err::Err resolveTypes(Variable* var);

    Err::Err validateTypeDefinitions();
    Err::Err validateErrorSets();
    Err::Err verifyFunctionsAreGlobal();
    Err::Err evaluateEnums();
    Err::Err evaluateCompileTimeVariables();
    Err::Err validateReturns();
    Err::Err validateAssignments();
    Err::Err validateLoops();
    Err::Err validateInitializations();
    Err::Err validateBranches();
    Err::Err validateStatements();
    Err::Err validateFunctionCalls();

    Err::Err validateImplicitCast(const DataTypeEnum dtype, const DataTypeEnum dtypeRef);
    Err::Err validateImplicitCast(void* dtype, void* dtypeRef, DataTypeEnum dtypeEnum, DataTypeEnum dtypeEnumRef);
    Err::Err validateAttributeCast(Variable* var, Variable* attribute);
    Err::Err validateTypeInitialization(TypeDefinition* dtype, TypeInitialization* dtypeInit);
    Err::Err validateTypeInitializations(TypeDefinition* dtype, Variable* var);
    Err::Err validateFunctionCall(Variable* fcnCallOp);
    Err::Err validatePointerAssignment(const Value* const val);
    Err::Err validateQualifiedName(Scope* scope, QualifiedName* name, Namespace** nspaceOut, ErrorSet** esetOut);

    Err::Err evaluateArrayLength(Variable* var, uint64_t flag = IS_LENGTH);
    Err::Err evaluateArrayLength(Variable* var, Variable* len, uint64_t flag = IS_LENGTH);
    Err::Err evaluateTypeInitialization(Variable* op, int attributesCount, TypeInitialization** tinitOut);
    // Err::Err evaluateDataTypes(Variable* op, TypeDefinition** customDtype = NULL, DataTypeEnum lvalueType = DT_UNDEFINED, TypeDefinition* lvalueTypeDef = NULL);
    Err::Err evaluate(Variable* op, TypeDefinition** customDtype = NULL);

    Err::Err verifyFunctionsAreGlobal();

    Function* findExactFunction(Scope* scope, INamed* const name, FunctionPrototype* const fptr);
    int findClosestFunction(Variable* callOp, Function** outFcn);
    Variable* findDefinition(Scope* scope, QualifiedName* const inVar, int idx);

    int isUnique(Scope* sc, INamed* named, Span* span);
    int computeSizeOfDataType(TypeDefinition* const def);
    int getFirstNonArrayDtype(Array* arr, int maxLevel = -1, int* level = NULL);
    int match(FunctionPrototype* const fptrA, FunctionPrototype* const fptrB);

    bool isBasicDtype(DataTypeEnum dtype);
    void castLiteral(Value* val, DataTypeEnum toDtype);

}
