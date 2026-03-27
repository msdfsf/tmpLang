#pragma once
#include "data_types.h"
#include "syntax.h"
#include "diagnostic.h"



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



    Err::Err validate(AstContext* astCtx);

    Err::Err linkAll(AstContext* ctx);
    Err::Err linkDataTypes(AstContext* ctx);
    Err::Err linkErrorSets(AstContext* ctx);
    Err::Err linkVariables(AstContext* ctx);
    Err::Err linkGotos(AstContext* ctx);
    Err::Err linkFunctionCalls(AstContext* ctx);

    Err::Err resolveTypes(AstContext* ctx);
    Err::Err resolveTypes(Variable* var, Variable* target);

    Err::Err computeTypesInfo(AstContext* ctx);

    Err::Err validateTypeDefinitions(AstContext* ctx);
    Err::Err validateErrorSets(AstContext* ctx);
    Err::Err verifyFunctionsAreGlobal(AstContext* ctx);
    Err::Err evaluateEnums(AstContext* ctx);
    Err::Err evaluateCompileTimeVariables(AstContext* ctx);
    Err::Err validateReturns(AstContext* ctx);
    Err::Err validateAssignments(AstContext* ctx);
    Err::Err validateLoops(AstContext* ctx);
    Err::Err validateInitializations(AstContext* ctx);
    Err::Err validateBranches(AstContext* ctx);
    Err::Err validateStatements(AstContext* ctx);
    Err::Err validateFunctionCalls(AstContext* ctx);

    Err::Err validateImplicitCast(const Type::Kind dtype, const Type::Kind dtypeRef);
    Err::Err validateImplicitCast(void* dtype, void* dtypeRef, Type::Kind dtypeEnum, Type::Kind dtypeEnumRef);
    Err::Err validateAttributeCast(Variable* var, Variable* attribute);
    Err::Err validateTypeInitialization(TypeDefinition* dtype, TypeInitialization* dtypeInit);
    Err::Err validateTypeInitializations(TypeDefinition* dtype, Variable* var);
    Err::Err validateFunctionCall(Variable* fcnCallOp);
    Err::Err validatePointerAssignment(const Value* const val);
    Err::Err validateQualifiedName(Scope* scope, QualifiedName* name, Namespace** nspaceOut, ErrorSet** esetOut);

    Err::Err verifyFunctionsAreGlobal();

    Function* findExactFunction(Scope* scope, INamed* const name, FunctionPrototype* const fptr);
    int findClosestFunction(Variable* callOp, Function** outFcn);
    Variable* findDefinition(Scope* scope, QualifiedName* const inVar, int idx);

    int isUnique(Scope* sc, INamed* named, Span* span);
    int computeSizeOfDataType(TypeDefinition* const def);
    int getFirstNonArrayDtype(Array* arr, int maxLevel = -1, int* level = NULL);
    int match(FunctionPrototype* const fptrA, FunctionPrototype* const fptrB);

    bool isBasicDtype(Type::Kind dtype);
    void castLiteral(Value* val, Type::Kind toDtype);

}
