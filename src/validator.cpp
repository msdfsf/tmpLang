// #pragma once

#include "logger.h"
#include "error.h"
#include "syntax.h"
#include "utils.h"
#include "interpreter.h"
#include "lexer.h"
#include <iterator>
#include "c_translator.h"
#include <cstring>

namespace Validator {

    int validateImplicitCast(const DataTypeEnum dtype, const DataTypeEnum dtypeRef);
    int validateImplicitCast(void* dtype, void* dtypeRef, DataTypeEnum dtypeEnum, DataTypeEnum dtypeEnumRef);
    int validateAttributeCast(Variable* var, Variable* attribute);
    int validateTypeInitialization(TypeDefinition* dtype, TypeInitialization* dtypeInit);
    int validateTypeInitializations(TypeDefinition* dtype, Variable* var);
    inline int validatePointerAssignment(const Value* const val);
    int validateFunctionCall(Variable* fcnCallOp);
    
    int evaluateArrayLength(Variable* var, uint64_t flag = IS_LENGTH);
    int evaluateArrayLength(Variable* var, Variable* len, uint64_t flag = IS_LENGTH);
    int evaluateTypeInitialization(Variable* op, int attributesCount, TypeInitialization** tinitOut);
    int evaluateDataTypes(Variable* op, TypeDefinition** customDtype = NULL, DataTypeEnum lvalueType = DT_UNDEFINED, TypeDefinition* lvalueTypeDef = NULL);
    int evaluate(Variable* op, TypeDefinition** customDtype = NULL);

    Function* findExactFunction(Scope* scope, INamed* const name, FunctionPrototype* const fptr);
    int findClosestFunction(Operand* callOp, Function** outFcn);
    Variable* findDefinition(Scope* scope, QualifiedName* const inVar, int idx);

    int isUnique(Scope* sc, INamed* named, Span* span);
    
    int computeSizeOfDataType(TypeDefinition* const def);

    int getFirstNonArrayDtype(Array* arr, int maxLevel = -1, int* level = NULL);

    void stripWrapperExpressions(Variable* var, Variable** opOut);
    void stripWrapperExpressions(Variable** op);
    void stripWrapperExpressions(Variable* op);

    int match(FunctionPrototype* const fptrA, FunctionPrototype* const fptrB);

    void copy(Variable* dest, Variable* src);
    int copyExpression(Variable* src, Variable** dest, int* pidx, int* nextIdx);

    // TODO : better name?
    struct FunctionScore {
        Function* fcn;
        int score;
    };

    std::vector<FunctionScore> fCandidates; // f as function



    int validate() {

        Logger::log(Logger::INFO, "Validating...\n");

        // link user defined data types with definitions
        //

        for (int i = 0; i < (int) NodeRegistry::customDataTypesReferences.size(); i++) {

            VariableDefinition* const varDef = NodeRegistry::customDataTypesReferences[i];

            void** dtype;
            int* dtypeEnum;
            
            if (varDef->lastPtr) {
                dtype = (void**) &(varDef->lastPtr->pointsTo);
                dtypeEnum = (int*) &(varDef->lastPtr->pointsToEnum);
            } else {
                dtype = (void**) &(varDef->var->cvalue.any);
                dtypeEnum = (int*) &(varDef->var->cvalue.dtypeEnum);
            }

            Scope* scope = varDef->scope;
            if (varDef->dtype->path.size() > 0) {
                Namespace* nspace = NULL;
                ErrorSet* eset = NULL;
                const int err = validateScopeNames(scope, varDef->dtype->path, &nspace, &eset);
                if (err != Err::OK) return err;
                if (eset) {
                    continue;
                }
                scope = nspace;
            }

            TypeDefinition* td = Utils::find<TypeDefinition>(scope, varDef->dtype->name, varDef->dtype->nameLen, &Scope::customDataTypes);
            if (td) {
                *dtype = (void*) td;
                *dtypeEnum = DT_CUSTOM;
                continue;
            }

            Enumerator* en = Utils::find<Enumerator>(scope, varDef->dtype->name, varDef->dtype->nameLen, &Scope::enums);
            if (en) {
                *dtype = (dataTypes + en->dtype);
                *dtypeEnum = en->dtype;
                continue;
            }

            Union* un = Utils::find<Union>(scope, varDef->dtype->name, varDef->dtype->nameLen, &Scope::unions);
            if (un) {
                *dtype = (void*) un;
                *dtypeEnum = DT_UNION;
                continue;
            }

            ErrorSet* er = Utils::find<ErrorSet>(scope, varDef->dtype->name, varDef->dtype->nameLen, &Scope::customErrors);
            if (er) {
                *dtype = (void*) er;
                *dtypeEnum = DT_ERROR;
                continue;
            }
            
            Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_DATA_TYPE), varDef->span, varDef->dtype->nameLen);
            return Err::UNKNOWN_DATA_TYPE;
        
        }

        // validate error sets
        {
            int varId = NodeRegistry::variables.size();
            int errId = NodeRegistry::customErrors.size() + 1;
            
            for (int i = 0; i < NodeRegistry::customErrors.size(); i++) {
                // hasValue is used to mark if error set have its definition or not,
                // if not, then hasValue is true, as there is nothing to expand to
                
                ErrorSet* const eset = NodeRegistry::customErrors[i];
                for (int i = 0; i < eset->vars.size(); i++) {
                    
                    Variable* const var = eset->vars[i];
                    
                    var->cvalue.err = Utils::find<ErrorSet>(eset->scope, var->name, var->nameLen, &Scope::customErrors);
                    if (!(var->cvalue.err)) {
                        var->cvalue.hasValue = 1;
                        var->cvalue.dtypeEnum = DT_ERROR;
                        var->cvalue.u64 = errId;
                        var->id = varId;
                        varId++;
                        errId++;
                    }
                
                }

            }
        }

        // link error sets with functions
        for (int i = 0; i < NodeRegistry::fcns.size(); i++) {

            Function* const fcn = NodeRegistry::fcns[i];
            QualifiedName* const errName = fcn->errorSetName;

            if (!errName) {
                fcn->errorSet = NULL;
                continue;
            }

            if (errName->path.size() > 0) {

                Namespace* nspace = NULL;
                ErrorSet* eset = NULL;

                const int err = validateScopeNames(fcn->scope, errName->path, &nspace, &eset);
                if (err != Err::OK) return err;

                if (!eset) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_ERROR_SET), fcn->span);
                    return Err::UNKNOWN_ERROR_SET;
                }

                if (nspace) {
                    eset = Utils::find<ErrorSet>(nspace->customErrors, errName->name, errName->nameLen);
                    if (!eset) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_ERROR_SET), fcn->span);
                        return Err::UNKNOWN_ERROR_SET;
                    }
                } else if (eset) {
                    eset = Utils::find<ErrorSet>(eset->scope, errName->name, errName->nameLen, &Scope::customErrors);
                    if (!eset) {
                        Logger::log(Logger::ERROR, "Unknown or empty error set!", fcn->span);
                        return Err::UNKNOWN_ERROR_SET;
                    }
                }

                fcn->errorSet = eset;

            } else {

                ErrorSet* const eset = Utils::find<ErrorSet>(fcn->scope, errName->name, errName->nameLen, &Scope::customErrors);
                if (!eset) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_ERROR_SET), fcn->span);
                    return Err::UNKNOWN_ERROR_SET;
                }

                fcn->errorSet = eset;
            
            }
        
        }

        // validate custom typedef
        for (int i = 0; i < NodeRegistry::customDataTypes.size(); i++) {

            TypeDefinition* const def = NodeRegistry::customDataTypes[i];
            const int isUnion = def->type == NT_UNION;

            for (int i = 0; i < def->vars.size(); i++) {
                
                Variable* const var = def->vars[i];

                if (isUnion && var->expression || var->cvalue.hasValue) {
                    Logger::log(Logger::ERROR, "Default values are not allowed within union initialization!", var->span, var->nameLen);
                    return Err::INVALID_RVALUE;
                }

                if (var->cvalue.dtypeEnum != DT_CUSTOM) continue;

                // in case of custom data type check if its defined before
                if (var->cvalue.def->parentIdx >= def->parentIdx) {
                    TypeDefinition* const tmpDef = var->cvalue.def;
                    Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_DECLARATION_ORDER), var->def->span, var->def->dtype->nameLen, def->nameLen, def->name);
                    return Err::INVALID_DECLARATION_ORDER;
                }
            }

        }

        // link variables with definitions
        // in case of scopeNames var is the last name (ex. point.x, var is then x)
        // scopeNames are sorted from left to right as written
        for (int i = 0; i < NodeRegistry::variables.size(); i++) {

            Variable* const var = NodeRegistry::variables[i];
            Variable* tmpVar;
            // const int scopeNamesLen = var->scopeNames.size();

            Scope* scope = var->scope;
            if (var->path.size() > 0) { 
                Namespace* nspace = NULL;
                ErrorSet* eset = NULL;
                const int err = validateScopeNames(var->scope, var->path, &nspace, &eset);
                if (err != Err::OK) return err;
                if (eset) {
                    tmpVar = Utils::find<Variable>(eset->vars, var->name, var->nameLen);
                    if (!tmpVar) {
                        Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_ERROR_SET), var->span, var->nameLen);
                        return Err::UNKNOWN_ERROR_SET;
                    }
                    copy(var, tmpVar);
                    continue;
                }
                scope = nspace;
                
                // if namespace, we dont care about order

                tmpVar = Utils::find<Variable>(scope, var->name, var->nameLen, &Scope::defs);
                if (tmpVar) {
                    copy(var, tmpVar);
                    continue;
                }

            } else {

                /*
                SyntaxNode* tmp = findDefinition(scope, var, var->parentIdx);
                if (tmp && tmp->type == NT_VARIABLE) {
                    copy(var, (Variable*) tmp);
                    continue;
                } else if (tmp && tmp->type == NT_ERROR) {
                    var->cvalue.err = (ErrorSet*) tmp;
                    var->cvalue.dtypeEnum = DT_ERROR;
                    continue;
                } else {
                    Logger::log(Logger::ERROR, "TODO : Error!");
                    return Err::UNEXPECTED_SYMBOL;
                }
                */
                tmpVar = findDefinition(scope, var, var->parentIdx);
                if (tmpVar) {
                    copy(var, tmpVar);
                    continue;
                }

            }

            /*
            Variable* tmpVar = Utils::find<Variable>(scope, var->name, var->nameLen, &Scope::vars);
            if (tmpVar) {
                copy(var, tmpVar);
                continue;
            }
            */

            tmpVar = Utils::find(internalVariables, internalVariablesCount, var->name, var->nameLen);
            if (tmpVar) {
                copy(var, tmpVar);
                continue;
            }

            Enumerator* en = Utils::find<Enumerator>(scope, var->name, var->nameLen, &Scope::enums);
            if (en) {
                var->cvalue.dtypeEnum = DT_ENUM;
                var->cvalue.enm = en;
                continue;
            }

            // or function pointer
            Function* fcn = Utils::find<Function>(scope, var->name, var->nameLen, &Scope::fcns);
            if (fcn) {
                var->cvalue.dtypeEnum = DT_FUNCTION;
                var->cvalue.fcn = NULL;
                continue;
            }

            // Data type itself represents its size
            TypeDefinition* td = Utils::find<TypeDefinition>(scope, var->name, var->nameLen, &Scope::customDataTypes);
            if (td) {
                // TODO : it seems better to compute it later and here just collect all candidates

                const int size = computeSizeOfDataType(td);
                if (size < 0) return size;

                var->cvalue.i64 = size;
                var->cvalue.hasValue = 1;
                var->cvalue.dtypeEnum = DT_I64;
                
                continue;
            
            }

            Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_VARIABLE), var->span, var->nameLen, var->nameLen, var->name);
            return Err::UNKNOWN_VARIABLE;
            
        }



        // verify and link goto statements
        for (int i = 0; i < NodeRegistry::gotos.size(); i++) {

            GotoStatement* gt = NodeRegistry::gotos[i];

            Label* lb = Utils::find<Label>(gt->scope, gt->name, gt->nameLen, &Scope::labels);
            if (lb) {
                gt->label = lb;
                continue;
            }

            Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_VARIABLE), gt->span, gt->nameLen, gt->nameLen, gt->name);
            return Err::UNKNOWN_VARIABLE;

        }



        // verify that each function is in global scope 
        // or in namespace that is in global scope
        for (int i = 0; i < NodeRegistry::fcns.size(); i++) {
            
            Function* const fcn = NodeRegistry::fcns[i];

            Scope* sc = fcn->scope;
            while (sc) {
                if (sc->type == NT_SCOPE) break;
                sc = sc->scope;
            }
            //Scope* const sc = fcn->scope->type == NT_SCOPE ? fcn->scope : fcn->scope->scope;
            if (sc != SyntaxNode::root) {
                Logger::log(Logger::ERROR, ERR_STR(Err::GLOBAL_SCOPE_REQUIRED), fcn->span, fcn->nameLen);
                return Err::GLOBAL_SCOPE_REQUIRED;
            }

        }



        // link function calls with definitions
        for (int i = 0; i < (int) NodeRegistry::fcnCalls.size(); i++) {

            Variable* fcnCallOp = NodeRegistry::fcnCalls[i];
            FunctionCall* fcnCall = (FunctionCall*) (fcnCallOp->expression);

            if (fcnCall->fcn) continue;

            Function* fcn;
            const int err = findClosestFunction(fcnCallOp, &fcn);
            if (err < 0) {
                // check for function pointer
               
                Variable* var = findDefinition(fcnCallOp->scope, fcnCall, fcnCallOp->parentIdx);
                if (!var) {
                    Logger::log(Logger::ERROR, ERR_STR(err), fcnCallOp->span, fcnCall->nameLen);
                    return err;
                }

                fcnCall->fptr = var;
                fcnCall->fcn = NULL;
                fcnCall->outArg = new Variable();
                fcnCall->outArg->cvalue.hasValue = 0;
                fcnCall->outArg->cvalue.dtypeEnum = var->cvalue.fcn->outArg->var->cvalue.dtypeEnum;

                continue;

            }

            fcnCall->fptr = NULL;
            fcnCall->fcn = fcn;
            fcnCall->outArg = new Variable();
            fcnCall->outArg->cvalue.hasValue = 0;
            fcnCall->outArg->cvalue.dtypeEnum = fcn->outArg->var->cvalue.dtypeEnum;

        }

        /*
        for (int i = 0; i < NodeRegistry::fcns.size(); i++) {

            Function* fcn = NodeRegistry::fcns[i];
            if (fcn->errorSetNameLen <= 0) continue;

            ErrorSet* errorSet = Utils::find<ErrorSet>(fcn->scope, fcn->errorSetName, fcn->errorSetNameLen, &Scope::customErrors);

            if (!errorSet) {
                Logger::log(Logger::ERROR, ERR_STR(Err::UNKNOWN_ERROR_SET), fcn->span);
                return Err::UNKNOWN_ERROR_SET;
            }

            fcn->errorSet = errorSet;

        }
        */


        // compute enum values
        //

        for (int i = 0; i < NodeRegistry::enumerators.size(); i++) {

            Enumerator* en = NodeRegistry::enumerators[i];

            int err;
            int64_t lastValue = -1;
            for (int i = 0; i < en->vars.size(); i++) {

                Variable* var = en->vars[i];

                if (var->expression) {

                    err = evaluateDataTypes(var);
                    if (err < Err::OK) return err;

                    err = evaluate(var);
                    if (err < Err::OK) return err;

                }

                var->unrollExpression = 0;
                var->expression = NULL;

                if (var->cvalue.hasValue) {
                    lastValue = var->cvalue.i64;
                } else {
                    lastValue++;
                    var->cvalue.i64 = lastValue;
                    var->cvalue.hasValue = 1;
                }

            }

        }

        // compute all cmptime vars
        //
        
        for (int i = 0; i < (int) NodeRegistry::cmpTimeVars.size(); i++) {

            int err;
            Variable* var = NodeRegistry::cmpTimeVars[i];

            if (!(var->expression)) continue;
            
            if (var->cvalue.dtypeEnum != DT_ARRAY) {
                err = evaluateDataTypes(var);
                if (err < Err::OK) return err;
            }

            err = evaluate(var);
            if (err < Err::OK) return err;
            
        }



        // compute arrays length
        //

        // for now messy
        for (int i = 0; i < (int) NodeRegistry::arrays.size(); i++) {

            Variable* var = NodeRegistry::arrays[i];
            Array* arr = var->cvalue.arr;

            // arr->flags : info about length
            // var should directly represent definition

            if (!(var->expression)) {
                
                const uint64_t flags = arr->flags;

                if (flags & IS_CONST || flags & IS_ARRAY_LIST || flags & IS_DYNAMIC) {
                    if (flags & IS_ARRAY_LIST) var->def->flags |= IS_ARRAY_LIST;
                    arr->flags |= IS_ALLOCATED;
                    continue;
                }

                if (!(arr->length) || !(arr->length->expression)) {
                    Logger::log(Logger::ERROR, "Array declaration have to have length defineed either as compile-time expression or be a qualifier. Length cannot be empty without right side.", var->span, var->nameLen);
                    return Err::INVALID_ARRAY_LENGTH;
                }

                int err;
                
                err =  evaluateDataTypes(arr->length);
                if (err < Err::OK) return err;
    
                err = evaluate(arr->length);
                if (err < Err::OK) return err;
                
                continue;

            }

            if (var->expression->type == EXT_FUNCTION_CALL) {
                // alloc

                Variable* alloc = ((FunctionCall*) (var->expression))->inArgs[0];

                if (arr->flags & IS_CMP_TIME) {
                    Logger::log(Logger::ERROR, "TODO error: invalid use of alloc!", var->span, var->nameLen);
                    return Err::CANNOT_EVALUATE_EXPRESION_AT_CMP_TIME;
                }

                if (!(arr->flags & IS_CONST)) {
                    if (arr->length && arr->length->cvalue.hasValue) {
                        if (arr->length->cvalue.i64 != alloc->cvalue.arr->length->cvalue.i64) {
                            Logger::log(Logger::ERROR, "TODO error: array and alloc size missmatch, you can omit array size while initializationg array with rvalue...");
                            return Err::INVALID_LVALUE;
                        }
                    }
                    arr->flags |= IS_ARRAY_LIST;
                    var->def->flags |= IS_ARRAY_LIST;
                }

                if (alloc->cvalue.dtypeEnum != DT_ARRAY) {
                    Logger::log(Logger::ERROR, "TODO error: validating arrays!");
                    return Err::INVALID_DATA_TYPE;
                }

                // var->cvalue.arr->length = alloc->cvalue.arr->length;

                //var->cvalue.arr->length = len.cvalue.i64;
                arr->flags |= IS_ALLOCATED;

                // whatever

                Variable lenVar;
                const int err = evaluateArrayLength(alloc, &lenVar);
                if (err < 0) {
                    Logger::log(Logger::ERROR, "!!! evaluateArrayLength failed with error: %i !!! ", var->span, 0, err);
                    return err;
                }

                alloc->cvalue.arr->length->cvalue.hasValue = lenVar.cvalue.hasValue;
                alloc->cvalue.arr->length->cvalue.i64 = lenVar.cvalue.i64;
                alloc->cvalue.arr->length->cvalue.dtypeEnum = DT_I64;
                alloc->cvalue.arr->length->expression = lenVar.expression;
                //var->cvalue.arr->pointsTo = NULL;

                var->cvalue.arr->length = alloc->cvalue.arr->length;

                continue;

            }

            stripWrapperExpressions(var);

            if (arr->length && (arr->length->cvalue.hasValue || arr->length->expression)) {
                stripWrapperExpressions(arr->length);
                const int dt = evaluateDataTypes(arr->length);
                if (dt > 0 && dt <= DT_U64) {
                    if (!(arr->length->cvalue.hasValue)) {
                        const int err = evaluate(arr->length);
                        if (err < 0) {
                            Logger::log(Logger::ERROR, "TODO error: array length has to be known at compile time!", var->span, 1);
                            return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                        }
                    }
                    continue;
                } else {
                    Logger::log(Logger::ERROR, "TODO error: array length has to be int!", var->span, 1);
                    return Err::INVALID_DATA_TYPE;
                }
            }

            int len;
            int err;
            Variable lenVar;

            // stripWrapperExpressions(var);

            err = evaluateArrayLength(var, &lenVar);
            if (err < 0) {
                Logger::log(Logger::ERROR, "!!! evaluateArrayLength failed with error: %i !!! ", var->span, 0, err);
                return err;
            }

            //translatorC.printVariable(stdout, 0, &lenVar, NULL);
            //printf("\n");

            len = lenVar.cvalue.i64;

            if (arr->length->cvalue.hasValue && arr->length->cvalue.i64 != len) {
                Logger::log(Logger::ERROR, "TODO error: array and alloc size mismatch, you can omit array size while initializing array with rvalue...");
                return Err::INVALID_LVALUE;
            }

            if (len == 0) {
                Logger::log(Logger::ERROR, "Array length cannot be zero!", var->span, var->nameLen);
                return Err::INVALID_ARRAY_LENGTH;
            }

            var->cvalue.arr->length->cvalue.hasValue = 1;
            var->cvalue.arr->length->cvalue.i64 = len;
            var->cvalue.arr->length->cvalue.dtypeEnum = DT_I64;
            //var->cvalue.arr->pointsTo = NULL;
            var->cvalue.hasValue = 1;

        }



        // link function calls with definitions, verify arguments etc.
        for (int i = 0; i < (int) NodeRegistry::fcnCalls.size(); i++) {
            const int err = validateFunctionCall(NodeRegistry::fcnCalls[i]);
            if (err < 0) return err;
        }



        // validate return statements
        for (int i = 0; i < NodeRegistry::returnStatements.size(); i++) {

            ReturnStatement* rt = NodeRegistry::returnStatements[i];
            Function* fc = rt->fcn;
            Value* outValue = &(fc->outArg->var->cvalue);

            if (!fc) {
                Logger::log(Logger::ERROR, "Unexpected return statement outside of the function!", rt->span);
                return Err::UNEXPECTED_SYMBOL;
            }

            if (outValue->dtypeEnum != DT_VOID && !rt->var && !rt->err) {
                Logger::log(Logger::ERROR, "Not enough return arguments!", rt->span); // TODO : better error message
                return Err::NOT_ENOUGH_ARGUMENTS;
            }

            if (outValue->dtypeEnum == DT_VOID && rt->var) {
                Logger::log(Logger::ERROR, "Too many return arguments!", rt->span); // TODO : better error message
                return Err::TOO_MANY_ARGUMENTS;
            }

            if (rt->var) {

                const int rtType = evaluateDataTypes(rt->var);
                if (rtType < 0) return rtType;

                // TODO : for now meh, later create validateCast that takes values
                int err;
                if (outValue->dtypeEnum == DT_POINTER) {
                    Logger::mute = 1;
                    err = validatePointerAssignment(&(rt->var->cvalue));
                    Logger::mute = 0;
                    if (err < 0) {
                        err = validateImplicitCast(rt->var->cvalue.any, outValue->any, (DataTypeEnum)rtType, outValue->dtypeEnum);                    
                    }
                } else {
                    err = validateImplicitCast(rt->var->cvalue.any, outValue->any, (DataTypeEnum)rtType, outValue->dtypeEnum);
                }

                if (err < 0) {
                    int len = outValue->dtypeEnum == DT_CUSTOM ? fc->outArg->dtype->nameLen : dataTypes[outValue->dtypeEnum].nameLen;
                    Logger::log(Logger::PLAIN, "", rt->span);
                    Logger::log(Logger::ERROR, "In following declaration:", fc->outArg->span, len);
                    return err;
                }
                /*
                const int err = validateImplicitCast(rt->var->cvalue.any, outValue->any, (DataTypeEnum) rtType, outValue->dtypeEnum);
                if (err < 0) {
                    if (outValue->dtypeEnum == DT_POINTER) {
                        const int err = validatePointerAssignment(&(rt->var->cvalue));
                        if (err < 0) return err;
                    }
                    return err;
                }
                if (!validateImplicitCast((DataTypeEnum) rtType, fc->outArg)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_TYPE_CONVERSION), rt->span, 1, (dataTypes + rtType)->name, (dataTypes + fc->outArg)->name);
                    return Err::INVALID_TYPE_CONVERSION;
                }
                */
            
            }

            /*
            if (fc->errorSet && rt->err) {
                
                Namespace* nspace;
                ErrorSet* eset;

                const int err = validateScopeNames(fc->scope, rt->err->scopeNames, &nspace, &eset);
                if (!err) return err;

                if (!err) {
                    Logger::log(Logger::ERROR, "Expected error set, pure namespace given!", rt->span);
                    return Err::UNKNOWN_ERROR_SET;
                }
            
            }
            */
        
        }




        // var assignments
        //

        // so, if lvalue is
        //  1) just variable, then, after evaluation, variable is the answer,
        //  2) expression, then, after evaluation, the first expression has to be unary and operator be 'get value' or binary and operator be 'subscript'
        for (int i = 0; i < NodeRegistry::variableAssignments.size(); i++) {
            
            VariableAssignment* const varAss = NodeRegistry::variableAssignments[i];

            TypeDefinition* dtypeLDef = NULL;

            DataTypeEnum dtypeL = (DataTypeEnum) evaluateDataTypes(varAss->lvar, &dtypeLDef);
            if (dtypeL < Err::OK) return dtypeL;
            
            if (varAss->lvar->expression) {
                stripWrapperExpressions(&(varAss->lvar));
            }
            //stripWrapperExpressions((varAss->lvar));

            Expression* lvalueEx = (varAss->lvar)->expression;

            // TODO : exclude binary expression check?
            //
            //
            if (!(
                    !(lvalueEx) || 
                    (lvalueEx->type == EXT_UNARY && ((UnaryExpression*) lvalueEx)->operType == OP_GET_VALUE) ||
                    (lvalueEx->type == EXT_BINARY && (((UnaryExpression*) lvalueEx)->operType == OP_SUBSCRIPT || IS_MEMBER_SELECTION(((UnaryExpression*) lvalueEx)->operType) )) ||
                    (lvalueEx->type == EXT_SLICE)
                )
            ) {
                Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_LVALUE), varAss->span, varAss->span->end.idx);// - varAss->lvar->span->idx);
                return Err::INVALID_LVALUE;
            }

            if (varAss->lvar->def && varAss->lvar->def->flags & IS_CONST) {
                Logger::log(Logger::ERROR, ERR_STR(Err::CANNOT_ASSIGN_TO_CONST), varAss->span, 1);
                return Err::CANNOT_ASSIGN_TO_CONST;
            }

            if (varAss->rvar->snFlags & IS_ALLOCATION) {
                if (dtypeL == DT_ARRAY) {
                    Array* arr = (Array*) dtypeLDef;
                    if (!(arr->flags & IS_ARRAY_LIST || arr->flags & IS_DYNAMIC)) {
                        Logger::log(Logger::ERROR, "Cannot realocate array of const length!", varAss->rvar->span, 0);
                        return Err::CANNOT_ASSIGN_TO_CONST;
                    }
                }
            }

            stripWrapperExpressions(&(varAss->rvar));

            DataTypeEnum dtypeR = (DataTypeEnum) evaluateDataTypes(varAss->rvar, NULL, dtypeL, dtypeLDef);
            if (dtypeR < 0) return dtypeR;

            if (dtypeL == DT_CUSTOM) {

                if (dtypeR != DT_CUSTOM) {
                    Logger::log(Logger::ERROR, "Invalid type conversion, rvalue should be proper type initialization!", varAss->span, 0);
                    return Err::INVALID_TYPE_CONVERSION;
                }

                // const int err = validateTypeInitializations(dtypeLDef, varAss->rvar);

                // TypeInitialization* tinit = (TypeInitialization*) varAss->rvar->expression;
                //const int err = evaluateTypeInitialization(varAss->rvar, dtypeLDef->vars.size(), &tinit);

                // int err = validateCustomTypeInitialization(dtypeLDef, tinit);
                // varAss->rvar->expression = tinit;

                // if (err < 0) return err;

                continue;
            
            } else if (dtypeL == DT_FUNCTION) {
                Logger::log(Logger::ERROR, "WEEE");
                continue;
            }

            if (lvalueEx && lvalueEx->type == EXT_SLICE && dtypeR != DT_ARRAY) {
                dtypeL = ((Slice*) lvalueEx)->arr->cvalue.arr->pointsToEnum;
            }

            const int err = validateImplicitCast(varAss->rvar->cvalue.any, varAss->lvar->cvalue.any, dtypeR, dtypeL);
            if (err < 0) {
                if (dtypeL == DT_POINTER) {
                    const int err = validatePointerAssignment(&(varAss->rvar->cvalue));
                    if (err < 0) return err;
                }
                return err;
            }

            if (dtypeR == DT_ARRAY) {

                Variable lenVar;
                const int err = evaluateArrayLength(varAss->rvar, &lenVar);
                if (err < 0) return err;

                if (!lvalueEx) continue;

                if (lvalueEx->type == EXT_SLICE) {

                    Slice* sex = (Slice*)(varAss->lvar->expression);
                    if (sex->eidx->cvalue.dtypeEnum == DT_UNDEFINED) {
                        copy(sex->eidx, &lenVar);
                        sex->eidx->cvalue.dtypeEnum = DT_UNDEFINED;
                    }

                } else if (lvalueEx->type == EXT_BINARY) {

                    BinaryExpression* bex = (BinaryExpression*) lvalueEx;
                    if (bex->operType == OP_SUBSCRIPT) {
                        if (!(bex->operandA->cvalue.arr->length)) {
                            bex->operandA->cvalue.arr->length = new Variable();
                            copy(bex->operandA->cvalue.arr->length, &lenVar);
                            bex->operandA->cvalue.arr->length->cvalue.dtypeEnum = DT_UNDEFINED;
                        } else if (bex->operandA->cvalue.arr->length->cvalue.dtypeEnum == DT_UNDEFINED) {
                            copy(bex->operandA->cvalue.arr->length, &lenVar);
                            bex->operandA->cvalue.arr->length->cvalue.dtypeEnum = DT_UNDEFINED;
                        }
                    }

                }
            }

        }



        // validationg loops
        for (int i = 0; i < NodeRegistry::loops.size(); i++) {
            
            Loop* loop = NodeRegistry::loops[i];
            
            
            stripWrapperExpressions(&(loop->array));

            DataTypeEnum dtype = (DataTypeEnum) evaluateDataTypes(loop->array);
            if (dtype != DT_ARRAY) {
                Logger::log(Logger::ERROR, "Only array is allowed!", loop->span, strlen(Lex::KWS_LOOP));
                return Err::INVALID_DATA_TYPE;
            }

            Variable lenVar;
            const int err = evaluateArrayLength(loop->array, &lenVar);
            if (err < 0) return err;

            //*(loop->array->cvalue.arr->length) = lenVar;
            /*
            DataTypeEnum dtype = (DataTypeEnum) evaluate(loop->array);
            if (dtype < Err::OK) {
                Logger::log(Logger::ERROR, "ASD", loop->span, strlen(KWS_LOOP));
                return dtype;
            }
            */

        }



        // initializations
        for (int i = 0; i < NodeRegistry::initializations.size(); i++) {

            VariableDefinition* const varDef = NodeRegistry::initializations[i];
            Variable* var = varDef->var;
            
            const Value value = var->cvalue;
            DataTypeEnum dtypeEnumRef = var->cvalue.dtypeEnum;
            void* dtypeRef = var->cvalue.any;

            void* dtype;
            int dtypeEnum = evaluateDataTypes(var, (TypeDefinition**) (&dtype), dtypeEnumRef, (TypeDefinition*) (dtypeRef));
            if (dtypeEnum < 0) return dtypeEnum;
            var->cvalue = value;

            // validateCustomTypeInitialization(dtype, dtypeInit);
            Logger::mute = 1;
            const int err = validateImplicitCast(dtype, dtypeRef, (DataTypeEnum) dtypeEnum, dtypeEnumRef);
            Logger::mute = 0;
            if (err < 0) {
                if (dtypeEnumRef == DT_POINTER) {
                    stripWrapperExpressions(&var);
                    Value* val = &((var->def) ? var->def->var->cvalue : var->cvalue);
                    const int err = validatePointerAssignment(val);
                    if (err < 0) return err;
                } else {
                    Logger::log(Logger::ERROR, "TODO: Invalid type conversion!", var->span);
                    return err;
                }
            }

            if (dtypeEnumRef == DT_FUNCTION) {
                
                Variable* tmp;
                stripWrapperExpressions(var, &tmp);

                Function* fcn = findExactFunction(varDef->scope, tmp, value.fcn);
                if (!fcn) {
                    Logger::log(Logger::ERROR, "Function doesnt match pointer definition!", var->span, var->nameLen);
                    return Err::NO_MATCHING_FUNCTION_FOUND;
                }

                tmp->cvalue.fcn = fcn;
                tmp->id = fcn->id;

            }

        }

        // branch expressions
        for (int i = 0; i < NodeRegistry::branchExpressions.size(); i++) {

            Variable* op = NodeRegistry::branchExpressions[i];
            int dtype = evaluateDataTypes(op);
            if (IS_INT(dtype) || dtype == DT_POINTER) continue;

            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_DATA_TYPE), op->span, 1);
            return Err::INVALID_DATA_TYPE;

        }

        // switch cases
        for (int i = 0; i < NodeRegistry::switchCases.size(); i++) {
            
            SwitchCase* const sw = NodeRegistry::switchCases[i];
            
            int dtype = evaluateDataTypes(sw->switchExp);
            if (dtype < 0) {
                Logger::log(Logger::ERROR, "TODO error", sw->span, 1);
                return dtype;
            }

            for (int i = 0; i < sw->casesExp.size(); i++) {
                
                int dtype = evaluateDataTypes(sw->casesExp[i]);
                if (dtype < 0) {
                    Logger::log(Logger::ERROR, "", sw->span, 1);
                    return dtype;
                }

                dtype = evaluate(sw->casesExp[i]);
                if (dtype < 0) return dtype;

            }

        }

        // statements
        for (int i = 0; i < NodeRegistry::statements.size(); i++) {

            Statement* st = NodeRegistry::statements[i];
            Variable* op = st->op;
            int dtype = evaluateDataTypes(op);
            if (dtype < 0) return dtype; 

        }

        return Err::OK;

    }

    // just wrapper function for now
    int evaluateArrayLength(Variable* var, uint64_t flag) {

        Variable* len = new Variable();
        Array* arr = new Array();
        const int err = evaluateArrayLength(var, len, flag);
        if (err < 0) {
            delete len;
            delete arr;
            return err;
        }

        *arr = *(var->cvalue.arr);
        arr->length = len;
        //len->snFlags = IS_LENGTH;
        len->cvalue.dtypeEnum = DT_U64;
        var->cvalue.arr = arr;

        return Err::OK;
    
    }

    // len parameter is used to store expression that describes 
    // the length of the array
    // out represent internal variable representing length of array
    // defined by calculated expression
    // TODO : make evaluateDataType to also validate if
    //        operands of expression can interact with operator
    int evaluateArrayLength(Variable* var, Variable* len, uint64_t flag) {

        Expression* ex = var->expression;
        if (!ex) {

            if (var->cvalue.dtypeEnum < DT_STRING) {
                return Err::OK;
            }
            
            if (!(len->expression)) {
            
                Variable* lenVar = var->cvalue.arr->length;
                if (lenVar) {
                    if (lenVar->expression && !(len->cvalue.hasValue)) {
                        len->expression = lenVar->expression;
                        len->unrollExpression = 0;
                    }
                    else {
                        len->cvalue = lenVar->cvalue;
                    }
                }

                len->def = var->def;
                len->snFlags |= flag;

            } else if (len->expression->type == EXT_BINARY) {
                
                BinaryExpression* obex = (BinaryExpression*) len->expression;
                BinaryExpression* nbex = new BinaryExpression();

                nbex->operandA = var;
                nbex->operandB = new Variable();
                nbex->operandB->expression = obex;

                len->expression = nbex;
                len->unrollExpression = 0;

            } else {

            }
            
            return Err::OK;
        
        }

        switch (ex->type) {

            case EXT_UNARY : {
                
                UnaryExpression* uex = (UnaryExpression*) ex;
                
                const int err = evaluateArrayLength(uex->operand, len, flag);
                if (err < 0) return err;

                return Err::OK;

            }

            case EXT_BINARY : {
                
                int err;
                BinaryExpression* bex = (BinaryExpression*) ex;
                
                Variable lenA;
                err = evaluateArrayLength(bex->operandA, &lenA, flag);
                if (err < 0) return err;

                Variable lenB;
                err = evaluateArrayLength(bex->operandB, &lenB, flag);
                if (err < 0) return err;
                
                if (bex->operType == OP_CONCATENATION) {
                    // the only thing that can increase size
                    
                    if (lenA.cvalue.hasValue && lenB.cvalue.hasValue) {
                        len->cvalue.hasValue = 1;
                        len->cvalue.dtypeEnum = DT_I64;
                        len->cvalue.i64 += lenA.cvalue.i64 + lenB.cvalue.i64;
                        return Err::OK;
                    }

                    BinaryExpression* bex = new BinaryExpression();
                    // bex->oper = const_cast<Operator*>(&operators[OP_ADDITION]);// + OP_ADDITION;
                    bex->operType = OP_ADDITION;
                    bex->operandA = new Variable(&lenA);
                    bex->operandB = new Variable(&lenB);

                    len->expression = bex;
                    len->unrollExpression = 0;

                    //len->def = var->def;
                    //len->snFlags = IS_LENGTH;
                    
                    return Err::OK;
                
                }

                if (!(len->expression)) {
                    
                    Variable* lenVar = &lenA;
                    if (lenVar->expression) {
                        len->expression = lenVar->expression;
                        len->unrollExpression = 0;
                    } else {
                        len->cvalue = lenVar->cvalue;
                    }

                    len->def = lenA.def;
                    len->snFlags |= flag;

                }

                return Err::OK;
            
            }

            case EXT_WRAPPER : {
                
                WrapperExpression* wex = (WrapperExpression*) ex;
                
                const int err = evaluateArrayLength(wex->operand, len, flag);
                if (err < 0) return err;

                return Err::OK;

            }

            case EXT_FUNCTION_CALL : {

                FunctionCall* fex = (FunctionCall*) ex;
                Variable* lenVar = fex->outArg->cvalue.arr->length;

                if (!(len->expression)) {
            
                    len->cvalue = lenVar->cvalue;

                } else if (len->expression->type == EXT_BINARY) {
                    
                    BinaryExpression* obex = (BinaryExpression*) len->expression;
                    BinaryExpression* nbex = new BinaryExpression();

                    nbex->operandA = lenVar;
                    nbex->operandB = new Variable();
                    nbex->operandB->expression = obex;

                    len->expression = nbex;

                }

                return Err::OK;

            }

            case EXT_ARRAY_INITIALIZATION : {
                    
                if (len->expression) return Err::OK;

                len->cvalue.hasValue = 1;
                len->cvalue.dtypeEnum = DT_I64;
                len->cvalue.i64 = ((ArrayInitialization*) (var->expression))->attributes.size();
                len->snFlags |= flag;

                return Err::OK;

            }
            
            case EXT_SLICE : {

                if (len->expression) return Err::OK;

                Slice* slice = (Slice*) var->expression;
                int isEndLength = 0;

                evaluate(slice->eidx);
                evaluate(slice->bidx);
                
                // dtypeEnum == DT_UNDEFINED -> no definition
                // hasValue == 0 -> runtime value

                if (slice->bidx->cvalue.dtypeEnum == DT_UNDEFINED && slice->eidx->cvalue.dtypeEnum == DT_UNDEFINED) {
                    // should never appear on right side
                    return Err::INVALID_RVALUE;
                }

                if (slice->eidx->cvalue.dtypeEnum == DT_UNDEFINED) {
                    // edtype is len of array
                    
                    // slice->eidx; new Variable();
                    const int err = evaluateArrayLength(slice->arr, slice->eidx);
                    if (err < 0) return err;

                    isEndLength = 1;

                }

                if (!slice->bidx->cvalue.hasValue || !slice->eidx->cvalue.hasValue) {
                    // expression cannot be computed cmp-time
                    
                    slice->len = new Variable();
                    slice->len->cvalue.dtypeEnum = DT_U64;

                    BinaryExpression* len = new BinaryExpression();
                    len->operandA = slice->eidx;
                    len->operandB = slice->bidx;
                    len->operType = OP_SUBTRACTION;

                    slice->len->expression = len;

                    return Err::OK;

                }

                Value ans = slice->eidx->cvalue;
                ans.dtypeEnum = DT_I64;
                Interpreter::applyOperator(OP_SUBTRACTION, &ans, &(slice->bidx->cvalue));
                
                if (!slice->len) slice->len = new Variable();
                slice->len->cvalue = ans;
                slice->len->expression = NULL;
                
                len->cvalue.hasValue = 1;
                len->cvalue.dtypeEnum = DT_I64;
                len->cvalue.i64 = ans.i64 + (isEndLength ? 0 : 1);

                return Err::OK;

            } 
            
            case EXT_STRING_INITIALIZATION : {

                if (len->expression) return Err::OK;

                StringInitialization* init = (StringInitialization*) var->expression;

                len->cvalue.hasValue = 1;
                len->cvalue.dtypeEnum = DT_I64;

                if (init->wideStr) {
                    len->cvalue.i64 = init->wideLen;
                } else {
                    len->cvalue.i64 = init->rawStr.size();
                }

                return Err::OK;

            }

        }

        return Err::OK;

    }

    Variable* findErrorInErrorSet(ErrorSet* eset, QualifiedName* var) {

        Namespace* tmpNspace;
        
        int i = 0;
        const int len = var->path.size();
        
        for (i = 0; i < len; i++) {

            INamedLoc* nm = var->path[i];
            Variable* tmp = Utils::find(eset->vars, nm->name, nm->nameLen);
            if (!tmp) return NULL;
            
            eset = tmp->cvalue.err;

        }

        return Utils::find(eset->vars, var->name, var->nameLen);
        
    }

    // scope cannot be NULL
    Variable* findDefinition(Scope* scope, QualifiedName* const inVar, int idx) {

        char* const name = inVar->name;
        const int nameLen = inVar->nameLen;
        // int idx = inVar->parentIdx;

        while (scope) {

            std::unordered_map<std::string_view, SyntaxNode*> nodes = scope->defSearch;
            auto it = nodes.find(std::string_view(name, nameLen));
            if (it != nodes.end()) {
                
                SyntaxNode* node = it->second;
                
                if (node->parentIdx < idx) {
                    switch (node->type) {
                    
                        case NT_VARIABLE : {
                            return (Variable*) node;
                            //Variable* var = (Variable*) node;
                            //if (Utils::match(var, inVar)) return var;
                        }
                
                    }
                }

            } else {

                for (int i = 0; i < scope->usings.size(); i++) {
                    
                    Using* usng = scope->usings[i];
                    if (usng->var->type != NT_FUNCTION) continue;
                
                    Variable* err = findErrorInErrorSet(((Function*) (usng->var))->errorSet, inVar);
                    if (err) return err;

                }

            }

            idx = scope->parentIdx;
            scope = scope->scope;
            
        }
        
        return NULL;

    }
    
    template <typename T>
    inline int isUniqueInCollection(std::vector<T*> collection, INamed* named, Span* span) {

        for (int i = 0; i < collection.size(); i++) {

            T* const item = collection[i];
            if (item->snFlags & IS_UNIQUE) continue;

            if (Utils::match(named, item)) {
                Logger::log(Logger::ERROR, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), span, named->nameLen);
                return Err::SYMBOL_ALREADY_DEFINED;
            }
            //item->snFlags |= IS_UNIQUE;
        
        }

        return Err::OK;

    }

    int isUnique(Scope* sc, INamed* named, Span* span) {

        if (isUniqueInCollection(sc->defs, named, span) < 0) return Err::SYMBOL_ALREADY_DEFINED;
        if (isUniqueInCollection(sc->fcns, named, span) < 0) return Err::SYMBOL_ALREADY_DEFINED;
        if (isUniqueInCollection(sc->customDataTypes, named, span) < 0) return Err::SYMBOL_ALREADY_DEFINED;
        if (isUniqueInCollection(sc->labels, named, span) < 0) return Err::SYMBOL_ALREADY_DEFINED;

        return Err::OK;
    
    }

    int match(FunctionPrototype* const fptrA, FunctionPrototype* const fptrB) {
        
        if (fptrA->inArgs.size() != fptrB->inArgs.size()) return 0;

        for (int i = 0; i < fptrA->inArgs.size(); i++) {
            const DataTypeEnum defA = fptrA->inArgs[i]->var->cvalue.dtypeEnum;
            const DataTypeEnum defB = fptrB->inArgs[i]->var->cvalue.dtypeEnum;
            if (defA != defB) return 0;
        }

        return 1;
    
    }

    Function* findExactFunction(Scope* scope, INamed* const name, FunctionPrototype* const fptr) {

        while (scope) {

            const std::vector<Function*> fcns = scope->fcns;

            for (int i = 0; i < fcns.size(); i++) {
                if (!Utils::match(fcns[i], name) || !match(fptr, fcns[i])) {
                    continue;
                }
                return fcns[i];
            }

            scope = scope->scope;

        }

        return NULL;

    }

    enum Score {
        FOS_IMPLICIT_CAST = 1,
        FOS_SIZE_DECREASE,
        FOS_SIGN_CHANGE,
        FOS_TO_FLOAT,
        FOS_PROMOTION,
        FOS_EXACT_MATCH,
    };

    // working with global array fCandidates
    void findCandidateFunctions(Scope* scope, FunctionCall* call) {

        fCandidates.clear();

        const char* const name = call->name;
        const int nameLen = call->nameLen;

        if (call->path.size() > 0) {

            Namespace* nspace = NULL;
            ErrorSet* eset = NULL;

            Logger::mute = 1;
            const int err = validateScopeNames(scope, call->path, &nspace, &eset);
            Logger::mute = 0;

            if (err < 0 || eset || !nspace) return;
            scope = nspace;

        }

        while (scope) {

            const std::vector<Function*> fcns = scope->fcns;

            for (int i = 0; i < (int)fcns.size(); i++) {

                if (nameLen != fcns[i]->nameLen) continue;

                char* const itemName = fcns[i]->name;
                const int itemNameLen = fcns[i]->nameLen;

                int j = 0;
                for (; j < itemNameLen; j++) {
                    if (itemName[j] != name[j]) break;
                }

                if (j == itemNameLen) {

                    const int fcnInCnt = fcns[i]->inArgs.size();
                    const int callInCnt = call->inArgs.size();
                    const int lastArgDtype = fcnInCnt > 0 ? fcns[i]->inArgs[fcnInCnt - 1]->var->cvalue.dtypeEnum : DT_UNDEFINED;
                    
                    if (
                        fcnInCnt == callInCnt ||
                        (fcnInCnt < callInCnt && lastArgDtype == DT_MULTIPLE_TYPES)
                    ) {
                        fCandidates.push_back({ fcns[i], 0 });
                    }

                }

            }

            scope = scope->scope;

        }

    }

    int findClosestFunction(Operand* callOp, Function** outFcn) {

        Scope* scope = callOp->scope;
        FunctionCall* call = (FunctionCall*)callOp->expression;
        const int callInCnt = call->inArgs.size();

        findCandidateFunctions(scope, call);

        for (int j = 0; j < fCandidates.size(); j++) {

            Function* fcn = fCandidates[j].fcn;
            int score = (fcn->inArgs.size() == 0) ? 100 : 0;
            const int fcnInCnt = fcn->inArgs.size();

            int k = 0;
            for (int i = 0; i < fcn->inArgs.size(); i++) {

                if (k >= callInCnt) {
                    score = 0;
                    break;
                }

                Variable* fArg = fcn->inArgs[i]->var;
                Variable* cArg = call->inArgs[k];

                k++;

                const int fDtype = fArg->cvalue.dtypeEnum;
                const int cDtype = evaluateDataTypes(cArg);

                if (
                    (fDtype == DT_CUSTOM && cDtype != DT_CUSTOM) ||
                    (fDtype != DT_CUSTOM && cDtype == DT_CUSTOM)
                    ) {
                    score = 0;
                    break;
                }
                else if (fDtype == DT_CUSTOM) {
                    if (fArg->cvalue.def != cArg->cvalue.def) {
                        score = 0;
                        break;
                    }
                }

                if (fDtype == DT_MULTIPLE_TYPES) {
                    break;
                }

                if (fDtype == cDtype) {
                    score += FOS_EXACT_MATCH;
                    continue;
                }

                if (IS_SIGNED_INT(cDtype) && IS_SIGNED_INT(fDtype)) {
                    if (cDtype < fDtype) {
                        score += FOS_PROMOTION;
                    }
                    else {
                        score += FOS_SIZE_DECREASE;
                    }
                    continue;
                }

                if (IS_UNSIGNED_INT(cDtype) && IS_UNSIGNED_INT(fDtype)) {
                    if (cDtype < fDtype) {
                        score += FOS_PROMOTION;
                    }
                    else {
                        score += FOS_SIZE_DECREASE;
                    }
                    continue;
                }

                if (
                    IS_SIGNED_INT(cDtype) && IS_UNSIGNED_INT(fDtype) ||
                    IS_SIGNED_INT(cDtype) && IS_UNSIGNED_INT(fDtype)
                    ) {
                    if ((cDtype - fDtype <= DT_U64 - DT_I64)) {
                        score += FOS_SIGN_CHANGE;
                    }
                    else {
                        score += FOS_SIZE_DECREASE;
                    }
                    continue;

                }

                if (IS_FLOAT(cDtype) && IS_FLOAT(fDtype)) {
                    if ((cDtype - fDtype <= DT_U64 - DT_I64)) {
                        score += FOS_PROMOTION;
                    }
                    else {
                        score += FOS_SIZE_DECREASE;
                    }
                    continue;
                }

                if (IS_INT(cDtype) && IS_FLOAT(fDtype)) {
                    score += FOS_SIGN_CHANGE;
                    continue;
                }

                if (IS_FLOAT(cDtype) && IS_INT(fDtype)) {
                    score += FOS_SIZE_DECREASE;
                    continue;
                }

                if (validateImplicitCast((DataTypeEnum)cDtype, (DataTypeEnum)fDtype)) {
                    score += FOS_IMPLICIT_CAST;
                    continue;
                }

                if (fDtype == DT_ARRAY && cDtype == DT_STRING) {
                    // TODO: for now as quick patch
                    score += FOS_IMPLICIT_CAST;
                    continue;
                }

                break;

            }

            if (score != 0) {
                fCandidates[j].score = score;
            } else {
                FunctionScore tmp = fCandidates[fCandidates.size() - 1];
                fCandidates[j] = tmp;
                fCandidates.pop_back();
                j--;
            }

        }

        int bestIdx = 0;
        int bestScore = 0;
        int sameScoreCnt = 0;
        for (int i = 0; i < fCandidates.size(); i++) {
            const int score = fCandidates[i].score;
            if (score > bestScore) {
                bestScore = score;
                bestIdx = i;
            } else if (score == bestScore) {
                sameScoreCnt++;
                continue;
            }
            sameScoreCnt = 0;
        }

        if (bestScore > 0 && sameScoreCnt == 0) {
            *outFcn = fCandidates[bestIdx].fcn;
            return Err::OK;
        } else {

            if (bestScore <= 0 ) {
                //Logger::log(Logger::ERROR, ERR_STR(Err::NO_MATCHING_FUNCTION_FOUND));
                return Err::NO_MATCHING_FUNCTION_FOUND;
            }

            //Logger::log(Logger::ERROR, ERR_STR(Err::MORE_THAN_ONE_OVERLOAD_MATCH), callOp->span, 1);
            return Err::MORE_THAN_ONE_OVERLOAD_MATCH;

        }

        return Err::OK;
        
    }

    // level has to be 0, if its output matters
    int getFirstNonArrayDtype(Array* arr, const int maxLevel, int* level) {
        
        const int dtype = arr->pointsToEnum;

        if (maxLevel > 0 && *level >= maxLevel) return dtype;
        if (dtype != DT_ARRAY) return dtype;

        if (level) *level = *level + 1;
        return getFirstNonArrayDtype((Array*) arr->pointsTo, maxLevel, level);
    
    }

    // TODO : to macro or constexpr
    int validateImplicitCast(const DataTypeEnum dtype, const DataTypeEnum dtypeRef) {

        const int basicTypes = (dtype != DT_VOID && dtypeRef != DT_VOID) && (dtype < DT_STRING && dtypeRef < DT_STRING);
        const int arrayToPointer = (dtype == DT_ARRAY && dtypeRef == DT_POINTER);
        const int pointerToArray = (dtypeRef == DT_ARRAY && dtype == DT_POINTER);
        const int sliceToArray = dtype == DT_SLICE && dtypeRef == DT_ARRAY;

        return (dtype == dtypeRef) || (basicTypes || arrayToPointer || pointerToArray || sliceToArray);

    }

    int validateImplicitCast(void* dtype, void* dtypeRef, DataTypeEnum dtypeEnum, DataTypeEnum dtypeEnumRef) {
        
        if (validateImplicitCast(dtypeEnum, dtypeEnumRef)) {
            return Err::OK;
        }

        if (dtypeEnumRef == DT_ARRAY && dtypeEnum == DT_STRING) {

            Array* arr = (Array*) dtypeRef;
            StringInitialization* str = (StringInitialization*) dtype;

            const int arrDtypeSize = dataTypes[arr->pointsToEnum].size;
            const int strDtypeSize = dataTypes[str->wideDtype].size;

            if (arrDtypeSize < strDtypeSize) {
                Logger::log(Logger::ERROR, "TODO error: ");
                return Err::INVALID_TYPE_CONVERSION;
            }

            if (!validateImplicitCast(arr->pointsToEnum, str->wideDtype)) {
                Logger::log(Logger::ERROR, "TODO error: ");
                return Err::INVALID_TYPE_CONVERSION;
            }

            if (arrDtypeSize < arrDtypeSize) {
                Logger::log(Logger::WARNING, "TODO warning: One can use smaller dtype");
            }

            return Err::OK;

        } else if (dtypeEnumRef == DT_ARRAY && dtypeEnum == DT_ARRAY) {
            
            Array* arrRef = (Array*) dtypeRef;
            Array* arr = (Array*) dtype;

            int levelRef = 0;
            const DataTypeEnum arrDtypeRef = (DataTypeEnum) getFirstNonArrayDtype(arrRef, -1, &levelRef);

            const int maxLevel = levelRef;
            levelRef = 0;
            const DataTypeEnum arrDtype = (DataTypeEnum) getFirstNonArrayDtype(arr, maxLevel, &levelRef);

            return validateImplicitCast(arrDtype, arrDtypeRef);

        
        } else if (dtypeEnumRef == DT_ARRAY) {

            Array* arr = (Array*) dtypeRef;
            const DataTypeEnum arrDtype = (DataTypeEnum) getFirstNonArrayDtype(arr);

            return validateImplicitCast(dtypeEnum, arrDtype);

        }

        Logger::log(Logger::ERROR, "TODO : invalid type conversion!");
        return Err::INVALID_TYPE_CONVERSION;

    }

    // used within validateTypeInitialization
    // think about better name
    int validateAttributeCast(Variable* var, Variable* attribute) {

        // Variable* op, TypeDefinition** customDtype, DataTypeEnum lvalueType, TypeDefinition* lvalueTypeDef
        const int id = attribute->id;
        char* const name = attribute->name;
        const int nameLen = attribute->nameLen;
        
        VariableDefinition* def = var->def;
        DataTypeEnum dtypeA = (DataTypeEnum) evaluateDataTypes(
            attribute,
            NULL,
            def ? def->var->cvalue.dtypeEnum : DT_UNDEFINED,
            def ? def->var->cvalue.def : NULL
        );

        attribute->id = id;
        attribute->name = name;
        attribute->nameLen = nameLen;

        DataTypeEnum dtypeB = (DataTypeEnum) evaluateDataTypes(var);

        if (!validateImplicitCast(dtypeB, dtypeA)) {
            return DT_UNDEFINED;
        }

        return Err::OK;

    }

    // assuming dtypeInit has at least one attribute
    // both TypeDefinition  has to be valid
    int validateTypeInitialization(TypeDefinition* dtype, TypeInitialization* dtypeInit) {
        
        const int count = dtype->vars.size();
        const int areNamed = dtypeInit->attributes.size() == 0 || dtypeInit->attributes[0]->name;

        if (count < dtypeInit->attributes.size()) {
            Logger::log(Logger::ERROR, ERR_STR(Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH), dtype->span, dtype->nameLen, dtypeInit->attributes.size(), dtype->vars.size());
            return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
        }

        if (dtype->type == NT_UNION) {

            if (dtypeInit->attributes.size() > 1) {
                Variable* var = dtypeInit->attributes[1];
                Logger::log(Logger::ERROR, "Only one attribute can be set while initializing union!", var->span, var->nameLen);
                return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
            }

            if (dtypeInit->fillVar) {
                Variable* var = dtypeInit->fillVar;
                Logger::log(Logger::ERROR, "Fill th rest option is not allowed while initializing union!", var->span, 2);
                return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
            }

            if (!areNamed) {
                Variable* var = dtypeInit->attributes[0];
                Logger::log(Logger::ERROR, "Only initialization through specifying name of attribute is allowed while initializing union!", var->span, 1);
                return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;    
            }

        }

        if (areNamed) {
            // treating all as named

            dtypeInit->idxs = (int*) malloc(sizeof(int) * count);
            for (int i = 0; i < count; i++) {
                dtypeInit->idxs[i] = -1;
            }
            
            for (int i = 0; i < dtypeInit->attributes.size(); i++) {

                Variable* attribute = dtypeInit->attributes[i];

                const int idx = Utils::findIdx(dtype->vars, attribute->name, attribute->nameLen);
                if (idx < 0) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_ATTRIBUTE_NAME), attribute->span, attribute->nameLen, attribute->nameLen, attribute->name);
                    return Err::INVALID_ATTRIBUTE_NAME;    
                }

                dtypeInit->idxs[idx] = i;
                attribute->id = dtype->vars[idx]->id;

                // typecheck
                // if (!attribute->expression) continue;

                const int err = validateAttributeCast(dtype->vars[idx], attribute);
                if (err < 0) return err;
                
            }

            if (dtypeInit->fillVar) {

                for (int i = 0; i < dtype->vars.size(); i++) {

                    Variable* attribute = dtype->vars[i];
                    if (dtypeInit->idxs[i] < 0) continue;
                    
                    // typecheck
                    // if (!attribute->expression) continue;
    
                    const int err = validateAttributeCast(attribute, dtypeInit->fillVar);
                    if (err < 0) return err;
                    
                }

            }

            return Err::OK;
        
        }

        for (int i = 0; i < dtypeInit->attributes.size(); i++) {

            Variable* attribute = dtypeInit->attributes[i];

            dtypeInit->idxs[i] = i;

            // typecheck
            if (!attribute->expression) continue;

            const int err = validateAttributeCast(dtype->vars[i], attribute);
            if (err < 0) return err;
        
        }

        return Err::OK;

    }

    // TODO
    int computeSizeOfDataType(TypeDefinition* const def) {

        if (def->size > 0) return def->size;

        const int isUnion = def->type == NT_UNION;

        int accSize = 0;
        for (int i = 0; i < def->vars.size(); i++) {
            
            Variable* const var = def->vars[i];
            
            int size = 0;
            switch (var->cvalue.dtypeEnum) {
                
                case DT_CUSTOM: {
                    size = computeSizeOfDataType(var->cvalue.def);
                    break;
                }

                case DT_ARRAY: {
                    
                    Array* const arr = var->cvalue.arr;
                    evaluate(arr->length);
                    
                    if (!(arr->length->cvalue.hasValue)) {
                        Logger::log(Logger::ERROR, "Was unable to compute array length while computing size of the %.*s!", var->span, var->nameLen, def->nameLen, def->name);
                        return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                    }

                    int chunkSize = 0;
                    if (arr->pointsToEnum == DT_CUSTOM) {
                        chunkSize = computeSizeOfDataType((TypeDefinition*) arr->pointsTo);
                    } else {
                        chunkSize = dataTypes[arr->pointsToEnum].size;
                    }

                    size = chunkSize * arr->length->cvalue.i64;

                    break;
                }

                default: {
                    size = dataTypes[var->def->var->cvalue.dtypeEnum].size;
                    break;
                }
            
            }

            if (isUnion) accSize = size > accSize ? size : accSize;
            else accSize += size;

        }

        def->size = accSize;
        return accSize;

    }

    int validateFunctionCall(Variable* fcnCallOp) {

        // Variable* fcnCallOp = SyntaxNode::fcnCalls[i];
        FunctionCall* fcnCall = (FunctionCall*) (fcnCallOp->expression);
        // Function* fcn = fcnCall->fcn;
        FunctionPrototype* fcn = fcnCall->fcn ? fcnCall->fcn : fcnCall->fptr->cvalue.fcn;
        const int fcnInCount = fcn->inArgs.size();
        // const int fcnCallInCount = fcnCall->inArgs.size();

        const int variableNumberOfArguments = fcnInCount > 0 && fcn->inArgs[fcnInCount - 1]->var->cvalue.dtypeEnum == DT_MULTIPLE_TYPES;

        // note: 
        //  array argument is parsed as two arguments (pointer and length) in definition

        int j = 0;
        for (int i = 0; i < fcnInCount - variableNumberOfArguments; i++) {

            if (j >= fcnCall->inArgs.size()) {
                Logger::log(Logger::ERROR, ERR_STR(Err::NOT_ENOUGH_ARGUMENTS), fcnCallOp->span, fcnCall->nameLen);
                return Err::NOT_ENOUGH_ARGUMENTS;
            }

            VariableDefinition* fcnVarDef = fcn->inArgs[i];
            Variable* fcnVar = fcnVarDef->var;
            Variable* fcnCallVar = fcnCall->inArgs[j];

            int fcnCallVarDtype;
            if (fcnCallVar->expression) {
                // TODOD : TypeDefinition** customDtype, DataTypeEnum lvalueType, TypeDefinition* lvalueTypeDef
                fcnCallVarDtype = evaluateDataTypes(fcnCallVar);
                if (fcnCallVarDtype < Err::OK) return fcnCallVarDtype;
            } else {
                fcnCallVarDtype = fcnCallVar->cvalue.dtypeEnum;
            }
            
            if (fcnVar->cvalue.dtypeEnum == DT_ARRAY) {
                
                if (!(fcnCallVar->cvalue.dtypeEnum == DT_ARRAY)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::ARRAY_EXPECTED), fcnCallVar->span, 1);
                    return Err::ARRAY_EXPECTED;
                }
                
                Variable* var = fcnCallVar;
                stripWrapperExpressions((Variable**) & var);
                if (!(var->def) || !(var->def->var->cvalue.arr)) {
                    Logger::log(Logger::ERROR, "TODO error: array expected!", fcnCallVar->span, fcnCallVar->nameLen);
                    return Err::ARRAY_EXPECTED;
                }

                const int err = evaluateArrayLength(var);
                if (err < 0) return err;

                fcnCall->inArgs[j] = var;

                /*
                Variable* lenVar = new Variable();
                const int err = evaluateArrayLength(var, lenVar);
                if (err < 0) return err;

                Value* val = new Value();
                val->arr

                var->cvalue.arr->length = lenVar;
                var->cvalue.arr->length->cvalue.dtypeEnum = DT_UINT_64;
                var->cvalue.arr->length->snFlags |= IS_LENGTH;
                */
                /*
                Array* arr = var->def->var->cvalue.arr;
                if (arr->flags & IS_ARRAY_LIST) {
                    fcnCall->inArgs.insert(fcnCall->inArgs.begin() + j + 1, arr->length);
                    i++;
                    j++;
                    continue;
                }

                // fcnVar->cvalue.arr->length = arr->length;
                fcnCall->inArgs.insert(fcnCall->inArgs.begin() + j + 1, arr->length); // fcnCallVar->def->var->cvalue.arr->length);
                // j++;
                */
            
            }
            
            if (!validateImplicitCast((DataTypeEnum) fcnCallVarDtype, fcnVar->cvalue.dtypeEnum)) {
                // error : cannot cast
                Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_TYPE_CONVERSION), fcnCallVar->span, 1, (dataTypes + fcnVar->cvalue.dtypeEnum)->name, (dataTypes + fcnCallVarDtype)->name);
                return Err::INVALID_TYPE_CONVERSION;
            }

            j++;
        
        }

        const int fcnCallInCount = fcnCall->inArgs.size();

        if (j < fcnCallInCount && !variableNumberOfArguments) {
            Logger::log(Logger::ERROR, ERR_STR(Err::TOO_MANY_ARGUMENTS), fcnCallOp->span, fcnCall->nameLen);
            return Err::TOO_MANY_ARGUMENTS;
        }
        
        for (; j < fcnCallInCount; j++) {
            
            Variable* var = fcnCall->inArgs[j];
            
            int err;
            if (var->def) {
                // TODO : ALLOC case for now
                
                Variable* tmp = var->def->var;
                const Value tmpValue = tmp->cvalue;
                const DataTypeEnum tmpDtype = tmp->cvalue.dtypeEnum;

                err = evaluateDataTypes(var, NULL, tmp->cvalue.dtypeEnum, tmp->cvalue.def);

                if (tmp->cvalue.dtypeEnum == DT_CUSTOM) {
                    // seems like evaluateDataTypes does the same thing
                    // validateTypeInitialization(tmp->cvalue.def, (TypeInitialization*) ((WrapperExpression*) (var->expression))->operand->expression);
                } else {
                    const int err = validateImplicitCast(var->cvalue.any, tmpValue.any, var->cvalue.dtypeEnum, tmpValue.dtypeEnum);
                    if (err < 0) return err;
                    // tmp->cvalue.dtypeEnum = tmpDtype;
                }

                tmp->cvalue = tmpValue;

            } else {
                err = evaluateDataTypes(var);
                if (err < 0) {
                    Logger::log(Logger::ERROR, "caused by %i argument", var->span, 1, j);
                }
            }

            if (err < 0) {
                return err;
            }
        
        }

        return Err::OK;

        // fcnCall->fcn = fcn;

    }

    int validateTypeInitializations(TypeDefinition* dtype, Variable* var) {

        Expression* ex = var->expression;
        if (!ex) {
            return Err::OK;
        }

        switch (ex->type) {

            case EXT_UNARY : {
                validateTypeInitializations(dtype, (Variable*) ((UnaryExpression*) ex)->operand);
                break;
            }

            case EXT_BINARY : {
                validateTypeInitializations(dtype, (Variable*) ((BinaryExpression*) ex)->operandA);
                validateTypeInitializations(dtype, (Variable*) ((BinaryExpression*) ex)->operandB);
                break;
            }

            case EXT_TERNARY : {
                break;
            }

            case EXT_WRAPPER : {
                validateTypeInitializations(dtype, (Variable*) ((WrapperExpression*)ex)->operand);
                break;
            }

            case EXT_FUNCTION_CALL : {
                break;
            }

            case EXT_TYPE_INITIALIZATION : {

                TypeInitialization* tinit = (TypeInitialization*) ex;
                
                int err = validateTypeInitialization(dtype, tinit);
                if (err < 0) return err;

            }

        }

        return Err::OK;

    }



    void stripWrapperExpressions(Variable* var, Variable** opOut) {

        *opOut = var;

        const Expression* const ex = var->expression;
        if (ex->type == EXT_WRAPPER) {
            Variable* tmp = ((WrapperExpression*) ex)->operand;
            *opOut = tmp;
        }

    }

    // makes sure, that first expression is meaningfull
    // ex.: ((1 + 2)) -> 1 + 2
    void stripWrapperExpressions(Variable** op) {

        const Expression* const ex = (*op)->expression;
        if (ex->type == EXT_WRAPPER) {
            Variable* tmp = ((WrapperExpression*) ex)->operand;
            if (tmp->expression) {
                (*op)->expression = tmp->expression;
            } else {
                *op = tmp;
            }
        }

    }

    void stripWrapperExpressions(Variable* op) {

        while (1) {
            
            const Expression* const ex = op->expression;
            if (!ex) return;

            if (ex->type == EXT_WRAPPER) {
                Expression* const newEx = ((WrapperExpression*) ex)->operand->expression;
                if (newEx) {
                    op->expression = newEx;
                } else {
                    break;
                }
            } else {
                break;
            }

        }

    }

    inline int validatePointerAssignment(const Value* const val) {
        if (val->hasValue && val->u64 == 0) return Err::OK;
        Logger::log(Logger::ERROR, "Only 0 could be assigned to a pointer variable!");
        return Err::INVALID_RVALUE;
    }



    /*
    int evaluateTypeInitialization(Variable* op, int attributesCount, TypeInitialization** tinitOut) {

        TypeInitialization* tinit = new TypeInitialization();
        
        int idx = -1;
        int nextIdx = -1;
        for (int i = 0; i < attributesCount; i++) {

            Variable* tmp = NULL;
            int err = copyExpression((Variable*) op, &tmp, &idx, &nextIdx);
            if (err < 0) return err;

            idx = nextIdx;
            nextIdx = -1;

            err = evaluateDataTypes(tmp);
            if (err < 0) { 
                Logger::log(Logger::ERROR, "TODO : Error, evaluateTypeInitialization -> evaluateDataTypes failed");
                return err;
            }
            
            tinit->attributes.push_back(tmp);
            tmp->name = NULL;
        
        }

        *tinitOut = tinit;

        return Err::OK;
        
    }
        */



    int evaluateDataTypes(Variable* op, TypeDefinition** customDtype, DataTypeEnum lvalueType, TypeDefinition* lvalueTypeDef) {

        Expression* ex = op->expression;
        if (!ex) {

            if (op->cvalue.dtypeEnum == DT_CUSTOM && customDtype) {
                *customDtype = op->cvalue.def;
            } else if (customDtype) {
                *customDtype = (TypeDefinition*) (op->cvalue.any);
            }

            return op->cvalue.dtypeEnum;
        
        }
        
        int rdtype = DT_UNDEFINED;

        switch (ex->type) {
            
            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) ex;
                
                const int dtype = evaluateDataTypes(uex->operand, customDtype);
                if (dtype < Err::OK) return dtype;
                
                switch (uex->operType) {
                    
                    case OP_GET_ADDRESS:
                        op->cvalue.ptr = uex->operand->cvalue.ptr;
                        rdtype = DT_POINTER;
                        break;

                    case OP_GET_VALUE: 
                        op->cvalue.any = uex->operand->cvalue.ptr->pointsTo;
                        rdtype = uex->operand->cvalue.ptr->pointsToEnum;
                        if (customDtype) *customDtype = (TypeDefinition*) op->cvalue.any;
                        break;

                    default:
                        if (op->cvalue.dtypeEnum == DT_CUSTOM) {
                            Logger::log(Logger::ERROR, "TODO error: unsupported operator with struct!");
                            return DT_UNDEFINED;
                        }
                        op->cvalue.any = uex->operand->cvalue.any;
                        rdtype = dtype;

                }

                break;

            }

            case EXT_BINARY: {
                
                BinaryExpression* bex = (BinaryExpression*) ex;

                TypeDefinition* customDtypeA;
                const int dtypeA = evaluateDataTypes(bex->operandA, &customDtypeA, lvalueType, lvalueTypeDef);
                if (dtypeA < Err::OK) return dtypeA;

                const int dtypeB = evaluateDataTypes(bex->operandB, customDtype, lvalueType, lvalueTypeDef);
                if (dtypeB < Err::OK) return dtypeB;

                const int oper = bex->operType;
                switch (oper) {

                    case OP_SUBSCRIPT: {

                        if (dtypeA == DT_ARRAY) {

                            Array* arr = bex->operandA->cvalue.arr;

                            if (customDtype) {
                                *customDtype = (TypeDefinition*)bex->operandA->cvalue.arr->pointsTo;
                            }

                            if (dtypeB == DT_UNDEFINED) {
                                // appending to array, so returning array
                                op->cvalue.any = arr;
                                rdtype = DT_ARRAY;                                
                                break;
                            }

                            if (arr->flags & IS_CMP_TIME) {


                            }

                            op->cvalue.any = arr->pointsTo;
                            rdtype = arr->pointsToEnum;

                        } else if (dtypeA == DT_POINTER) {

                            Pointer* ptr = bex->operandA->cvalue.ptr;

                            op->cvalue.any = ptr->pointsTo;
                            rdtype = ptr->pointsToEnum;

                            if (customDtype) {
                                *customDtype = (TypeDefinition*) bex->operandA->cvalue.ptr->pointsTo;
                            }
                        
                        } else {

                            Logger::log(Logger::ERROR, "Only array and pointer can be indexed!");
                            return Err::ARRAY_EXPECTED;
                        
                        }

                        break;
                    }
                    
                    case OP_DEREFERENCE_MEMBER_SELECTION:
                    case OP_MEMBER_SELECTION: {
                        
                        if (dtypeA == DT_ARRAY) {

                            if (Utils::match(bex->operandB, Lex::KWS_ARRAY_LENGTH)) {

                                const int err = evaluateArrayLength(bex->operandA, IS_LENGTH);
                                if (err < 0) return err;

                            } else if (Utils::match(bex->operandB, Lex::KWS_ARRAY_SIZE)) {

                                const int err = evaluateArrayLength(bex->operandA, IS_LENGTH | IS_SIZE);
                                if (err < 0) return err;

                            } else {
                                Logger::log(Logger::ERROR, "Unknown member of array!");
                                return Err::UNEXPECTED_SYMBOL;
                            }
                            
                            return DT_U64;

                        }

                        if (dtypeA == DT_ENUM) {

                            Enumerator* en = (Enumerator*) customDtypeA;
                            Variable* var = Utils::find(en->vars, bex->operandB->name, bex->operandB->nameLen);
                            if (!var) {
                                Logger::log(Logger::ERROR, "Unable to find member of enum!", bex->operandB->span, 0);
                                return Err::UNEXPECTED_SYMBOL;
                            }

                            // just a patch, be fixed other way, should happen with refactoring
                            // TODO : dont forget, otherwise memory leak...
                            WrapperExpression* tmp = new WrapperExpression();
                            tmp->operand = var;
                            op->expression = tmp;
                            
                            return ((Enumerator*) customDtypeA)->dtype;
                        
                        }

                        if (dtypeA == DT_POINTER) {

                            Variable* var = (Variable*) bex->operandB;
                            Pointer* ptr = (Pointer*) customDtypeA;

                            if (ptr->pointsToEnum != DT_CUSTOM || !ptr->pointsTo) {
                                Logger::log(Logger::ERROR, "Invalid type of dereferenced pointer for member selection!", var->span, var->nameLen);
                                return Err::INVALID_TYPE_CONVERSION;
                            }

                            bex->operType = OP_DEREFERENCE_MEMBER_SELECTION;
                            customDtypeA = (TypeDefinition*) (ptr->pointsTo);

                        } else if (dtypeA != DT_CUSTOM) {
                            Logger::log(Logger::ERROR, "Invalid type for member selection!", bex->operandB->span, bex->operandB->nameLen);
                            return Err::INVALID_TYPE_CONVERSION;
                        }

                        Variable* var = (Variable*) bex->operandB;
                        Variable* ans = Utils::find<Variable>(customDtypeA->vars, var->name, var->nameLen);
                        if (!ans) {
                            Logger::log(Logger::ERROR, ERR_STR(Err::INVALID_ATTRIBUTE_NAME), var->span, var->nameLen, var->nameLen, var->name);
                            return Err::INVALID_ATTRIBUTE_NAME;
                        }

                        copy(var, ans);

                        rdtype = ans->cvalue.dtypeEnum;
                        break;
                    
                    }

                    case OP_BOOL_AND :
                    case OP_BOOL_OR :
                    case OP_EQUAL :
                    case OP_NOT_EQUAL :
                    case OP_LESS_THAN :
                    case OP_GREATER_THAN :
                    case OP_LESS_THAN_OR_EQUAL :
                    case OP_GREATER_THAN_OR_EQUAL : {
                        
                        const int isAValid = dtypeA <= DT_F64 || dtypeA == DT_POINTER || dtypeA == DT_ERROR;
                        const int isBValid = dtypeB <= DT_F64 || dtypeB == DT_POINTER || dtypeB == DT_ERROR;
                        
                        if (isAValid || isBValid) {
                            rdtype = DT_INT;
                            break;
                        }

                        // TODO : error
                        return DT_UNDEFINED;
                    
                    }

                    case OP_BITWISE_AND :
                    case OP_BITWISE_OR :
                    case OP_BITWISE_XOR : {
                        
                        const int isAValid = dtypeA <= DT_U64;
                        const int isBValid = dtypeB <= DT_U64;

                        if (isAValid || isBValid) {
                            rdtype = DT_INT;
                            break;
                        }

                        // TODO : error
                        return DT_UNDEFINED;
                    
                    }

                    default:

                        if (dtypeA >= dtypeB) {
                            op->cvalue.any = bex->operandA->cvalue.any;
                            rdtype = dtypeA;
                        } else {
                            op->cvalue.any = bex->operandB->cvalue.any;
                            rdtype = dtypeB;
                        }

                }

                break;
                
            }

            case EXT_TERNARY: {

            }

            case EXT_WRAPPER: {
                
                WrapperExpression* wex = (WrapperExpression*) ex;

                const int dtype = evaluateDataTypes(wex->operand, customDtype, lvalueType, lvalueTypeDef);
                if (dtype < Err::OK) return dtype;

                //op->cvalue = wex->operand->cvalue;
                if (wex->operand->cvalue.any) {
                    // whatever quick patch, should never be a question
                    // now its tied to a definition, has to be indicator
                    op->cvalue.hasValue = wex->operand->cvalue.hasValue;
                    op->cvalue.any = wex->operand->cvalue.any;
                }
                
                rdtype = dtype;
                break;

            }

            case EXT_FUNCTION_CALL: {

                FunctionCall* fex = (FunctionCall*)ex;

                // TODO : why i do even check it here...
                /*
                for (int i = 0; i < fex->inArgs.size(); i++) {
                    if (fex->fcn->inArgs[i]->var->cvalue.dtypeEnum == DT_MULTIPLE_TYPES) continue;
                    const int err = evaluateDataTypes(fex->inArgs[i], customDtype, lvalueType, lvalueTypeDef);
                    if (err < 0) return err;
                }
                */


                op->cvalue.any = fex->outArg->cvalue.any;
                rdtype = fex->outArg->cvalue.dtypeEnum;

                break;

            }
            
            case EXT_CATCH : {
                
                Catch* cex = (Catch*) ex;
                FunctionCall* fex = cex->call;

                op->cvalue.any = fex->outArg->cvalue.any;
                rdtype = fex->outArg->cvalue.dtypeEnum;

                break;

            }

            case EXT_SLICE: {

                Slice* sex = (Slice*) ex;

                DataTypeEnum arrDtype = (DataTypeEnum) evaluateDataTypes(sex->arr);
                if (!validateImplicitCast(arrDtype, DT_ARRAY)) {
                    return DT_UNDEFINED;
                }

                DataTypeEnum idxDtype;

                idxDtype = (DataTypeEnum) evaluateDataTypes(sex->bidx);
                if (idxDtype == DT_UNDEFINED && !sex->bidx->expression) {
                    sex->bidx->cvalue.dtypeEnum = DT_I64;
                    sex->bidx->cvalue.hasValue = 1;
                    sex->bidx->cvalue.i64 = 0;
                } else if (!validateImplicitCast(idxDtype, DT_INT)) {
                    return DT_UNDEFINED;
                }
                
                idxDtype = (DataTypeEnum) evaluateDataTypes(sex->eidx);
                if ((idxDtype != DT_UNDEFINED || sex->eidx->expression) && !validateImplicitCast(idxDtype, DT_INT)) {
                    return DT_UNDEFINED;
                }

                if (customDtype) {
                    *customDtype = (TypeDefinition*) (sex->arr->cvalue.any);
                }

                return DT_ARRAY;
            
            }

            case EXT_STRING_INITIALIZATION: {

                StringInitialization* init = (StringInitialization*) ex;

                if (customDtype) {
                    *customDtype = (TypeDefinition*) ex;
                }

                return DT_STRING;

            }

            case EXT_ARRAY_INITIALIZATION: {

                ArrayInitialization* aex = (ArrayInitialization*) ex;

                if (lvalueType != DT_ARRAY) {
                    return DT_UNDEFINED;
                }

                Array* lvalueDtype = (Array*)lvalueTypeDef;
                DataTypeEnum dtypeB = (DataTypeEnum) lvalueDtype->pointsToEnum;

                DataTypeEnum initDtype = DT_UNDEFINED;
                for (int i = 0; i < aex->attributes.size(); i++) {

                    DataTypeEnum dtypeA = (DataTypeEnum) evaluateDataTypes(aex->attributes[i]);
                    if (i == 0) initDtype = dtypeA;

                    if (!validateImplicitCast(dtypeB, dtypeA)) {
                        return DT_UNDEFINED;
                    }

                }

                if (customDtype) *customDtype = lvalueTypeDef;

                if (op) {
                    op->cvalue.dtypeEnum = lvalueType;
                    op->cvalue.arr = new Array();
                    op->cvalue.arr->pointsToEnum = initDtype;
                    op->cvalue.arr->pointsTo = NULL;
                }
                return lvalueType;

            }

            case EXT_TYPE_INITIALIZATION: {

                if (lvalueType != DT_CUSTOM) {
                    Logger::log(Logger::ERROR, "TODO error: Invalid lvalue for type initialization used!");
                    return Err::INVALID_LVALUE;
                }

                TypeInitialization* tex = (TypeInitialization*) ex;
                
                const int err = validateTypeInitialization(lvalueTypeDef, tex);
                if (err < 0) return err;
                
                if (customDtype) *customDtype = lvalueTypeDef;
                return lvalueType; // (lvalueType == DT_ARRAY) ? DT_CUSTOM;
                
            }

        }

        op->cvalue.dtypeEnum = (DataTypeEnum) rdtype;
        return rdtype;

    }



    // returns DataTypeEnum of expression or error
    // assumings that each operand has defined dtype (evaluateDataTypes ran through branch)
    int evaluate(Variable* op, TypeDefinition** customDtype) {

        Expression* ex = op->expression;
        if (!ex) {
            
            const DataTypeEnum dtype = op->cvalue.dtypeEnum;
            if (dtype == DT_CUSTOM && customDtype) {
                *customDtype = op->def->var->cvalue.def;
            }

            if (op->def && !(op->cvalue.hasValue)) {
                op->cvalue = op->def->var->cvalue;
            }

            return dtype;
        
        }

        switch (ex->type) {
            
            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) ex;
                
                const int dtype = evaluate(uex->operand);
                if (dtype < Err::OK) return dtype;

                if (!(uex->operand->cvalue.hasValue)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED), uex->operand->span);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                }

                int x = Interpreter::applyOperator(uex->operType, &(uex->operand->cvalue));
                op->cvalue = uex->operand->cvalue;

                return op->cvalue.dtypeEnum;
                
            }

            case EXT_BINARY: {
                
                BinaryExpression* bex = (BinaryExpression*) ex;

                const int dtypeA = evaluate(bex->operandA);
                if (dtypeA < Err::OK) return dtypeA;

                if (!(bex->operandA->cvalue.hasValue)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED), bex->operandA->span);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                }

                const int dtypeB = evaluate(bex->operandB);
                if (dtypeB < Err::OK) return dtypeB;

                if (!(bex->operandB->cvalue.hasValue)) {
                    Logger::log(Logger::ERROR, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED), bex->operandB->span);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                }

                const int hasValueA = bex->operandA->cvalue.hasValue;
                const int hasValueB = bex->operandB->cvalue.hasValue;
                
                if (dtypeA < dtypeB) {
                    Variable* tmp = bex->operandA;
                    bex->operandA = bex->operandB;
                    bex->operandB = tmp;
                }

                
                Interpreter::applyOperator(bex->operType, &(bex->operandA->cvalue), &(bex->operandB->cvalue));
                op->cvalue = bex->operandA->cvalue;

                return op->cvalue.dtypeEnum;

            }

            case EXT_TERNARY: {

            }

            case EXT_WRAPPER: {
                
                WrapperExpression* wex = (WrapperExpression*) ex;
                
                const int dtype = evaluate(wex->operand);
                if (dtype < Err::OK) return dtype;

                const int opDtype = op->cvalue.dtypeEnum;
                if (opDtype == DT_ARRAY) {
                    return DT_ARRAY;
                }

                if (wex->operand->def) {
                    op->cvalue = wex->operand->def->var->cvalue;
                } else {
                    op->cvalue = wex->operand->cvalue;
                }

                return op->cvalue.dtypeEnum;

            }

            case EXT_FUNCTION_CALL: {

                FunctionCall* fex = (FunctionCall*) ex;

                for (int i = 0; i < fex->inArgs.size(); i++) {
                    const int err = evaluate(fex->inArgs[i]);
                    if (err < Err::OK) return err;
                    fex->fcn->inArgs[i]->var->cvalue.ptr = fex->inArgs[i]->cvalue.ptr;
                }

                const int err = Interpreter::execFunction(fex->fcn, op);
                if (err < 0) return err;

                op->cvalue.hasValue = 1;
                return err;

            }

            case EXT_ARRAY_INITIALIZATION: {
            
                ArrayInitialization* init = (ArrayInitialization*) ex;

                for (int i = 0; i < init->attributes.size(); i++) {
                    const int err = evaluate(init->attributes[i]);
                    if (err < Err::OK) return err;
                }

                return Err::OK;

            }

            case EXT_TYPE_INITIALIZATION: {

                TypeInitialization* tex = (TypeInitialization*) ex;
                
                for (int i = 0; i < tex->attributes.size(); i++) {
                    const int err = evaluate(tex->attributes[i]);
                    if (err < Err::OK) return err;
                }

                return Err::OK;

            }

        }

        // TODO
        return -1;

    }



    void copy(Variable* dest, Variable* src) {

        Span* span = dest->span;
        Expression* ex = dest->expression;
        // Variable* aloc = dest->allocSize;
        // std::vector<ScopeName*> sc = dest->path;
        memcpy(dest, src, sizeof(Variable));
        dest->span = span;
        dest->expression = ex;
        // dest->allocSize = aloc;
        // dest->path = sc;
    
    }

    // better name?
    // if idx is < 0, function will use the first attribute of
    // the first initialization (expected at first call)
    // function returns index of the next avaliable attribute
    // the input index is returned if there is no attribute avaliable
    int copyExpression(Variable* src, Variable** dest, int* pidx, int* nextIdx) {
        
        Expression* ex = src->expression;
        if (!ex) {

            *dest = new Variable();
            copy(*dest, src);
            (*dest)->name = src->name;
            (*dest)->nameLen = src->nameLen;
            
            (*dest)->cvalue = src->cvalue;

            return Err::OK;

        }

        switch (ex->type) {

            case EXT_UNARY : {
                
                UnaryExpression* uex = (UnaryExpression*) ex;
                UnaryExpression* duex = new UnaryExpression();

                *dest = new Variable();
                (*dest)->expression = duex;

                // duex->oper = uex->oper;
                duex->operType = uex->operType;
                
                return copyExpression((Variable*) uex->operand, (Variable**) &(duex->operand), pidx, nextIdx);
                
                break;

            }

            case EXT_BINARY: {

                BinaryExpression* bex = (BinaryExpression*) ex;
                BinaryExpression* dbex = new BinaryExpression();

                *dest = new Variable();
                (*dest)->expression = dbex;
                
                // dbex->oper = bex->oper;
                dbex->operType = bex->operType;

                copyExpression((Variable*) bex->operandA, (Variable**) &(dbex->operandA), pidx, nextIdx);
                copyExpression((Variable*) bex->operandB, (Variable**) &(dbex->operandB), pidx, nextIdx);

                break;
            
            }

            case EXT_TERNARY: {

                break;
            }

            case EXT_WRAPPER: {

                WrapperExpression* wex = (WrapperExpression*) ex;

                *dest = new Variable();
                (*dest)->expression = new WrapperExpression();
                
                return copyExpression((Variable*) wex->operand, (Variable**) &(((WrapperExpression*) (*dest)->expression)->operand), pidx, nextIdx);

                break;

            }

            case EXT_FUNCTION_CALL: {

                FunctionCall* fex = (FunctionCall*) ex;
                FunctionCall* dfex = new FunctionCall();

                dfex->fcn = fex->fcn;
                dfex->name = fex->name;
                dfex->nameLen = fex->nameLen;
                dfex->path = fex->path;
                dfex->id = fex->id;

                for (int i = 0; i < fex->inArgs.size(); i++) {
                    copyExpression(fex->inArgs[i], &(dfex->inArgs[i]), pidx, nextIdx);
                }

                *dest = new Variable();
                (*dest)->expression = dfex;

                return copyExpression(fex->outArg, &(dfex->outArg), pidx, nextIdx);

                break;

            }

            case EXT_TYPE_INITIALIZATION: {

                TypeInitialization* tex = (TypeInitialization*) ex;
                
                *dest = new Variable();

                int idx = *pidx;

                if (tex->attributes[idx]->cvalue.dtypeEnum == DT_CUSTOM) {
                    // TODO : have to think even if allow inner initialization
                    // but if so, seems like it has to be done in itterative way
                    // or rethink everything

                }
                
                if (idx < 0) {

                    idx = 0;
                
                } else {

                    if (tex->attributes[0]->name) {

                        const int size = tex->attributes.size();

                        int i = 0;
                        for (; i < size; i++) { 
                            if (idx == tex->idxs[i]) {
                                idx = i;
                                break;
                            }
                        }

                        if (i >= size) {
                            Logger::log(Logger::ERROR, "TODO : error type initialization, no such attribute");
                        }
                    
                    }

                }

                int err = copyExpression(tex->attributes[idx], dest, pidx, nextIdx); // TODO : pidx and nextIdx are placeholders
                if (err < 0) return err;

                if (*nextIdx >= 0) return Err::OK;

                if (idx + 1 >= tex->attributes.size()) {
                    *nextIdx = idx;
                    break;
                }

                if (tex->attributes[0]->name) {
                    *nextIdx = tex->idxs[idx + 1];
                    break;
                }

                *nextIdx = idx + 1;
                // copy(*dest, tex->attributes[idx]);
                // (*dest)->cvalue = src->cvalue;

                break;

            }


        }

        return Err::OK;

    }

}
