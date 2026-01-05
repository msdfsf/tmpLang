// #include "allocator.h"
#include "validator.h"
#include "array_list.h"
#include "data_types.h"
#include "globals.h"
#include "logger.h"
#include "error.h"
#include "operators.h"
#include "ordered_dict.h"
#include "string.h"
#include "strlib.h"
#include "syntax.h"
#include "interpreter.h"
#include "lexer.h"
#include <cstring>
#include <ios>

namespace Validator {

    Logger::Type logErr = { .level = Logger::ERROR, .tag = "validator" };
    Logger::Type logWrn = { .level = Logger::WARNING, .tag = "validator" };

    DArray::Container fCandidates; // f as function



    Err::Err validate() {

        Err::Err err;

        DArray::init(&fCandidates, 32, sizeof(FunctionPrototype));

        err = linkAll();
        if (err != Err::OK) return err;

        err = resolveTypes();
        if (err != Err::OK) return err;

        // TEST:
        Interpreter::init();
        Function* fcn = *(Function**) DArray::get(&SyntaxNode::root->fcns.base, 0);
        Interpreter::ExeBlock block;
        Interpreter::compile(fcn, &block);
        Interpreter::print(&block);

        err = validateFunctionCalls();
        if (err != Err::OK) return err;

        err = validateTypeDefinitions();
        if (err != Err::OK) return err;

        err = validateErrorSets();
        if (err != Err::OK) return err;

        err = verifyFunctionsAreGlobal();
        if (err != Err::OK) return err;

        // TODO: move to the end
        err = evaluateEnums();
        if (err != Err::OK) return err;

        err = evaluateCompileTimeVariables();
        if (err != Err::OK) return err;

        err = validateReturns();
        if (err != Err::OK) return err;

        err = validateAssignments();
        if (err != Err::OK) return err;

        err = validateLoops();
        if (err != Err::OK) return err;

        err = validateInitializations();
        if (err != Err::OK) return err;

        err = validateBranches();
        if (err != Err::OK) return err;

        err = validateStatements();
        if (err != Err::OK) return err;

        return Err::OK;

    }



    // ======================
    // LINK FUNCTIONS

    Err::Err linkAll() {

        Err::Err err;

        err = linkDataTypes();
        if (err != Err::OK) return err;

        err = linkErrorSets();
        if (err != Err::OK) return err;

        err = linkVariables();
        if (err != Err::OK) return err;

        err = linkGotos();
        if (err != Err::OK) return err;

        err = linkFunctionCalls();
        if (err != Err::OK) return err;

        return err;

    }

    Err::Err linkDataTypes() {

        for (int i = 0; i < (int) Reg.customDataTypesReferences.base.size; i++) {

            VariableDefinition* const varDef = *(VariableDefinition**) DArray::get(&Reg.customDataTypesReferences.base, i);

            void** dtype;
            int* dtypeEnum;

            if (varDef->lastPtr) {
                dtype = (void**) &(varDef->lastPtr->pointsTo);
                dtypeEnum = (int*) &(varDef->lastPtr->pointsToEnum);
            } else {
                dtype = (void**) &(varDef->var->cvalue.any);
                dtypeEnum = (int*) &(varDef->var->cvalue.dtypeEnum);
            }

            Scope* scope = varDef->base.scope;
            if (varDef->dtype && varDef->dtype->path->len > 0) {
                Namespace* nspace = NULL;
                ErrorSet* eset = NULL;
                const Err::Err err = validateQualifiedName(scope, varDef->dtype, &nspace, &eset);
                if (err != Err::OK) return err;
                if (eset) {
                    continue;
                }
                scope = (Scope*) nspace;
            }

            TypeDefinition* td = Reg.Find.inScopeTypeDefinition(scope, (String*) varDef->dtype);
            if (td) {
                *dtype = (void*) td;
                *dtypeEnum = DT_CUSTOM;
                continue;
            }

            Enumerator* en = (Enumerator*) Reg.Find.inScopeEnumerator(scope, (String*) varDef->dtype);
            if (en) {
                *dtype = (dataTypes + en->dtype);
                *dtypeEnum = en->dtype;
                continue;
            }

            Union* un = (Union*) Reg.Find.inScopeUnion(scope, (String*) varDef->dtype);
            if (un) {
                *dtype = (void*) un;
                *dtypeEnum = DT_UNION;
                continue;
            }

            ErrorSet* er = Reg.Find.inScopeErrorSet(scope, (String*) varDef->dtype);
            if (er) {
                *dtype = (void*) er;
                *dtypeEnum = DT_ERROR;
                continue;
            }

            Logger::log(logErr, ERR_STR(Err::UNKNOWN_DATA_TYPE), varDef->base.span, varDef->dtype->len);
            return Err::UNKNOWN_DATA_TYPE;

        }

        return Err::OK;

    }

    Err::Err linkErrorSets() {

        for (int i = 0; i < Reg.fcns.base.size; i++) {

            Function* const fcn = *(Function**) DArray::get(&Reg.fcns.base, i);
            QualifiedName* const errName = fcn->errorSetName;

            if (!errName) {
                fcn->errorSet = NULL;
                continue;
            }

            if (errName->path->len > 0) {

                Namespace* nspace = NULL;
                ErrorSet* eset = NULL;

                const Err::Err err = validateQualifiedName(fcn->base.scope, errName, &nspace, &eset);
                if (err != Err::OK) return err;

                if (!eset) {
                    Logger::log(logErr, ERR_STR(Err::UNKNOWN_ERROR_SET), fcn->base.span);
                    return Err::UNKNOWN_ERROR_SET;
                }

                if (nspace) {
                    eset = Reg.Find.inArray(&nspace->scope.customErrors, (String*) errName);
                    if (!eset) {
                        Logger::log(logErr, ERR_STR(Err::UNKNOWN_ERROR_SET), fcn->base.span);
                        return Err::UNKNOWN_ERROR_SET;
                    }
                } else if (eset) {
                    eset = Reg.Find.inScopeErrorSet(eset->base.scope, (String*) errName);
                    if (!eset) {
                        Logger::log(logErr, "Unknown or empty error set!", fcn->base.span);
                        return Err::UNKNOWN_ERROR_SET;
                    }
                }

                fcn->errorSet = eset;

            } else {

                ErrorSet* const eset = Reg.Find.inScopeErrorSet(fcn->base.scope, (String*) errName);
                if (!eset) {
                    Logger::log(logErr, ERR_STR(Err::UNKNOWN_ERROR_SET), fcn->base.span);
                    return Err::UNKNOWN_ERROR_SET;
                }

                fcn->errorSet = eset;

            }

        }

        return Err::OK;

    }

    Err::Err linkVariables() {

        // in case of scopeNames var is the last name (ex. point.x, var is then x)
        // scopeNames are sorted from left to right as written
        for (int i = 0; i < Reg.variables.base.size; i++) {

            Variable* const var = *(Variable**) DArray::get(&Reg.variables.base, i);
            Variable* tmpVar;

            // TODO: TMP
            var->base.parentIdx = var->base.scope->children.base.size;

            // const int scopeNamesLen = var->scopeNames.size();

            Scope* scope = var->base.scope;
            if (var->name.pathSize > 0) {
                Namespace* nspace = NULL;
                ErrorSet* eset = NULL;
                const Err::Err err = validateQualifiedName(scope, &var->name, &nspace, &eset);
                if (err != Err::OK) return err;
                if (eset) {
                    tmpVar = Reg.Find.inArray(&eset->vars, (String*) &var->name);
                    if (!tmpVar) {
                        Logger::log(logErr, ERR_STR(Err::UNKNOWN_ERROR_SET), var->base.span, var->name.len);
                        return Err::UNKNOWN_ERROR_SET;
                    }
                    Reg.Node.copyRef(var, tmpVar);
                    continue;
                }
                scope = (Scope*) nspace;

                // if namespace, we dont care about order

                tmpVar = Reg.Find.inScopeVariable(scope, (String*) &var->name);

                if (tmpVar) {
                    Reg.Node.copyRef(var, tmpVar);
                    continue;
                }

            } else {

                /*
                SyntaxNode* tmp = findDefinition(scope, var, var->parentIdx);
                if (tmp && tmp->type == NT_VARIABLE) {
                    Reg.Find.copy(var, (Variable*) tmp);
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
                tmpVar = findDefinition(scope, &var->name, var->base.parentIdx);
                if (tmpVar) {
                    Reg.Node.copyRef(var, tmpVar);
                    continue;
                }

            }

            /*
            Variable* tmpVar = Utils::find<Variable>(scope, var->name, var->nameLen, &Scope::vars);
            if (tmpVar) {
                Reg.Find.copy(var, tmpVar);
                continue;
            }
            */

            tmpVar = Reg.Find.inArray(Internal::variables, Internal::IV_COUNT, (String*) &var->name);
            if (tmpVar) {
                Reg.Node.copyRef(var, tmpVar);
                continue;
            }

            Enumerator* en = Reg.Find.inScopeEnumerator(scope, (String*) &var->name);
            if (en) {
                var->cvalue.dtypeEnum = DT_ENUM;
                var->cvalue.enm = en;
                continue;
            }

            // or function pointer
            Function* fcn = Reg.Find.inScopeFunction(scope, (String*) &var->name);
            if (fcn) {
                var->cvalue.dtypeEnum = DT_FUNCTION;
                var->cvalue.fcn = NULL;
                continue;
            }

            // Data type itself represents its size
            TypeDefinition* td = Reg.Find.inScopeTypeDefinition(scope, (String*) (&var->name));
            if (td) {
                // TODO : it seems better to compute it later and here just collect all candidates

                const int size = computeSizeOfDataType(td);
                if (size < 0) return (Err::Err) size;

                var->cvalue.i64 = size;
                var->cvalue.hasValue = 1;
                var->cvalue.dtypeEnum = DT_I64;

                continue;

            }

            Logger::log(logErr, ERR_STR(Err::UNKNOWN_VARIABLE), var->base.span, var->name.len, var->name.len, var->name.buff);
            return Err::UNKNOWN_VARIABLE;

        }

        return Err::OK;

    }

    // link goto statements
    Err::Err linkGotos() {

        for (int i = 0; i < Reg.gotos.base.size; i++) {

            GotoStatement* gt = (GotoStatement*) DArray::get(&Reg.gotos.base, i);

            Label* lb = Reg.Find.inScopeLabel(gt->base.scope, (String*) &gt->name);
            if (lb) {
                gt->label = lb;
                continue;
            }

            Logger::log(logErr, ERR_STR(Err::UNKNOWN_VARIABLE), gt->base.span, gt->name.len, gt->name.len, gt->name.buff);
            return Err::UNKNOWN_VARIABLE;

        }

    }

    Err::Err linkFunctionCalls() {

        for (int i = 0; i < (int) Reg.fcnCalls.base.size; i++) {

            Variable* fcnCallOp = *(Variable**) DArray::get(&Reg.fcnCalls.base, i);
            FunctionCall* fcnCall = (FunctionCall*) (fcnCallOp->expression);

            if (fcnCall->fcn) continue;

            Function* fcn;
            const int err = findClosestFunction(fcnCallOp, &fcn);
            if (err < 0) {
                // check for function pointer

                Variable* var = findDefinition(fcnCallOp->base.scope, &fcnCall->name, fcnCallOp->base.parentIdx);
                if (!var) {
                    Logger::log(logErr, ERR_STR(err), fcnCallOp->base.span, fcnCall->name.len);
                    return (Err::Err) err;
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
            fcnCall->outArg->cvalue.dtypeEnum = fcn->prototype.outArg->var->cvalue.dtypeEnum;

        }

        return Err::OK;

    }

    Err::Err verifyFunctionsAreGlobal() {

        for (int i = 0; i < Reg.fcns.base.size; i++) {

            Function* const fcn = *(Function**) DArray::get(&Reg.fcns.base, i);

            Scope* sc = fcn->base.scope;
            while (sc) {
                if (sc->base.type == NT_SCOPE) break;
                sc = sc->base.scope;
            }
            //Scope* const sc = fcn->scope->type == NT_SCOPE ? fcn->scope : fcn->scope->scope;
            if (sc != SyntaxNode::root) {
                Logger::log(logErr, ERR_STR(Err::GLOBAL_SCOPE_REQUIRED), fcn->base.span, fcn->name.len);
                return Err::GLOBAL_SCOPE_REQUIRED;
            }

        }

        return Err::OK;

    }







    // ======================
    // TYPE RESOLUTION STUFF

    Err::Err applyImplicitCast(Value* lval, Variable* rvar) {
        
        if (lval->dtypeEnum == rvar->cvalue.dtypeEnum) {
            return Err::OK;
        }

        if (rvar->cvalue.hasValue) {
            // TODO: suppose to cast literal in place.
            castLiteral(&rvar->cvalue, lval->dtypeEnum);
            return Err::OK;
        }

        // clone the current node to preserve also metadata
        // TODO: maybe no need to do a full copy
        Variable* innerOperand = Reg.Node.copy(rvar);

        Cast* castEx = Reg.Node.initCast();
        castEx->operand = innerOperand;
        castEx->target = lval->dtypeEnum;

        rvar->expression = (Expression*) castEx;
        rvar->cvalue = *lval;

        return Err::OK;

    }

    Err::Err resolveTypes() {

        Err::Err err;

        for (int i = 0; i < Reg.variableDefinitions.base.size; i++) {

            VariableDefinition* def = *(VariableDefinition**) DArray::get(&Reg.variableDefinitions.base, i);
            Value leftValue = def->var->cvalue;

            err = resolveTypes(def->var);
            if (err != Err::OK) return err;

            err = applyImplicitCast(&leftValue, def->var);
            if (err != Err::OK) return err;

            def->var->cvalue = leftValue;

        }

        for (int i = 0; i < Reg.fcnCalls.base.size; i++) {

            Variable* var = *(Variable**) DArray::get(&Reg.fcnCalls.base, i);
            FunctionCall* call = (FunctionCall*) var->expression;

            for (int i = 0; i < call->inArgs.base.size; i++) {
                
                Variable* arg = *(Variable**) DArray::get(&call->inArgs.base, i);
                Variable* def = *(Variable**) DArray::get(&call->fcn->prototype.inArgs.base, i);

                err = resolveTypes(arg);
                if (err != Err::OK) return err;

                err = applyImplicitCast(&def->cvalue, arg);
                if (err != Err::OK) return err;

            }

        }

        for (int i = 0; i < Reg.variableAssignments.base.size; i++) {

            VariableAssignment* ass = *(VariableAssignment**) DArray::get(&Reg.variableAssignments.base, i);
            
            err = resolveTypes(ass->lvar);
            if (err != Err::OK) return err;

            err = resolveTypes(ass->rvar);
            if (err != Err::OK) return err;

            err = applyImplicitCast(&ass->lvar->cvalue, ass->rvar);
            if (err != Err::OK) return err;

        }

        // TODO
        //
        return Err::OK;

    }

    Err::Err resolveResultType(UnaryExpression* uex, Variable* var) {
        
        if (uex->base.opType == OP_GET_ADDRESS) {
            var->cvalue.dtypeEnum = DT_POINTER;
        } else {
            var->cvalue.dtypeEnum = uex->operand->cvalue.dtypeEnum;
        }

        return Err::OK;
    
    }

    Err::Err resolveResultType(BinaryExpression* bex, Variable* var) {

        DataTypeEnum lDtype = bex->left->cvalue.dtypeEnum;
        DataTypeEnum rDtype = bex->right->cvalue.dtypeEnum;

        // TODO:
        // validateImplicitCast(lDtype, rDtype);

        if (dataTypes[lDtype].rank < dataTypes[rDtype].rank) {
            var->cvalue.dtypeEnum = lDtype;
        } else {
            var->cvalue.dtypeEnum = rDtype;
        }

        return Err::OK;

    }

    Err::Err resolveResultType(FunctionCall* fex, Variable* var) {
        var->cvalue.dtypeEnum = fex->outArg->cvalue.dtypeEnum;
        return Err::OK;
    }

    Err::Err resolveTypes(Variable* var) {

        Err::Err err;

        Expression* ex = var->expression;
        if (!ex) {
            return Err::OK;
        }

        switch (ex->type) {

            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) ex;

                err = resolveTypes(uex->operand);
                if (err != Err::OK) return err;

                err = resolveResultType(uex, var);
                if (err != Err::OK) return err;

                break;

            }

            case EXT_BINARY: {

                BinaryExpression* bex = (BinaryExpression*) ex;

                err = resolveTypes(bex->left);
                if (err != Err::OK) return err;

                err = resolveTypes(bex->right);
                if (err != Err::OK) return err;

                err = resolveResultType(bex, var);
                if (err != Err::OK) return err;

                break;

            }

            case EXT_FUNCTION_CALL: {

                FunctionCall* fex = (FunctionCall*) ex;

                for (int i = 0; i < fex->inArgs.base.size; i++) {
                    VariableDefinition* def = (VariableDefinition*) DArray::get(&fex->inArgs.base, i);
                    err = resolveTypes(def->var);
                }

                resolveResultType(fex, var);

                break;

            }

            case EXT_SLICE: {

                Slice* slice = (Slice*) ex;

                err = resolveTypes(slice->bidx);
                if (err != Err::OK) return err;
                if (!isInt(slice->bidx->cvalue.dtypeEnum)) {
                    Logger::log(logErr, "TODO");
                    return Err::INVALID_DATA_TYPE;
                }

                err = resolveTypes(slice->eidx);
                if (err != Err::OK) return err;
                if (!isInt(slice->eidx->cvalue.dtypeEnum)) {
                    Logger::log(logErr, "TODO");
                    return Err::INVALID_DATA_TYPE;
                }

                break;

            }

            default: {
                // TODO
            }
        }

        return Err::OK;

    }



    // ======================
    // VALIDATION FUNCTIONS

    // assuming size > 0
    Err::Err validateQualifiedName(Scope* sc, QualifiedName* name, Namespace** nspace, ErrorSet** eset) {

        Namespace* tmpNspace = NULL;
        INamed* names = name->path;
        // INamedLoc* names = (INamedLoc*) namesContainer->base.buffer;

        int i = 0;
        const int len = name->pathSize;

        for (; i < len; i++) {

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

    Err::Err validateFunctionCalls() {
        for (int i = 0; i < (int) Reg.fcnCalls.base.size; i++) {
            const int err = validateFunctionCall((Variable*) DArray::get(&Reg.fcnCalls.base, i));
            if (err < 0) return (Err::Err) err;
        }
    }

    Err::Err validateTypeDefinitions() {

        for (int i = 0; i < Reg.customDataTypes.base.size; i++) {

            TypeDefinition* const def = (TypeDefinition*) DArray::get(&Reg.customDataTypes.base, i);
            const int isUnion = def->base.type == NT_UNION;

            for (int i = 0; i < def->vars.base.size; i++) {

                Variable* const var = (Variable*) DArray::get(&def->vars.base, i);

                if (isUnion && var->expression || var->cvalue.hasValue) {
                    Logger::log(logErr, "Default values are not allowed within union initialization!", var->base.span, var->name.len);
                    return Err::INVALID_RVALUE;
                }

                if (var->cvalue.dtypeEnum != DT_CUSTOM) continue;

                // in case of custom data type check if its defined before
                if (var->cvalue.def->base.parentIdx >= def->base.parentIdx) {
                    TypeDefinition* const tmpDef = var->cvalue.def;
                    Logger::log(logErr, ERR_STR(Err::INVALID_DECLARATION_ORDER), var->def->base.span, var->def->dtype->len, def->name.len, def->name.buff);
                    return Err::INVALID_DECLARATION_ORDER;
                }
            }

        }

    }

    Err::Err validateArrays() {

        // for now messy
        for (int i = 0; i < (int) Reg.arrays.base.size; i++) {

            Variable* var = (Variable*) DArray::get(&Reg.arrays.base, i);
            Array* arr = var->cvalue.arr;

            // arr->flags : info about length
            // var should directly represent definition

            if (!(var->expression)) {

                const uint64_t flags = arr->flags;

                if (flags & IS_CONST || flags & IS_ARRAY_LIST || flags & IS_DYNAMIC) {
                    if (flags & IS_ARRAY_LIST) var->def->base.flags |= IS_ARRAY_LIST;
                    arr->flags |= IS_ALLOCATED;
                    continue;
                }

                if (!(arr->length) || !(arr->length->expression)) {
                    Logger::log(logErr, "Array declaration have to have length defineed either as compile-time expression or be a qualifier. Length cannot be empty without right side.", var->base.span, var->name.len);
                    return Err::INVALID_ARRAY_LENGTH;
                }

                Err::Err err;

                // err = evaluateDataTypes(arr->length);
                // if (err < Err::OK) return err;

                err = evaluate(arr->length);
                if (err < Err::OK) return err;

                continue;

            }

            if (var->expression->type == EXT_FUNCTION_CALL) {
                // alloc

                Variable* callInArgs = (Variable*) ((FunctionCall*) var->expression)->inArgs.base.buffer;
                Variable* alloc = callInArgs + 0;

                if (arr->flags & IS_CMP_TIME) {
                    Logger::log(logErr, "TODO error: invalid use of alloc!", var->base.span, var->name.len);
                    return Err::CANNOT_EVALUATE_EXPRESION_AT_CMP_TIME;
                }

                if (!(arr->flags & IS_CONST)) {
                    if (arr->length && arr->length->cvalue.hasValue) {
                        if (arr->length->cvalue.i64 != alloc->cvalue.arr->length->cvalue.i64) {
                            Logger::log(logErr, "TODO error: array and alloc size missmatch, you can omit array size while initializationg array with rvalue...");
                            return Err::INVALID_LVALUE;
                        }
                    }
                    arr->flags |= IS_ARRAY_LIST;
                    var->def->base.flags |= IS_ARRAY_LIST;
                }

                if (alloc->cvalue.dtypeEnum != DT_ARRAY) {
                    Logger::log(logErr, "TODO error: validating arrays!");
                    return Err::INVALID_DATA_TYPE;
                }

                // var->cvalue.arr->length = alloc->cvalue.arr->length;

                //var->cvalue.arr->length = len.cvalue.i64;
                arr->flags |= IS_ALLOCATED;

                // whatever

                Variable lenVar;
                const Err::Err err = evaluateArrayLength(alloc, &lenVar);
                if (err < 0) {
                    Logger::log(logErr, "!!! evaluateArrayLength failed with error: %i !!! ", var->base.span, 0, err);
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

            if (arr->length && (arr->length->cvalue.hasValue || arr->length->expression)) {

                // const int dt = evaluateDataTypes(arr->length);
                const DataTypeEnum dt = arr->length->cvalue.dtypeEnum;
                if (dt > 0 && dt <= DT_U64) {
                    if (!(arr->length->cvalue.hasValue)) {
                        const int err = evaluate(arr->length);
                        if (err < 0) {
                            Logger::log(logErr, "TODO error: array length has to be known at compile time!", var->base.span, 1);
                            return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                        }
                    }
                    continue;
                } else {
                    Logger::log(logErr, "TODO error: array length has to be int!", var->base.span, 1);
                    return Err::INVALID_DATA_TYPE;
                }
            }

            int len;
            Err::Err err;
            Variable lenVar;

            // stripWrapperExpressions(var);

            err = evaluateArrayLength(var, &lenVar);
            if (err < 0) {
                Logger::log(logErr, "!!! evaluateArrayLength failed with error: %i !!! ", var->base.span, 0, err);
                return err;
            }

            //translatorC.printVariable(stdout, 0, &lenVar, NULL);
            //printf("\n");

            len = lenVar.cvalue.i64;

            if (arr->length->cvalue.hasValue && arr->length->cvalue.i64 != len) {
                Logger::log(logErr, "TODO error: array and alloc size mismatch, you can omit array size while initializing array with rvalue...");
                return Err::INVALID_LVALUE;
            }

            if (len == 0) {
                Logger::log(logErr, "Array length cannot be zero!", var->base.span, var->name.len);
                return Err::INVALID_ARRAY_LENGTH;
            }

            var->cvalue.arr->length->cvalue.hasValue = 1;
            var->cvalue.arr->length->cvalue.i64 = len;
            var->cvalue.arr->length->cvalue.dtypeEnum = DT_I64;
            //var->cvalue.arr->pointsTo = NULL;
            var->cvalue.hasValue = 1;

        }

        return Err::OK;

    }

    Err::Err validateErrorSets() {

        int varId = Reg.variables.base.size;
        int errId = Reg.customErrors.base.size + 1;

        for (int i = 0; i < Reg.customErrors.base.size; i++) {
            // hasValue is used to mark if error set have its definition or not,
            // if not, then hasValue is true, as there is nothing to expand to

            ErrorSet* const eset = (ErrorSet*) DArray::get(&Reg.customErrors.base, i);
            for (int i = 0; i < eset->vars.base.size; i++) {

                Variable* const var = (Variable*) DArray::get(&eset->vars.base, i);

                var->cvalue.err =Reg.Find.inScopeErrorSet(eset->base.scope, (String*) &var->name);

                if (!(var->cvalue.err)) {
                    var->cvalue.hasValue = 1;
                    var->cvalue.dtypeEnum = DT_ERROR;
                    var->cvalue.u64 = errId;
                    var->name.id = varId;
                    varId++;
                    errId++;
                }

            }
        }

        return Err::OK;

    }

    Err::Err validateReturns() {

        for (int i = 0; i < Reg.returnStatements.base.size; i++) {

            ReturnStatement* rt = (ReturnStatement*) DArray::get(&Reg.returnStatements.base, i);
            Function* fc = rt->fcn;
            Value* outValue = &(fc->prototype.outArg->var->cvalue);

            if (!fc) {
                Logger::log(logErr, "Unexpected return statement outside of the function!", rt->base.span);
                return Err::UNEXPECTED_SYMBOL;
            }

            if (outValue->dtypeEnum != DT_VOID && !rt->var && !rt->err) {
                Logger::log(logErr, "Not enough return arguments!", rt->base.span); // TODO : better error message
                return Err::NOT_ENOUGH_ARGUMENTS;
            }

            if (outValue->dtypeEnum == DT_VOID && rt->var) {
                Logger::log(logErr, "Too many return arguments!", rt->base.span); // TODO : better error message
                return Err::TOO_MANY_ARGUMENTS;
            }

            if (rt->var) {

                // const Err::Err rtType = evaluateDataTypes(rt->var);
                // if (rtType < 0) return rtType;
                const DataTypeEnum rtType = rt->var->cvalue.dtypeEnum;

                // TODO : for now meh, later create validateCast that takes values
                Err::Err err;
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
                    int len = outValue->dtypeEnum == DT_CUSTOM ? fc->prototype.outArg->dtype->len : 0;// TODO: dataTypes[outValue->dtypeEnum];
                    Logger::log({ Logger::PLAIN }, "", rt->base.span);
                    Logger::log(logErr, "In following declaration:", fc->prototype.outArg->base.span, len);
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
                    Logger::log(logErr, ERR_STR(Err::INVALID_TYPE_CONVERSION), rt->span, 1, (dataTypes + rtType)->name, (dataTypes + fc->outArg)->name);
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
                    Logger::log(logErr, "Expected error set, pure namespace given!", rt->span);
                    return Err::UNKNOWN_ERROR_SET;
                }

            }
            */

        }

    }

    Err::Err validateAssignments() {

        // so, if lvalue is
        //  1) just variable, then, after evaluation, variable is the answer,
        //  2) expression, then, after evaluation, the first expression has to be unary and operator be 'get value' or binary and operator be 'subscript'
        for (int i = 0; i < Reg.variableAssignments.base.size; i++) {

            VariableAssignment* const varAss = (VariableAssignment*) DArray::get(&Reg.variableAssignments.base, i);

            // TypeDefinition* dtypeLDef = NULL;
            // DataTypeEnum dtypeL = (DataTypeEnum) evaluateDataTypes(varAss->lvar, &dtypeLDef);

            TypeDefinition* dtypeLDef = varAss->lvar->cvalue.def;
            DataTypeEnum dtypeL = varAss->lvar->cvalue.dtypeEnum;

            if (dtypeL < Err::OK) return (Err::Err) dtypeL;

            Expression* lvalueEx = (varAss->lvar)->expression;

            // TODO : exclude binary expression check?
            //
            //
            if (!(
                    !(lvalueEx) ||
                    (lvalueEx->type == EXT_UNARY && ((UnaryExpression*) lvalueEx)->base.opType == OP_GET_VALUE) ||
                    (lvalueEx->type == EXT_BINARY && (((UnaryExpression*) lvalueEx)->base.opType == OP_SUBSCRIPT || isMemberSelection(((UnaryExpression*) lvalueEx)->base.opType) )) ||
                    (lvalueEx->type == EXT_SLICE)
                )
            ) {
                Logger::log(logErr, ERR_STR(Err::INVALID_LVALUE), varAss->base.span, varAss->base.span->end.idx);// - varAss->lvar->span->idx);
                return Err::INVALID_LVALUE;
            }

            if (varAss->lvar->def && varAss->lvar->def->base.flags & IS_CONST) {
                Logger::log(logErr, ERR_STR(Err::CANNOT_ASSIGN_TO_CONST), varAss->base.span, 1);
                return Err::CANNOT_ASSIGN_TO_CONST;
            }

            if (varAss->rvar->base.flags & IS_ALLOCATION) {
                if (dtypeL == DT_ARRAY) {
                    Array* arr = (Array*) dtypeLDef;
                    if (!(arr->flags & IS_ARRAY_LIST || arr->flags & IS_DYNAMIC)) {
                        Logger::log(logErr, "Cannot realocate array of const length!", varAss->rvar->base.span, 0);
                        return Err::CANNOT_ASSIGN_TO_CONST;
                    }
                }
            }

            // DataTypeEnum dtypeR = (DataTypeEnum) evaluateDataTypes(varAss->rvar, NULL, dtypeL, dtypeLDef);
            DataTypeEnum dtypeR = varAss->rvar->cvalue.dtypeEnum;
            if (dtypeR < 0) return (Err::Err) dtypeR;

            if (dtypeL == DT_CUSTOM) {

                if (dtypeR != DT_CUSTOM) {
                    Logger::log(logErr, "Invalid type conversion, rvalue should be proper type initialization!", varAss->base.span, 0);
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
                Logger::log(logErr, "WEEE");
                continue;
            }

            if (lvalueEx && lvalueEx->type == EXT_SLICE && dtypeR != DT_ARRAY) {
                dtypeL = ((Slice*) lvalueEx)->arr->cvalue.arr->base.pointsToEnum;
            }

            const Err::Err err = validateImplicitCast(varAss->rvar->cvalue.any, varAss->lvar->cvalue.any, dtypeR, dtypeL);
            if (err < 0) {
                if (dtypeL == DT_POINTER) {
                    const Err::Err err = validatePointerAssignment(&(varAss->rvar->cvalue));
                    if (err < 0) return err;
                }
                return err;
            }

            if (dtypeR == DT_ARRAY) {

                Variable lenVar;
                const Err::Err err = evaluateArrayLength(varAss->rvar, &lenVar);
                if (err < 0) return err;

                if (!lvalueEx) continue;

                if (lvalueEx->type == EXT_SLICE) {

                    Slice* sex = (Slice*)(varAss->lvar->expression);
                    if (sex->eidx->cvalue.dtypeEnum == DT_UNDEFINED) {
                        Reg.Node.copy(sex->eidx, &lenVar);
                        sex->eidx->cvalue.dtypeEnum = DT_UNDEFINED;
                    }

                } else if (lvalueEx->type == EXT_BINARY) {

                    BinaryExpression* bex = (BinaryExpression*) lvalueEx;
                    if (bex->base.opType == OP_SUBSCRIPT) {
                        if (!(bex->left->cvalue.arr->length)) {
                            bex->left->cvalue.arr->length = new Variable();
                            Reg.Node.copy(bex->left->cvalue.arr->length, &lenVar);
                            bex->left->cvalue.arr->length->cvalue.dtypeEnum = DT_UNDEFINED;
                        } else if (bex->left->cvalue.arr->length->cvalue.dtypeEnum == DT_UNDEFINED) {
                            Reg.Node.copy(bex->left->cvalue.arr->length, &lenVar);
                            bex->left->cvalue.arr->length->cvalue.dtypeEnum = DT_UNDEFINED;
                        }
                    }

                }
            }

        }

    }

    Err::Err validateLoops() {

        for (int i = 0; i < Reg.loops.base.size; i++) {

            Loop* loop = (Loop*) DArray::get(&Reg.loops.base, i);

            DataTypeEnum dtype = loop->array->cvalue.dtypeEnum; // (DataTypeEnum) evaluateDataTypes(loop->array);
            if (dtype != DT_ARRAY) {
                Logger::log(logErr, "Only array is allowed!", loop->base.span, strlen(Lex::KWS_LOOP));
                return Err::INVALID_DATA_TYPE;
            }

            Variable lenVar;
            const Err::Err err = evaluateArrayLength(loop->array, &lenVar);
            if (err < 0) return err;

            //*(loop->array->cvalue.arr->length) = lenVar;
            /*
            DataTypeEnum dtype = (DataTypeEnum) evaluate(loop->array);
            if (dtype < Err::OK) {
                Logger::log(logErr, "ASD", loop->span, strlen(KWS_LOOP));
                return dtype;
            }
            */

        }

    }

    Err::Err validateInitializations() {

        for (int i = 0; i < Reg.initializations.base.size; i++) {

            VariableDefinition* const varDef = (VariableDefinition*) DArray::get(&Reg.initializations.base, i);
            Variable* var = varDef->var;

            const Value value = var->cvalue;
            DataTypeEnum dtypeEnumRef = var->cvalue.dtypeEnum;
            void* dtypeRef = var->cvalue.any;

            void* dtype;
            // int dtypeEnum = evaluateDataTypes(var, (TypeDefinition**) (&dtype), dtypeEnumRef, (TypeDefinition*) (dtypeRef));
            DataTypeEnum dtypeEnum = var->cvalue.dtypeEnum;
            if (dtypeEnum < 0) return (Err::Err) dtypeEnum;
            var->cvalue = value;

            // validateCustomTypeInitialization(dtype, dtypeInit);
            Logger::mute = 1;
            const Err::Err err = validateImplicitCast(dtype, dtypeRef, (DataTypeEnum) dtypeEnum, dtypeEnumRef);
            Logger::mute = 0;
            if (err < 0) {
                if (dtypeEnumRef == DT_POINTER) {
                    Value* val = &((var->def) ? var->def->var->cvalue : var->cvalue);
                    const Err::Err err = validatePointerAssignment(val);
                    if (err < 0) return err;
                } else {
                    Logger::log(logErr, "TODO: Invalid type conversion!", var->base.span);
                    return err;
                }
            }

            if (dtypeEnumRef == DT_FUNCTION) {

                Variable* tmp;

                Function* fcn = findExactFunction(varDef->base.scope, (String*) &tmp->name, value.fcn);
                if (!fcn) {
                    Logger::log(logErr, "Function doesnt match pointer definition!", var->base.span, var->name.len);
                    return Err::NO_MATCHING_FUNCTION_FOUND;
                }

                tmp->cvalue.fcn = &fcn->prototype;
                tmp->name.id = fcn->name.id;

            }

        }

    }

    Err::Err validateBranches() {

        for (int i = 0; i < Reg.branchExpressions.base.size; i++) {

            Variable* op = (Variable*) DArray::get(&Reg.branchExpressions.base, i);
            // int dtype = evaluateDataTypes(op);
            DataTypeEnum dtype = op->cvalue.dtypeEnum;
            if (isInt(dtype) || dtype == DT_POINTER) continue;

            Logger::log(logErr, ERR_STR(Err::INVALID_DATA_TYPE), op->base.span, 1);
            return Err::INVALID_DATA_TYPE;

        }

        return Err::OK;

    }

    Err::Err validateStatements() {

        for (int i = 0; i < Reg.statements.base.size; i++) {

            Statement* st = (Statement*) DArray::get(&Reg.statements.base, i);
            Variable* op = st->operand;
            // int dtype = evaluateDataTypes(op);
            DataTypeEnum dtype = op->cvalue.dtypeEnum;
            if (dtype < 0) return (Err::Err) dtype;

        }

        return Err::OK;

    }

    Err::Err validateSwitchCases() {

        Err::Err err;

        for (int i = 0; i < Reg.switchCases.base.size; i++) {

            SwitchCase* const sw = (SwitchCase*) DArray::get(&Reg.switchCases.base, i);

            // err = evaluateDataTypes(sw->switchExp);
            // if (err != Err::OK) return err;

            DataTypeEnum dtype = sw->switchExp->cvalue.dtypeEnum;
            if (dtype < 0) {
                Logger::log(logErr, "TODO error", sw->base.span, 1);
                return (Err::Err) dtype;
            }

            for (int i = 0; i < sw->casesExp.base.size; i++) {

                Variable* caseExp = (Variable*) DArray::get(&sw->casesExp.base, i);
                // int dtype = evaluateDataTypes(caseExp);
                dtype = caseExp->cvalue.dtypeEnum;
                if (dtype < 0) {
                    Logger::log(logErr, "", sw->base.span, 1);
                    return (Err::Err) dtype;
                }

                dtype = caseExp->cvalue.dtypeEnum;// evaluate(caseExp);
                if (dtype < 0) return (Err::Err) dtype;

            }

        }

    }

    // used within validateTypeInitialization
    // think about better name
    Err::Err validateAttributeCast(Variable* var, Variable* attribute) {

        // Variable* op, TypeDefinition** customDtype, DataTypeEnum lvalueType, TypeDefinition* lvalueTypeDef
        const int id = attribute->name.id;
        char* const name = attribute->name.buff;
        const int nameLen = attribute->name.len;

        VariableDefinition* def = var->def;
        DataTypeEnum dtypeA = attribute->cvalue.dtypeEnum;
        // DataTypeEnum dtypeA = (DataTypeEnum) evaluateDataTypes(
        //    attribute,
        //    NULL,
        //    def ? def->var->cvalue.dtypeEnum : DT_UNDEFINED,
        //    def ? def->var->cvalue.def : NULL
        //);

        attribute->name.id = id;
        attribute->name.buff = name;
        attribute->name.len = nameLen;

        DataTypeEnum dtypeB = var->cvalue.dtypeEnum;
        // DataTypeEnum dtypeB = (DataTypeEnum) evaluateDataTypes(var);

        if (!validateImplicitCast(dtypeB, dtypeA)) {
            return Err::INVALID_DATA_TYPE;
        }

        return Err::OK;

    }

    // assuming dtypeInit has at least one attribute
    // both TypeDefinition  has to be valid
    Err::Err validateTypeInitialization(TypeDefinition* dtype, TypeInitialization* dtypeInit) {

        Variable* attributes = (Variable*) dtypeInit->attributes.base.buffer;

        const int count = dtype->vars.base.size;
        const int areNamed = dtypeInit->attributes.base.size == 0 || (attributes + 0)->name.buff;

        if (count < dtypeInit->attributes.base.size) {
            Logger::log(logErr, ERR_STR(Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH), dtype->base.span, dtype->name.len, dtypeInit->attributes.base.size, dtype->vars.base.size);
            return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
        }

        if (dtype->base.type == NT_UNION) {

            if (dtypeInit->attributes.base.size > 1) {
                Variable* var = (attributes + 1);
                Logger::log(logErr, "Only one attribute can be set while initializing union!", var->base.span, var->name.len);
                return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
            }

            if (dtypeInit->fillVar) {
                Variable* var = dtypeInit->fillVar;
                Logger::log(logErr, "Fill th rest option is not allowed while initializing union!", var->base.span, 2);
                return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
            }

            if (!areNamed) {
                Variable* var = (attributes + 0);
                Logger::log(logErr, "Only initialization through specifying name of attribute is allowed while initializing union!", var->base.span, 1);
                return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
            }

        }

        if (areNamed) {
            // treating all as named

            dtypeInit->idxs = (int*) malloc(sizeof(int) * count);
            for (int i = 0; i < count; i++) {
                dtypeInit->idxs[i] = -1;
            }

            for (int i = 0; i < dtypeInit->attributes.base.size; i++) {

                Variable* attribute = attributes + i;

                const int idx = Reg.Find.inArray(&dtype->vars, (String*) &attribute->name, NULL);
                if (idx < 0) {
                    Logger::log(logErr, ERR_STR(Err::INVALID_ATTRIBUTE_NAME), attribute->base.span, attribute->name.len, attribute->name.len, attribute->name.buff);
                    return Err::INVALID_ATTRIBUTE_NAME;
                }

                Variable* var = ((Variable*) DArray::get(&dtype->vars.base, idx));

                dtypeInit->idxs[idx] = i;
                attribute->name.id = var->name.id;

                // typecheck
                // if (!attribute->expression) continue;

                const Err::Err err = validateAttributeCast(var, attribute);
                if (err < 0) return err;

            }

            if (dtypeInit->fillVar) {

                for (int i = 0; i < dtype->vars.base.size; i++) {

                    Variable* attribute = (Variable*) DArray::get(&dtype->vars.base, i);
                    if (dtypeInit->idxs[i] < 0) continue;

                    // typecheck
                    // if (!attribute->expression) continue;

                    const Err::Err err = validateAttributeCast(attribute, dtypeInit->fillVar);
                    if (err < 0) return err;

                }

            }

            return Err::OK;

        }

        for (int i = 0; i < dtypeInit->attributes.base.size; i++) {

            Variable* attribute = (Variable*) DArray::get(&dtypeInit->attributes.base, i);
            Variable* var = (Variable*) DArray::get(&dtype->vars.base, i);

            dtypeInit->idxs[i] = i;

            // typecheck
            if (!attribute->expression) continue;

            const Err::Err err = validateAttributeCast(var, attribute);
            if (err < 0) return err;

        }

        return Err::OK;

    }

    Err::Err validateTypeInitializations(TypeDefinition* dtype, Variable* var) {

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
                validateTypeInitializations(dtype, (Variable*) ((BinaryExpression*) ex)->left);
                validateTypeInitializations(dtype, (Variable*) ((BinaryExpression*) ex)->right);
                break;
            }

            case EXT_TERNARY : {
                break;
            }

            case EXT_FUNCTION_CALL : {
                break;
            }

            case EXT_TYPE_INITIALIZATION : {

                TypeInitialization* tinit = (TypeInitialization*) ex;

                Err::Err err = validateTypeInitialization(dtype, tinit);
                if (err < 0) return err;

            }

            default: {
                // TODO
            }

        }

        return Err::OK;

    }

    Err::Err validateFunctionCall(Variable* fcnCallOp) {

        // Variable* fcnCallOp = SyntaxNode::fcnCalls[i];
        FunctionCall* fcnCall = (FunctionCall*) (fcnCallOp->expression);
        // Function* fcn = fcnCall->fcn;
        FunctionPrototype* fcn = fcnCall->fcn ? &fcnCall->fcn->prototype : fcnCall->fptr->cvalue.fcn;
        const int fcnInCount = fcn->inArgs.base.size;
        // const int fcnCallInCount = fcnCall->inArgs.size();

        VariableDefinition* fcnInArgs = (VariableDefinition*) fcn->inArgs.base.buffer;
        const int variableNumberOfArguments = fcnInCount > 0 && (fcnInArgs + (fcnInCount - 1))->var->cvalue.dtypeEnum == DT_MULTIPLE_TYPES;

        // note:
        //  array argument is parsed as two arguments (pointer and length) in definition

        int j = 0;
        for (int i = 0; i < fcnInCount - variableNumberOfArguments; i++) {

            if (j >= fcnCall->inArgs.base.size) {
                Logger::log(logErr, ERR_STR(Err::NOT_ENOUGH_ARGUMENTS), fcnCallOp->base.span, fcnCall->name.len);
                return Err::NOT_ENOUGH_ARGUMENTS;
            }

            VariableDefinition* fcnVarDef = fcnInArgs + i;
            Variable* fcnVar = fcnVarDef->var;
            Variable* fcnCallVar = (Variable*) DArray::get(&fcnCall->inArgs.base, j);

            int fcnCallVarDtype;
            if (fcnCallVar->expression) {
                // TODOD : TypeDefinition** customDtype, DataTypeEnum lvalueType, TypeDefinition* lvalueTypeDef
                // fcnCallVarDtype = evaluateDataTypes(fcnCallVar);
                fcnCallVarDtype = fcnCallVar->cvalue.dtypeEnum;
                if (fcnCallVarDtype < Err::OK) return (Err::Err) fcnCallVarDtype;
            } else {
                fcnCallVarDtype = fcnCallVar->cvalue.dtypeEnum;
            }

            if (fcnVar->cvalue.dtypeEnum == DT_ARRAY) {

                if (!(fcnCallVar->cvalue.dtypeEnum == DT_ARRAY)) {
                    Logger::log(logErr, ERR_STR(Err::ARRAY_EXPECTED), fcnCallVar->base.span, 1);
                    return Err::ARRAY_EXPECTED;
                }

                Variable* var = fcnCallVar;
                if (!(var->def) || !(var->def->var->cvalue.arr)) {
                    Logger::log(logErr, "TODO error: array expected!", fcnCallVar->base.span, fcnCallVar->name.len);
                    return Err::ARRAY_EXPECTED;
                }

                const Err::Err err = evaluateArrayLength(var);
                if (err < 0) return err;

                DArray::set(&fcnCall->inArgs.base, j, var);

                /*
                Variable* lenVar = new Variable();
                const int err = evaluateArrayLength(var, lenVar);
                if (err < 0) return err;

                Value* val = new Value();
                val->arr

                var->cvalue.arr->length = lenVar;
                var->cvalue.arr->length->cvalue.dtypeEnum = DT_UINT_64;
                var->cvalue.arr->length->flags |= IS_LENGTH;
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
                // TODO: Logger::log(logErr, ERR_STR(Err::INVALID_TYPE_CONVERSION), fcnCallVar->base.span, 1, (dataTypes + fcnVar->cvalue.dtypeEnum)->name, (dataTypes + fcnCallVarDtype)->name);
                return Err::INVALID_TYPE_CONVERSION;
            }

            j++;

        }

        const int fcnCallInCount = fcnCall->inArgs.base.size;

        if (j < fcnCallInCount && !variableNumberOfArguments) {
            Logger::log(logErr, ERR_STR(Err::TOO_MANY_ARGUMENTS), fcnCallOp->base.span, fcnCall->name.len);
            return Err::TOO_MANY_ARGUMENTS;
        }

        for (; j < fcnCallInCount; j++) {

            Variable* var = (Variable*) DArray::get(&fcnCall->inArgs.base, j);

            int err;
            if (var->def) {
                // TODO : ALLOC case for now

                Variable* tmp = var->def->var;
                const Value tmpValue = tmp->cvalue;
                const DataTypeEnum tmpDtype = tmp->cvalue.dtypeEnum;

                err = var->cvalue.dtypeEnum;// evaluateDataTypes(var, NULL, tmp->cvalue.dtypeEnum, tmp->cvalue.def);

                if (tmp->cvalue.dtypeEnum == DT_CUSTOM) {
                    // seems like evaluateDataTypes does the same thing
                    // validateTypeInitialization(tmp->cvalue.def, (TypeInitialization*) ((WrapperExpression*) (var->expression))->operand->expression);
                } else {
                    const Err::Err err = validateImplicitCast(var->cvalue.any, tmpValue.any, var->cvalue.dtypeEnum, tmpValue.dtypeEnum);
                    if (err < 0) return err;
                    // tmp->cvalue.dtypeEnum = tmpDtype;
                }

                tmp->cvalue = tmpValue;

            } else {
                err = var->cvalue.dtypeEnum; // evaluateDataTypes(var);
                if (err < 0) {
                    Logger::log(logErr, "caused by %i argument", var->base.span, 1, j);
                }
            }

            if (err < 0) {
                return (Err::Err) err;
            }

        }

        return Err::OK;

        // fcnCall->fcn = fcn;

    }

    inline Err::Err validatePointerAssignment(const Value* const val) {
        if (val->hasValue && val->u64 == 0) return Err::OK;
        Logger::log(logErr, "Only 0 could be assigned to a pointer variable!");
        return Err::INVALID_RVALUE;
    }

    Err::Err validateImplicitCast(const DataTypeEnum dtype, const DataTypeEnum dtypeRef) {

        const int basicTypes = (dtype != DT_VOID && dtypeRef != DT_VOID) && (dtype < DT_STRING && dtypeRef < DT_STRING);
        const int arrayToPointer = (dtype == DT_ARRAY && dtypeRef == DT_POINTER);
        const int pointerToArray = (dtypeRef == DT_ARRAY && dtype == DT_POINTER);
        const int sliceToArray = dtype == DT_SLICE && dtypeRef == DT_ARRAY;

        return (Err::Err) ((dtype == dtypeRef) || (basicTypes || arrayToPointer || pointerToArray || sliceToArray));

    }

    Err::Err validateImplicitCast(void* dtype, void* dtypeRef, DataTypeEnum dtypeEnum, DataTypeEnum dtypeEnumRef) {

        if (validateImplicitCast(dtypeEnum, dtypeEnumRef)) {
            return Err::OK;
        }

        if (dtypeEnumRef == DT_ARRAY && dtypeEnum == DT_STRING) {

            Array* arr = (Array*) dtypeRef;
            StringInitialization* str = (StringInitialization*) dtype;

            const int arrDtypeSize = dataTypes[arr->base.pointsToEnum].size;
            const int strDtypeSize = dataTypes[str->wideDtype].size;

            if (arrDtypeSize < strDtypeSize) {
                Logger::log(logErr, "TODO error: ");
                return Err::INVALID_TYPE_CONVERSION;
            }

            if (!validateImplicitCast(arr->base.pointsToEnum, str->wideDtype)) {
                Logger::log(logErr, "TODO error: ");
                return Err::INVALID_TYPE_CONVERSION;
            }

            if (strDtypeSize < arrDtypeSize) {
                Logger::log(logWrn, "TODO warning: One can use smaller dtype");
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

        Logger::log(logErr, "TODO : invalid type conversion!");
        return Err::INVALID_TYPE_CONVERSION;

    }

    #define GENERATE_TARGET_SWITCH(dest, src) \
    switch (dest) { \
        case DT_I8:  val->i8  = (int8_t)  (src); break; \
        case DT_U8:  val->u8  = (uint8_t) (src); break; \
        case DT_I16: val->i16 = (int16_t) (src); break; \
        case DT_U16: val->u16 = (uint16_t)(src); break; \
        case DT_I32: val->i32 = (int32_t) (src); break; \
        case DT_U32: val->u32 = (uint32_t)(src); break; \
        case DT_I64: val->i64 = (int64_t) (src); break; \
        case DT_U64: val->u64 = (uint64_t)(src); break; \
        case DT_F32: val->f32 = (float)   (src); break; \
        case DT_F64: val->f64 = (double)  (src); break; \
        default: { \
            Logger::log(logErr, ERR_STR(Err::UNEXPECTED_ERROR)); \
            exit(213); \
        } \
    }

    void castLiteral(Value* val, DataTypeEnum toDtype) {

        if (val->dtypeEnum == toDtype) return;

        switch (val->dtypeEnum) {

            case DT_U8: {
                uint8_t src = val->u8;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case DT_U16: {
                uint16_t src = val->u16;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case DT_U32: {
                uint32_t src = val->u32;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case DT_U64: {
                uint64_t src = val->u64;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case DT_I8: {
                int8_t src = val->i8;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case DT_I16: {
                int16_t src = val->i16;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case DT_I32: {
                int32_t src = val->i32;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case DT_I64: {
                int64_t src = val->i64;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case DT_F32: {
                float src = val->f32;
                GENERATE_TARGET_SWITCH(toDtype, src)
                break;
            }

            case DT_F64: {
                double src = val->f64;
                GENERATE_TARGET_SWITCH(toDtype, src)
                break;
            }

            default: {
                // TODO
                Logger::log(logErr, ERR_STR(Err::UNEXPECTED_ERROR));
                exit(123);
                break;
            }

        }

        val->dtypeEnum = toDtype;

    }

    bool isBasicDtype(DataTypeEnum dtype) {
        return (dtype > DT_VOID && dtype <= DT_F64);
    }



    // ======================
    // EVALUATION FUNCTIONS

    Err::Err evaluateEnums() {

        for (int i = 0; i < Reg.enumerators.base.size; i++) {

            Enumerator* en = (Enumerator*) DArray::get(&Reg.enumerators.base, i);

            Err::Err err;
            int64_t lastValue = -1;
            for (int i = 0; i < en->vars.base.size; i++) {

                Variable* var = (Variable*) DArray::get(&en->vars.base, i);

                if (var->expression) {

                    // err = //evaluateDataTypes(var);
                    // if (err < Err::OK) return err;

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

        return Err::OK;

    }

    Err::Err evaluateCompileTimeVariables() {

        for (int i = 0; i < (int) Reg.cmpTimeVars.base.size; i++) {

            Err::Err err;
            Variable* var = *(Variable**) DArray::get(&Reg.cmpTimeVars.base, i);

            if (!(var->expression)) continue;

            if (var->cvalue.dtypeEnum != DT_ARRAY) {
                // err = evaluateDataTypes(var);
                // if (err < Err::OK) return err;
            }

            err = evaluate(var);
            if (err < Err::OK) return err;

        }

    }

    // just wrapper function for now
    Err::Err evaluateArrayLength(Variable* var, uint64_t flag) {

        Variable* len = (Variable*) nalloc(nalc, NT_VARIABLE);
        Array* arr = (Array*) nalloc(nalc, AT_ARRAY);
        const Err::Err err = evaluateArrayLength(var, len, flag);
        if (err < 0) {
            ndealloc(nalc, len);
            ndealloc(nalc, arr);
            return err;
        }

        *arr = *(var->cvalue.arr);
        arr->length = len;
        //len->flags = IS_LENGTH;
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
    Err::Err evaluateArrayLength(Variable* var, Variable* len, uint64_t flag) {

        Err::Err err;

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
                len->base.flags |= flag;

            } else if (len->expression->type == EXT_BINARY) {

                BinaryExpression* obex = (BinaryExpression*) len->expression;
                BinaryExpression* nbex = new BinaryExpression();

                nbex->left = var;
                nbex->right = new Variable();
                nbex->right->expression = (Expression*) obex;

                len->expression = (Expression*) nbex;
                len->unrollExpression = 0;

            } else {

            }

            return Err::OK;

        }

        switch (ex->type) {

            case EXT_UNARY : {

                UnaryExpression* uex = (UnaryExpression*) ex;

                err = evaluateArrayLength(uex->operand, len, flag);
                if (err < 0) return err;

                return Err::OK;

            }

            case EXT_BINARY : {

                BinaryExpression* bex = (BinaryExpression*) ex;

                Variable lenA;
                err = evaluateArrayLength(bex->left, &lenA, flag);
                if (err < 0) return err;

                Variable lenB;
                err = evaluateArrayLength(bex->right, &lenB, flag);
                if (err < 0) return err;

                if (bex->base.opType == OP_CONCATENATION) {
                    // the only thing that can increase size

                    if (lenA.cvalue.hasValue && lenB.cvalue.hasValue) {
                        len->cvalue.hasValue = 1;
                        len->cvalue.dtypeEnum = DT_I64;
                        len->cvalue.i64 += lenA.cvalue.i64 + lenB.cvalue.i64;
                        return Err::OK;
                    }

                    BinaryExpression* bex = new BinaryExpression();
                    // bex->oper = const_cast<Operator*>(&operators[OP_ADDITION]);// + OP_ADDITION;
                    bex->base.opType = OP_ADDITION;
                    bex->left = Reg.Node.copy(&lenA);
                    bex->right= Reg.Node.copy(&lenB);

                    len->expression = (Expression*) bex;
                    len->unrollExpression = 0;

                    //len->def = var->def;
                    //len->flags = IS_LENGTH;

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
                    len->base.flags |= flag;

                }

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

                    nbex->left = lenVar;
                    nbex->right = new Variable();
                    nbex->right->expression = (Expression*) obex;

                    len->expression = (Expression*) nbex;

                }

                return Err::OK;

            }

            case EXT_ARRAY_INITIALIZATION : {

                if (len->expression) return Err::OK;

                len->cvalue.hasValue = 1;
                len->cvalue.dtypeEnum = DT_I64;
                len->cvalue.i64 = ((ArrayInitialization*) (var->expression))->attributes.base.size;
                len->base.flags |= flag;

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
                    const Err::Err err = evaluateArrayLength(slice->arr, slice->eidx);
                    if (err < 0) return err;

                    isEndLength = 1;

                }

                if (!slice->bidx->cvalue.hasValue || !slice->eidx->cvalue.hasValue) {
                    // expression cannot be computed cmp-time

                    slice->len = new Variable();
                    slice->len->cvalue.dtypeEnum = DT_U64;

                    BinaryExpression* len = new BinaryExpression();
                    len->left = slice->eidx;
                    len->right = slice->bidx;
                    len->base.opType = OP_SUBTRACTION;

                    slice->len->expression = (Expression*) len;

                    return Err::OK;

                }

                Value ans = slice->eidx->cvalue;
                ans.dtypeEnum = DT_I64;
                Interpreter::applyOperator(OP_SUBTRACTION, &ans, &(slice->bidx->cvalue));

                if (!slice->len) slice->len = (Variable*) nalloc(nalc, NT_VARIABLE);
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

            default: {
                // TODO
            }

        }

        return Err::OK;

    }

    Err::Err evaluateDataTypes(Variable* op, TypeDefinition** customDtype, DataTypeEnum lvalueType, TypeDefinition* lvalueTypeDef) {

        Expression* ex = op->expression;
        if (!ex) {

            if (op->cvalue.dtypeEnum == DT_CUSTOM && customDtype) {
                *customDtype = op->cvalue.def;
            } else if (customDtype) {
                *customDtype = (TypeDefinition*) (op->cvalue.any);
            }

            return (Err::Err) op->cvalue.dtypeEnum;

        }

        int rdtype = DT_UNDEFINED;

        switch (ex->type) {

            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) ex;

                const DataTypeEnum dtype = uex->operand->cvalue.dtypeEnum;// (DataTypeEnum) evaluateDataTypes(uex->operand, customDtype);
                if (dtype < Err::OK) return (Err::Err) dtype;

                switch (uex->base.opType) {

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
                            Logger::log(logErr, "TODO error: unsupported operator with struct!");
                            return (Err::Err) DT_UNDEFINED;
                        }
                        op->cvalue.any = uex->operand->cvalue.any;
                        rdtype = dtype;

                }

                break;

            }

            case EXT_BINARY: {

                BinaryExpression* bex = (BinaryExpression*) ex;

                TypeDefinition* customDtypeA;
                const DataTypeEnum dtypeA = (DataTypeEnum) evaluateDataTypes(bex->left, &customDtypeA, lvalueType, lvalueTypeDef);
                if (dtypeA < Err::OK) return (Err::Err) dtypeA;

                const DataTypeEnum dtypeB = (DataTypeEnum) evaluateDataTypes(bex->right, customDtype, lvalueType, lvalueTypeDef);
                if (dtypeB < Err::OK) return (Err::Err) dtypeB;

                const int oper = bex->base.opType;
                switch (oper) {

                    case OP_SUBSCRIPT: {

                        if (dtypeA == DT_ARRAY) {

                            Array* arr = bex->left->cvalue.arr;

                            if (customDtype) {
                                *customDtype = (TypeDefinition*)bex->left->cvalue.arr->base.pointsTo;
                            }

                            if (dtypeB == DT_UNDEFINED) {
                                // appending to array, so returning array
                                op->cvalue.any = arr;
                                rdtype = DT_ARRAY;
                                break;
                            }

                            if (arr->flags & IS_CMP_TIME) {


                            }

                            op->cvalue.any = arr->base.pointsTo;
                            rdtype = arr->base.pointsToEnum;

                        } else if (dtypeA == DT_POINTER) {

                            Pointer* ptr = bex->left->cvalue.ptr;

                            op->cvalue.any = ptr->pointsTo;
                            rdtype = ptr->pointsToEnum;

                            if (customDtype) {
                                *customDtype = (TypeDefinition*) bex->left->cvalue.ptr->pointsTo;
                            }

                        } else {

                            Logger::log(logErr, "Only array and pointer can be indexed!");
                            return Err::ARRAY_EXPECTED;

                        }

                        break;
                    }

                    case OP_DEREFERENCE_MEMBER_SELECTION:
                    case OP_MEMBER_SELECTION: {

                        if (dtypeA == DT_ARRAY) {

                            if (Strings::compare((String*) &bex->right->name, String(Lex::KWS_ARRAY_LENGTH))) {

                                const Err::Err err = evaluateArrayLength(bex->left, IS_LENGTH);
                                if (err < 0) return err;

                            } else if (Strings::compare((String*) &bex->right->name, String(Lex::KWS_ARRAY_SIZE))) {

                                const Err::Err err = evaluateArrayLength(bex->left, IS_LENGTH | IS_SIZE);
                                if (err < 0) return err;

                            } else {
                                Logger::log(logErr, "Unknown member of array!");
                                return Err::UNEXPECTED_SYMBOL;
                            }

                            return (Err::Err) DT_U64;

                        }

                        if (dtypeA == DT_ENUM) {

                            Enumerator* en = (Enumerator*) customDtypeA;
                            Variable* var = Reg.Find.inArray(&en->vars, (String*) &bex->right->name);
                            if (!var) {
                                Logger::log(logErr, "Unable to find member of enum!", bex->right->base.span, 0);
                                return Err::UNEXPECTED_SYMBOL;
                            }

                            // just a patch, be fixed other way, should happen with refactoring
                            // TODO : dont forget, otherwise memory leak...
                           // WrapperExpression* tmp = new WrapperExpression();
                            //tmp->operand = var;
                            //op->expression = tmp;

                            return (Err::Err) ((Enumerator*) customDtypeA)->dtype;

                        }

                        if (dtypeA == DT_POINTER) {

                            Variable* var = (Variable*) bex->right;
                            Pointer* ptr = (Pointer*) customDtypeA;

                            if (ptr->pointsToEnum != DT_CUSTOM || !ptr->pointsTo) {
                                Logger::log(logErr, "Invalid type of dereferenced pointer for member selection!", var->base.span, var->name.len);
                                return Err::INVALID_TYPE_CONVERSION;
                            }

                            bex->base.opType = OP_DEREFERENCE_MEMBER_SELECTION;
                            customDtypeA = (TypeDefinition*) (ptr->pointsTo);

                        } else if (dtypeA != DT_CUSTOM) {
                            Logger::log(logErr, "Invalid type for member selection!", bex->right->base.span, bex->right->name.len);
                            return Err::INVALID_TYPE_CONVERSION;
                        }

                        Variable* var = (Variable*) bex->right;
                        Variable* ans = Reg.Find.inArray(&customDtypeA->vars, (String*) &var->name);
                        if (!ans) {
                            Logger::log(logErr, ERR_STR(Err::INVALID_ATTRIBUTE_NAME), var->base.span, var->name.len, var->name.len, var->name.buff);
                            return Err::INVALID_ATTRIBUTE_NAME;
                        }

                        Reg.Node.copy(var, ans);

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
                        return (Err::Err) DT_UNDEFINED;

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
                        return (Err::Err) DT_UNDEFINED;

                    }

                    default:

                        if (dtypeA >= dtypeB) {
                            op->cvalue.any = bex->left->cvalue.any;
                            rdtype = dtypeA;
                        } else {
                            op->cvalue.any = bex->right->cvalue.any;
                            rdtype = dtypeB;
                        }

                }

                break;

            }

            case EXT_TERNARY: {

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

                DataTypeEnum arrDtype = sex->arr->cvalue.dtypeEnum;// (DataTypeEnum) evaluateDataTypes(sex->arr);
                if (!validateImplicitCast(arrDtype, DT_ARRAY)) {
                    return (Err::Err) DT_UNDEFINED;
                }

                DataTypeEnum idxDtype;

                // idxDtype = (DataTypeEnum) evaluateDataTypes(sex->bidx);
                idxDtype = sex->bidx->cvalue.dtypeEnum;
                if (idxDtype == DT_UNDEFINED && !sex->bidx->expression) {
                    sex->bidx->cvalue.dtypeEnum = DT_I64;
                    sex->bidx->cvalue.hasValue = 1;
                    sex->bidx->cvalue.i64 = 0;
                } else if (!validateImplicitCast(idxDtype, DT_INT)) {
                    return (Err::Err) DT_UNDEFINED;
                }

                // idxDtype = (DataTypeEnum) evaluateDataTypes(sex->eidx);
                idxDtype = sex->eidx->cvalue.dtypeEnum;
                if ((idxDtype != DT_UNDEFINED || sex->eidx->expression) && !validateImplicitCast(idxDtype, DT_INT)) {
                    return (Err::Err) DT_UNDEFINED;
                }

                if (customDtype) {
                    *customDtype = (TypeDefinition*) (sex->arr->cvalue.any);
                }

                return (Err::Err) DT_ARRAY;

            }

            case EXT_STRING_INITIALIZATION: {

                StringInitialization* init = (StringInitialization*) ex;

                if (customDtype) {
                    *customDtype = (TypeDefinition*) ex;
                }

                return (Err::Err) DT_STRING;

            }

            case EXT_ARRAY_INITIALIZATION: {

                ArrayInitialization* aex = (ArrayInitialization*) ex;

                if (lvalueType != DT_ARRAY) {
                    return (Err::Err) DT_UNDEFINED;
                }

                Array* lvalueDtype = (Array*)lvalueTypeDef;
                DataTypeEnum dtypeB = (DataTypeEnum) lvalueDtype->base.pointsToEnum;

                DataTypeEnum initDtype = DT_UNDEFINED;
                for (int i = 0; i < aex->attributes.base.size; i++) {

                    Variable* attribute = (Variable*) DArray::get(&aex->attributes.base, i);
                    DataTypeEnum dtypeA = attribute->cvalue.dtypeEnum;// (DataTypeEnum) evaluateDataTypes(attribute);
                    if (i == 0) initDtype = dtypeA;

                    if (!validateImplicitCast(dtypeB, dtypeA)) {
                        return (Err::Err) DT_UNDEFINED;
                    }

                }

                if (customDtype) *customDtype = lvalueTypeDef;

                if (op) {
                    op->cvalue.dtypeEnum = lvalueType;
                    op->cvalue.arr = new Array();
                    op->cvalue.arr->base.pointsToEnum = initDtype;
                    op->cvalue.arr->base.pointsTo = NULL;
                }
                return (Err::Err) lvalueType;

            }

            case EXT_TYPE_INITIALIZATION: {

                if (lvalueType != DT_CUSTOM) {
                    Logger::log(logErr, "TODO error: Invalid lvalue for type initialization used!");
                    return Err::INVALID_LVALUE;
                }

                TypeInitialization* tex = (TypeInitialization*) ex;

                const int err = validateTypeInitialization(lvalueTypeDef, tex);
                if (err < 0) return (Err::Err) err;

                if (customDtype) *customDtype = lvalueTypeDef;
                return (Err::Err) lvalueType; // (lvalueType == DT_ARRAY) ? DT_CUSTOM;

            }

            default: {
                // TODO
            }

        }

        op->cvalue.dtypeEnum = (DataTypeEnum) rdtype;
        return (Err::Err) rdtype;

    }

    // returns DataTypeEnum of expression or error
    // assumings that each operand has defined dtype (evaluateDataTypes ran through branch)
    Err::Err evaluate(Variable* op, TypeDefinition** customDtype) {

        Expression* ex = op->expression;
        if (!ex) {

            const DataTypeEnum dtype = op->cvalue.dtypeEnum;
            if (dtype == DT_CUSTOM && customDtype) {
                *customDtype = op->def->var->cvalue.def;
            }

            if (op->def && !(op->cvalue.hasValue)) {
                op->cvalue = op->def->var->cvalue;
            }

            return (Err::Err) dtype;

        }

        switch (ex->type) {

            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) ex;

                const int dtype = evaluate(uex->operand);
                if (dtype < Err::OK) return (Err::Err) dtype;

                if (!(uex->operand->cvalue.hasValue)) {
                    Logger::log(logErr, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED), uex->operand->base.span);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                }

                int x = Interpreter::applyOperator(uex->base.opType, &(uex->operand->cvalue));
                op->cvalue = uex->operand->cvalue;

                return (Err::Err) op->cvalue.dtypeEnum;

            }

            case EXT_BINARY: {

                BinaryExpression* bex = (BinaryExpression*) ex;

                const DataTypeEnum dtypeA = (DataTypeEnum) evaluate(bex->left);
                if (dtypeA < Err::OK) return (Err::Err) dtypeA;

                if (!(bex->left->cvalue.hasValue)) {
                    Logger::log(logErr, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED), bex->left->base.span);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                }

                const DataTypeEnum dtypeB = (DataTypeEnum) evaluate(bex->right);
                if (dtypeB < Err::OK) return (Err::Err) dtypeB;

                if (!(bex->right->cvalue.hasValue)) {
                    Logger::log(logErr, ERR_STR(Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED), bex->right->base.span);
                    return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                }

                const int hasValueA = bex->left->cvalue.hasValue;
                const int hasValueB = bex->right->cvalue.hasValue;

                if (dtypeA < dtypeB) {
                    Variable* tmp = bex->left;
                    bex->left = bex->right;
                    bex->right = tmp;
                }


                Interpreter::applyOperator(bex->base.opType, &(bex->left->cvalue), &(bex->right->cvalue));
                op->cvalue = bex->left->cvalue;

                return (Err::Err) op->cvalue.dtypeEnum;

            }

            case EXT_TERNARY: {

            }

            case EXT_FUNCTION_CALL: {

                FunctionCall* fex = (FunctionCall*) ex;

                for (int i = 0; i < fex->inArgs.base.size; i++) {

                    Variable* cArg = (Variable*) DArray::get(&fex->inArgs.base, i);
                    VariableDefinition* fArg = (VariableDefinition*) DArray::get(&fex->fcn->prototype.inArgs.base, i);

                    const DataTypeEnum err = (DataTypeEnum) evaluate(cArg);
                    if (err < Err::OK) return (Err::Err) err;
                    fArg->var->cvalue.ptr = cArg->cvalue.ptr;
                }

                const DataTypeEnum err = (DataTypeEnum) Interpreter::execFunction(fex->fcn, op);
                if (err < 0) return (Err::Err) err;

                op->cvalue.hasValue = 1;
                return (Err::Err) err;

            }

            case EXT_ARRAY_INITIALIZATION: {

                ArrayInitialization* init = (ArrayInitialization*) ex;

                for (int i = 0; i < init->attributes.base.size; i++) {
                    Variable* attribute = (Variable*) DArray::get(&init->attributes.base, i);
                    const int err = evaluate(attribute);
                    if (err < Err::OK) return (Err::Err) err;
                }

                return Err::OK;

            }

            case EXT_TYPE_INITIALIZATION: {

                TypeInitialization* tex = (TypeInitialization*) ex;

                for (int i = 0; i < tex->attributes.base.size; i++) {
                    Variable* attribute = (Variable*) DArray::get(&tex->attributes.base, i);
                    const int err = evaluate(attribute);
                    if (err < Err::OK) return (Err::Err) err;
                }

                return Err::OK;

            }

            default: {
                // TODO
            }

        }

        // TODO
        return (Err::Err) -1;

    }



   // ================
   //  MISCELLANEOUS

    // TODO
    int computeSizeOfDataType(TypeDefinition* const def) {

        if (def->dtype.size > 0) return def->dtype.size;

        const int isUnion = def->base.type == NT_UNION;

        int accSize = 0;
        for (int i = 0; i < def->vars.base.size; i++) {

            Variable* const var = (Variable*) DArray::get(&def->vars.base, i);

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
                        Logger::log(logErr, "Was unable to compute array length while computing size of the %.*s!", var->base.span, var->name.len, def->name.len, def->name.buff);
                        return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                    }

                    int chunkSize = 0;
                    if (arr->base.pointsToEnum == DT_CUSTOM) {
                        chunkSize = computeSizeOfDataType((TypeDefinition*) arr->base.pointsTo);
                    } else {
                        chunkSize = dataTypes[arr->base.pointsToEnum].size;
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

        def->dtype.size = accSize;
        return accSize;

    }

    // level has to be 0, if its output matters
    int getFirstNonArrayDtype(Array* arr, const int maxLevel, int* level) {

        const int dtype = arr->base.pointsToEnum;

        if (maxLevel > 0 && *level >= maxLevel) return dtype;
        if (dtype != DT_ARRAY) return dtype;

        if (level) *level = *level + 1;
        return getFirstNonArrayDtype((Array*) arr->base.pointsTo, maxLevel, level);

    }


    Variable* findErrorInErrorSet(ErrorSet* eset, QualifiedName* var) {

        Namespace* tmpNspace;

        int i = 0;
        const int len = var->pathSize;

        for (i = 0; i < len; i++) {

            INamed* nm = var->path + i;
            Variable* tmp = Reg.Find.inArray(&eset->vars, nm);
            if (!tmp) return NULL;

            eset = tmp->cvalue.err;

        }

        return Reg.Find.inArray(&eset->vars, (String*) &var);

    }

    // scope cannot be NULL
    Variable* findDefinition(Scope* scope, QualifiedName* const inVar, int idx) {

        const String name = { inVar->buff, (uint64_t) inVar->len };
        // int idx = inVar->parentIdx;

        while (scope) {

            SyntaxNode* node = (SyntaxNode*) OrderedDict::get(&scope->defSearch, name);
            if (node) {

                if (node->parentIdx < idx) {
                    switch (node->type) {

                        case NT_VARIABLE : {
                            return (Variable*) node;
                            //Variable* var = (Variable*) node;
                            //if (Utils::match(var, inVar)) return var;
                        }

                        default: {

                        }

                    }

                }

            } else {

                for (int i = 0; i < scope->usings.base.size; i++) {

                    Using* usng = (Using*) DArray::get(&scope->usings.base, i);
                    if (usng->var->type != NT_FUNCTION) continue;

                    Variable* err = findErrorInErrorSet(((Function*) (usng->var))->errorSet, inVar);
                    if (err) return err;

                }

            }

            idx = scope->base.parentIdx;
            scope = scope->base.scope;

        }

        return NULL;

    }

    inline int isUniqueInCollection(DArray::Container* collection, INamed* named, Span* span, MemberOffset mName) {

        for (int i = 0; i < collection->size; i++) {

            SyntaxNode* const item = (SyntaxNode*) DArray::get(collection, i);
            if (item->flags & IS_UNIQUE) continue;

            INamed* itemName = (INamed*) getMember(item, mName);
            if (Strings::compare(*named, *itemName)) {
                Logger::log(logErr, ERR_STR(Err::SYMBOL_ALREADY_DEFINED), span, named->len);
                return Err::SYMBOL_ALREADY_DEFINED;
            }
            //item->flags |= IS_UNIQUE;

        }

        return Err::OK;

    }

    int isUnique(Scope* sc, INamed* named, Span* span) {

        if (isUniqueInCollection(&sc->defs.base, named, span, getMemberOffset(Variable, name)) < 0) return Err::SYMBOL_ALREADY_DEFINED;
        if (isUniqueInCollection(&sc->fcns.base, named, span, getMemberOffset(Function, name)) < 0) return Err::SYMBOL_ALREADY_DEFINED;
        if (isUniqueInCollection(&sc->customDataTypes.base, named, span, getMemberOffset(TypeDefinition, name)) < 0) return Err::SYMBOL_ALREADY_DEFINED;
        if (isUniqueInCollection(&sc->labels.base, named, span, getMemberOffset(Label, name)) < 0) return Err::SYMBOL_ALREADY_DEFINED;

        return Err::OK;

    }

    int match(FunctionPrototype* const fptrA, FunctionPrototype* const fptrB) {

        if (fptrA->inArgs.base.size != fptrB->inArgs.base.size) return 0;

        for (int i = 0; i < fptrA->inArgs.base.size; i++) {
            const DataTypeEnum defA = ((VariableDefinition*) DArray::get(&fptrA->inArgs.base, i))->var->cvalue.dtypeEnum;
            const DataTypeEnum defB = ((VariableDefinition*) DArray::get(&fptrB->inArgs.base, i))->var->cvalue.dtypeEnum;
            if (defA != defB) return 0;
        }

        return 1;

    }

    Function* findExactFunction(Scope* scope, INamed* const name, FunctionPrototype* const fptr) {

        while (scope) {

            Function* fcns = ((Function*) scope->fcns.base.buffer);

            for (int i = 0; i < scope->fcns.base.size; i++) {
                if (!Strings::compare((String*) &(fcns + i)->name, name) || !match(fptr, &(fcns + i)->prototype)) {
                    continue;
                }
                return fcns + i;
            }

            scope = scope->base.scope;

        }

        return NULL;

    }

    // working with global array fCandidates
    void findCandidateFunctions(Scope* scope, FunctionCall* call) {

        DArray::clear(&fCandidates);

        const String callName = { call->name.buff, call->name.len };

        if (call->name.pathSize > 0) {

            Namespace* nspace = NULL;
            ErrorSet* eset = NULL;

            Logger::mute = 1;
            const Err::Err err = validateQualifiedName(scope, &call->name, &nspace, &eset);
            Logger::mute = 0;

            if (err < 0 || eset || !nspace) return;
            scope = (Scope*) nspace;

        }

        while (scope) {

            Function** fcns = (Function**) scope->fcns.base.buffer;

            for (int i = 0; i < scope->fcns.base.size; i++) {

                Function* fcn = fcns[i];
                const String fcnName = { fcn->name.buff, fcn->name.len };

                if (!cstrcmp(callName, fcnName)) {
                    continue;
                }

                const int fcnInCnt = fcn->prototype.inArgs.base.size;
                const int callInCnt = call->inArgs.base.size;
                const int lastArgDtype = fcnInCnt > 0 ? 0 : 0;// TODO (fcns + i)->prototype.inArgs[fcnInCnt - 1]->var->cvalue.dtypeEnum : DT_UNDEFINED;

                if (
                    fcnInCnt == callInCnt ||
                    (fcnInCnt < callInCnt && lastArgDtype == DT_MULTIPLE_TYPES)
                ) {
                    FunctionScore tmp = { fcn, 0 };
                    DArray::push(&fCandidates, &tmp);
                }

            }

            scope = scope->base.scope;

        }

    }

    int findClosestFunction(Variable* callOp, Function** outFcn) {

        Scope* scope = callOp->base.scope;
        FunctionCall* call = (FunctionCall*)callOp->expression;
        const int callInCnt = call->inArgs.base.size;

        findCandidateFunctions(scope, call);

        for (int j = 0; j < fCandidates.size; j++) {

            Function* fcn = ((FunctionScore*) DArray::get(&fCandidates, j))->fcn;
            int score = (fcn->prototype.inArgs.base.size == 0) ? 100 : 0;
            const int fcnInCnt = fcn->prototype.inArgs.base.size;

            int k = 0;
            for (int i = 0; i < fcn->prototype.inArgs.base.size; i++) {

                if (k >= callInCnt) {
                    score = 0;
                    break;
                }

                Variable* fArg = ((VariableDefinition*) DArray::get(&fcn->prototype.inArgs.base, i))->var;
                Variable* cArg = (Variable*) DArray::get(&call->inArgs.base, k);

                k++;

                const int fDtype = fArg->cvalue.dtypeEnum;
                const int cDtype = cArg->cvalue.dtypeEnum;// evaluateDataTypes(cArg);

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

                if (isSignedInt(cDtype) && isSignedInt(fDtype)) {
                    if (cDtype < fDtype) {
                        score += FOS_PROMOTION;
                    }
                    else {
                        score += FOS_SIZE_DECREASE;
                    }
                    continue;
                }

                if (isUnsignedInt(cDtype) && isUnsignedInt(fDtype)) {
                    if (cDtype < fDtype) {
                        score += FOS_PROMOTION;
                    }
                    else {
                        score += FOS_SIZE_DECREASE;
                    }
                    continue;
                }

                if (
                    isSignedInt(cDtype) && isUnsignedInt(fDtype) ||
                    isSignedInt(cDtype) && isUnsignedInt(fDtype)
                ) {

                    if ((cDtype - fDtype <= DT_U64 - DT_I64)) {
                        score += FOS_SIGN_CHANGE;
                    } else {
                        score += FOS_SIZE_DECREASE;
                    }

                    continue;

                }

                if (isFloat(cDtype) && isFloat(fDtype)) {
                    if ((cDtype - fDtype <= DT_U64 - DT_I64)) {
                        score += FOS_PROMOTION;
                    }
                    else {
                        score += FOS_SIZE_DECREASE;
                    }
                    continue;
                }

                if (isInt(cDtype) && isFloat(fDtype)) {
                    score += FOS_SIGN_CHANGE;
                    continue;
                }

                if (isFloat(cDtype) && isInt(fDtype)) {
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
                FunctionScore* fscore = (FunctionScore*) fCandidates.buffer;
                fscore[j].score = score;
            } else {
                FunctionScore* tmp = ((FunctionScore*) fCandidates.buffer) + (fCandidates.size - 1);
                DArray::set(&fCandidates, j, &tmp);
                DArray::pop(&fCandidates);
                j--;
            }

        }

        int bestIdx = 0;
        int bestScore = 0;
        int sameScoreCnt = 0;
        for (int i = 0; i < fCandidates.size; i++) {
            const int score = ((FunctionScore*) DArray::get(&fCandidates, i))->score;
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
            *outFcn = ((FunctionScore*) DArray::get(&fCandidates, bestIdx))->fcn;
            return Err::OK;
        } else {

            if (bestScore <= 0 ) {
                //Logger::log(logErr, ERR_STR(Err::NO_MATCHING_FUNCTION_FOUND));
                return Err::NO_MATCHING_FUNCTION_FOUND;
            }

            //Logger::log(logErr, ERR_STR(Err::MORE_THAN_ONE_OVERLOAD_MATCH), callOp->span, 1);
            return Err::MORE_THAN_ONE_OVERLOAD_MATCH;

        }

        return Err::OK;

    }

}
