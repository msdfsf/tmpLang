// #include "allocator.h"
#include "validator.h"
#include "array_list.h"
#include "data_types.h"
#include "globals.h"
#include "logger.h"
#include "operators.h"
#include "string.h"
#include "strlib.h"
#include "syntax.h"
#include "interpreter.h"
#include "lexer.h"
#include "supplement/runtime.h"
#include "utils.h"
#include "diagnostic.h"

#include <algorithm>
#include <cstdint>
#include <cstring>



namespace Validator {

    enum {
        EXACT_MATCH = 1
    };

    Logger::Type logErr = { .level = Logger::ERROR,   .tag = "validator" };
    Logger::Type logWrn = { .level = Logger::WARNING, .tag = "validator" };

    DArray::Container fCandidates; // f as function



    Err::Err validate(AstContext* ctx) {

        Err::Err err;

        DArray::init(&fCandidates, 32, sizeof(FunctionPrototype));

        err = linkAll(ctx);
        if (err != Err::OK) return err;

        err = validateTypeDefinitions(ctx);
        if (err != Err::OK) return err;

        err = resolveTypes(ctx);
        if (err != Err::OK) return err;

        err = computeTypesInfo(ctx);
        if (err != Err::OK) return err;



        // TEST:
        // TODO before :
        //  1) resolve types in array lengths
        //  2) check if function returns
        Interpreter::init();
        Function* fcn = *(Function**) DArray::get(&ctx->reg->fcns, 0);
        // fcn->exe = (Interpreter::ExeBlock*) alloc(alc, sizeof(Interpreter::ExeBlock));
        Interpreter::compile(fcn);
        Interpreter::print(fcn);

        Value val;
        Interpreter::exec(fcn, &val);
        printf("val: %d\n", val.i64);



        return Err::OK;

    }



    // ======================
    // LINK FUNCTIONS

    Err::Err linkAll(AstContext* ctx) {

        Err::Err err;

        err = linkDataTypes(ctx);
        if (err != Err::OK) return err;

        err = linkErrorSets(ctx);
        if (err != Err::OK) return err;

        err = linkVariables(ctx);
        if (err != Err::OK) return err;

        err = linkGotos(ctx);
        if (err != Err::OK) return err;

        err = linkFunctionCalls(ctx);
        if (err != Err::OK) return err;

        return err;

    }

    Err::Err linkDataTypes(AstContext* ctx) {

        for (int i = 0; i < ctx->reg->customDataTypesReferences.size; i++) {

            VariableDefinition* const varDef = *(VariableDefinition**) DArray::get(&ctx->reg->customDataTypesReferences, i);

            void** type;
            Type::Kind* typeKind;

            if (varDef->lastPtr) {
                type = (void**) &(varDef->lastPtr->pointsTo);
                typeKind = &(varDef->lastPtr->pointsToKind);
            } else {
                type = (void**) &(varDef->var->value.any);
                typeKind = &(varDef->var->value.typeKind);
            }

            // TODO : why path is pointer
            Scope* scope = varDef->base.scope;
            if (varDef->dtype && varDef->dtype->path && varDef->dtype->path->len > 0) {
                Namespace* nspace = NULL;
                ErrorSet* eset = NULL;
                const Err::Err err = validateQualifiedName(scope, varDef->dtype, &nspace, &eset);
                if (err != Err::OK) return err;
                if (eset) continue;
                scope = (Scope*) nspace;
            }

            TypeDefinition* td = Ast::Find::inScopeTypeDefinition(scope, (String*) varDef->dtype);
            if (td) {
                *type = (void*) td;
                *typeKind = Type::DT_CUSTOM;
                continue;
            }

            Enumerator* en = (Enumerator*) Ast::Find::inScopeEnumerator(scope, (String*) varDef->dtype);
            if (en) {
                *type = Type::basicTypes + en->dtype;
                *typeKind = en->dtype;
                continue;
            }

            Union* un = (Union*) Ast::Find::inScopeUnion(scope, (String*) varDef->dtype);
            if (un) {
                *type = (void*) un;
                *typeKind = Type::DT_UNION;
                continue;
            }

            ErrorSet* er = Ast::Find::inScopeErrorSet(scope, (String*) varDef->dtype);
            if (er) {
                *type = (void*) er;
                *typeKind = Type::DT_ERROR;
                continue;
            }

            Logger::log(logErr, Err::str(Err::UNKNOWN_DATA_TYPE), varDef->base.span, varDef->dtype->len);
            return Err::UNKNOWN_DATA_TYPE;

        }

        return Err::OK;

    }

    Err::Err linkErrorSets(AstContext* ctx) {

        for (int i = 0; i < ctx->reg->fcns.size; i++) {

            Function* const fcn = *(Function**) DArray::get(&ctx->reg->fcns, i);
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
                    Logger::log(logErr, Err::str(Err::UNKNOWN_ERROR_SET), fcn->base.span);
                    return Err::UNKNOWN_ERROR_SET;
                }

                if (nspace) {
                    // TODO
                    eset = NULL;//ctx->reg->Find.inArray(&nspace->scope.customErrors, (String*) errName);
                    if (!eset) {
                        Logger::log(logErr, Err::str(Err::UNKNOWN_ERROR_SET), fcn->base.span);
                        return Err::UNKNOWN_ERROR_SET;
                    }
                } else if (eset) {
                    eset = Ast::Find::inScopeErrorSet(eset->base.scope, (String*) errName);
                    if (!eset) {
                        Logger::log(logErr, "Unknown or empty error set!", fcn->base.span);
                        return Err::UNKNOWN_ERROR_SET;
                    }
                }

                fcn->errorSet = eset;

            } else {

                ErrorSet* const eset = Ast::Find::inScopeErrorSet(fcn->base.scope, (String*) errName);
                if (!eset) {
                    Logger::log(logErr, Err::str(Err::UNKNOWN_ERROR_SET), fcn->base.span);
                    return Err::UNKNOWN_ERROR_SET;
                }

                fcn->errorSet = eset;

            }

        }

        return Err::OK;

    }

    Err::Err linkVariables(AstContext* ctx) {

        // in case of scopeNames var is the last name (ex. point.x, var is then x)
        // scopeNames are sorted from left to right as written
        for (int i = 0; i < ctx->reg->variables.size; i++) {

            Variable* const var = *(Variable**) DArray::get(&ctx->reg->variables, i);
            Variable* tmpVar;

            // TODO: TMP
            var->base.definitionIdx = var->base.scope->childrenCount;

            // const int scopeNamesLen = var->scopeNames.size();

            Scope* scope = var->base.scope;
            if (var->name.pathSize > 0) {
                Namespace* nspace = NULL;
                ErrorSet* eset = NULL;
                const Err::Err err = validateQualifiedName(scope, &var->name, &nspace, &eset);
                if (err != Err::OK) return err;
                if (eset) {
                    tmpVar = Ast::Find::inArray(eset->vars, eset->varCount, (String*) &var->name);
                    if (!tmpVar) {
                        Logger::log(logErr, Err::str(Err::UNKNOWN_ERROR_SET), var->base.span, var->name.len);
                        return Err::UNKNOWN_ERROR_SET;
                    }
                    Ast::Node::copyRef(var, tmpVar);
                    continue;
                }
                scope = (Scope*) nspace;

                // if namespace, we dont care about order

                tmpVar = Ast::Find::inScopeVariable(scope, (String*) &var->name);

                if (tmpVar) {
                    Ast::Node::copyRef(var, tmpVar);
                    continue;
                }

            } else {

                /*
                SyntaxNode* tmp = findDefinition(scope, var, var->parentIdx);
                if (tmp && tmp->type == NT_VARIABLE) {
                    ctx->reg->Find.copy(var, (Variable*) tmp);
                    continue;
                } else if (tmp && tmp->type == NT_ERROR) {
                    var->value.err = (ErrorSet*) tmp;
                    var->value.typeKind = DT_ERROR;
                    continue;
                } else {
                    Logger::log(Logger::ERROR, "TODO : Error!");
                    return Err::UNEXPECTED_SYMBOL;
                }
                */
                tmpVar = findDefinition(scope, &var->name, var->base.definitionIdx);
                if (tmpVar) {
                    Ast::Node::copyRef(var, tmpVar);
                    continue;
                }

            }

            /*
            Variable* tmpVar = Utils::find<Variable>(scope, var->name, var->nameLen, &Scope::vars);
            if (tmpVar) {
                ctx->reg->Find.copy(var, tmpVar);
                continue;
            }
            */

            tmpVar = Ast::Find::inArray(Ast::Internal::variables, Ast::Internal::IV_COUNT, (String*) &var->name);
            if (tmpVar) {
                Ast::Node::copyRef(var, tmpVar);
                continue;
            }

            Enumerator* en = Ast::Find::inScopeEnumerator(scope, (String*) &var->name);
            if (en) {
                var->value.typeKind = Type::DT_ENUM;
                var->value.enm = en;
                continue;
            }

            // or function pointer
            Function* fcn = Ast::Find::inScopeFunction(scope, (String*) &var->name);
            if (fcn) {
                var->value.typeKind = Type::DT_FUNCTION;
                var->value.fcn = NULL;
                continue;
            }

            // Data type itself represents its size
            TypeDefinition* td = Ast::Find::inScopeTypeDefinition(scope, (String*) (&var->name));
            if (td) {
                // TODO : it seems better to compute it later and here just collect all candidates

                const int size = computeSizeOfDataType(td);
                if (size < 0) return (Err::Err) size;

                var->value.i64 = size;
                var->value.hasValue = true;
                var->value.typeKind = Type::DT_I64;

                continue;

            }

            Logger::log(logErr, Err::str(Err::UNKNOWN_VARIABLE), var->base.span, var->name.len, var->name.buff);
            return Err::UNKNOWN_VARIABLE;

        }

        return Err::OK;

    }

    // link goto statements
    Err::Err linkGotos(AstContext* ctx) {

        for (int i = 0; i < ctx->reg->gotos.size; i++) {

            GotoStatement* gt = (GotoStatement*) DArray::get(&ctx->reg->gotos, i);

            Label* lb = Ast::Find::inScopeLabel(gt->base.scope, (String*) &gt->name);
            if (lb) {
                gt->label = lb;
                continue;
            }

            Logger::log(logErr, Err::str(Err::UNKNOWN_VARIABLE), gt->base.span, gt->name.len, gt->name.len, gt->name.buff);
            return Err::UNKNOWN_VARIABLE;

        }

    }

    Err::Err linkFunctionCalls(AstContext* ctx) {

        for (int i = 0; i < (int) ctx->reg->fcnCalls.size; i++) {

            Variable* fcnCallOp = *(Variable**) DArray::get(&ctx->reg->fcnCalls, i);
            FunctionCall* fcnCall = (FunctionCall*) (fcnCallOp->expression);

            if (fcnCall->fcn) continue;

            Function* fcn;
            const int err = findClosestFunction(fcnCallOp, &fcn);
            if (err < 0) {
                // check for function pointer

                Variable* var = findDefinition(fcnCallOp->base.scope, &fcnCall->name, fcnCallOp->base.definitionIdx);
                if (!var) {
                    Logger::log(logErr, Err::str((Err::Err) err), fcnCallOp->base.span, fcnCall->name.len);
                    return (Err::Err) err;
                }

                fcnCall->fptr = var;
                fcnCall->fcn = NULL;
                fcnCall->outArg = new Variable();
                fcnCall->outArg->value.hasValue = false;
                fcnCall->outArg->value.typeKind =
                    var->value.fcn->outArg->var->value.typeKind;

                continue;

            }

            fcnCall->fptr = NULL;
            fcnCall->fcn = fcn;
            fcnCall->outArg = new Variable();
            fcnCall->outArg->value.hasValue = false;
            fcnCall->outArg->value.typeKind =
                fcn->prototype.outArg->var->value.typeKind;

        }

        return Err::OK;

    }

    Err::Err verifyFunctionsAreGlobal(AstContext* ctx) {

        for (int i = 0; i < ctx->reg->fcns.size; i++) {

            Function* const fcn = *(Function**) DArray::get(&ctx->reg->fcns, i);

            Scope* sc = fcn->base.scope;
            while (sc) {
                if (sc->base.type == NT_SCOPE) break;
                sc = sc->base.scope;
            }
            //Scope* const sc = fcn->scope->type == NT_SCOPE ? fcn->scope : fcn->scope->scope;
            if (sc != SyntaxNode::root) {
                Logger::log(logErr, Err::str(Err::GLOBAL_SCOPE_REQUIRED), fcn->base.span, fcn->name.len);
                return Err::GLOBAL_SCOPE_REQUIRED;
            }

        }

        return Err::OK;

    }







    // ======================
    // TYPE RESOLUTION STUFF

    Err::Err computeTypeInfo(TypeDefinition* td) {

        if (td->state == TS_READY) return Err::OK;

        if (td->state == TS_RUNNING) {
            // TODO : add new error, add path logging
            Logger::log(logErr, "TODO : Circular type definition detected!");
            return Err::CIRCULAR_IMPORT;
        }

        td->state = TS_RUNNING;

        uint64_t offset = 0;
        uint64_t align  = 0;

        for (int i = 0; i < td->varCount; i++) {
            Variable* var = td->vars[i];

            Type::TypeInfo* typeInfo = NULL;
            Type::Kind typeKind = var->value.typeKind;

            if (Type::isPrimitive(typeKind)) {

                typeInfo = Type::basicTypes + typeKind;

            } else if (typeKind == Type::DT_CUSTOM) {

                computeTypeInfo(td);
                typeInfo = (Type::TypeInfo*) td->typeInfo;

            } else if (typeKind == Type::DT_ARRAY) {

                // if length is NULL, we are basicly dealing with
                // a runtime length, and we can interpret our dtype
                // as slice
                /*
                if (!arr->length) {
                    *size = 16;
                    *align = 8;
                    return Err::OK;
                }

                arr->length = unwrap(arr->length);
                const uint64_t len = arr->length->value.u64;

                uint64_t elementSize;
                uint64_t elementAlign;
                Value tmpVal = toValue(&arr->base);
                // TODO : cache
                err = getDtypeInfo(&tmpVal, &elementSize, &elementAlign);
                if (err != Err::OK) return err;

                *size = len * elementSize;
                *align = elementAlign;
                */
            } else {

                return Err::NOT_YET_IMPLEMENTED;

            }

            offset += typeInfo->size;
            offset += Utils::getPadding(offset, typeInfo->align);
            align  = std::max(align, (uint64_t) typeInfo->align);
        }

        td->typeInfo->base.size = offset;
        td->typeInfo->base.size += Utils::getPadding(offset, align);
        td->typeInfo->base.align = align;
        td->typeInfo->base.rank = 0;

        return Err::OK;

    }

    Err::Err computeTypesInfo(AstContext* ctx) {

        for (int i = 0; i < ctx->reg->customDataTypes.size; i++) {
            TypeDefinition* td = *(TypeDefinition**) DArray::get(&ctx->reg->customDataTypes, i);
            const Err::Err err = computeTypeInfo(td);
            if (err!= Err::OK) return err;
        }

    }

    Err::Err applyImplicitCast(Value* lval, Variable* rvar) {

        if (lval->typeKind == Type::DT_MULTIPLE_TYPES) {
            return Err::OK;
        }

        if (isBasicDtype(lval->typeKind) &&
            lval->typeKind == rvar->value.typeKind
        ) {
            return Err::OK;
        }

        if (lval->typeKind == Type::DT_CUSTOM &&
            rvar->value.typeKind == lval->typeKind &&
            rvar->value.any == lval->any
        ) {
            return Err::OK;
        }

        if (rvar->value.hasValue) {
            // TODO: suppose to cast literal in place.
            castLiteral(&rvar->value, lval->typeKind);
            return Err::OK;
        }

        // TODO : proper implicit cast validation requaried!!!
        const int64_t ans = (int64_t) validateImplicitCast(
            rvar->value.any, lval->any,
            rvar->value.typeKind, lval->typeKind
        );

        if (ans < 0) {
            // TODO : error
            Logger::log(logErr, "It was a bad day for an implicit cast :(");
            return Err::UNEXPECTED_SYMBOL;
        } else if (ans == EXACT_MATCH) {
            return Err::OK;
        }

        // clone the current node to preserve also metadata
        // TODO: maybe no need to do a full copy
        Variable* innerOperand = Ast::Node::copy(rvar);

        Cast* castEx = Ast::Node::makeCast();
        castEx->operand = innerOperand;

        if (lval->typeKind != Type::DT_ARRAY) {
            castEx->target = lval->typeKind;
        } else {
            castEx->target = lval->arr->base.pointsToKind;
        }

        rvar->expression = (Expression*) castEx;
        rvar->value = *lval;

        return Err::OK;

    }

    Err::Err resolveTypes(AstContext* ctx) {

        Err::Err err;

        for (int i = 0; i < ctx->reg->variableDefinitions.size; i++) {

            VariableDefinition* def = *(VariableDefinition**) DArray::get(&ctx->reg->variableDefinitions, i);
            Value leftValue = def->var->value;

            err = resolveTypes(def->var, def->var);
            if (err != Err::OK) return err;

            err = applyImplicitCast(&leftValue, def->var);
            if (err != Err::OK) return err;

            // in case of static array, we may need to infer length
            // TODO : IS_CMP_TIME to IS_EMBEDED ?
            if (leftValue.typeKind == Type::DT_ARRAY && leftValue.arr->flags & IS_CMP_TIME) {
                Variable* len = (Variable*) unwrap(leftValue.arr->length);
                if (len && !len->value.hasValue) {
                    continue;
                }
            }

            def->var->value = leftValue;

        }

        // TODO : tobe removed, as it should happens naturaly
        for (int i = 0; i < ctx->reg->fcnCalls.size; i++) {

            Variable* var = *(Variable**) DArray::get(&ctx->reg->fcnCalls, i);
            FunctionCall* call = (FunctionCall*) var->expression;

            int variadic = 0;
            for (int i = 0; i < call->inArgCount; i++) {

                Variable* arg = call->inArgs[i];

                err = resolveTypes(arg, var);
                if (err != Err::OK) return err;

                if (variadic) continue;

                VariableDefinition* def = call->fcn->prototype.inArgs[i];
                if (def->var->value.typeKind == Type::DT_MULTIPLE_TYPES) {
                    variadic = 1;
                }

                err = applyImplicitCast(&def->var->value, arg);
                if (err != Err::OK) return err;

            }

        }

        for (int i = 0; i < ctx->reg->returnStatements.size; i++) {

            ReturnStatement* ret = *(ReturnStatement**) DArray::get(&ctx->reg->returnStatements, i);

            err = resolveTypes(ret->var, ret->fcn->prototype.outArg->var);
            if (err != Err::OK) return err;

            err = applyImplicitCast(&ret->fcn->prototype.outArg->var->value, ret->var);
            if (err != Err::OK) return err;

            // TODO : error?
        }

        for (int i = 0; i < ctx->reg->variableAssignments.size; i++) {

            VariableAssignment* ass = *(VariableAssignment**) DArray::get(&ctx->reg->variableAssignments, i);

            err = resolveTypes(ass->lvar, ass->lvar);
            if (err != Err::OK) return err;

            err = resolveTypes(ass->rvar, ass->lvar);
            if (err != Err::OK) return err;

            err = applyImplicitCast(&ass->lvar->value, ass->rvar);
            if (err != Err::OK) return err;

        }

        // TODO
        //
        return Err::OK;

    }

    inline void inheritType(Variable* dest, Value* source) {
        if (isPrimitive(source->typeKind)) {
            dest->value.typeKind = source->typeKind;
        } else {
            dest->value = *source;
        }
    }

    Err::Err resolveResultType(UnaryExpression* uex, Variable* var) {

        const OperatorEnum op = uex->base.opType;
        if (op == OP_GET_ADDRESS) {

            // LOOK AT : do we need to create?
            Pointer* ptr = Ast::Node::makePointer();
            ptr->pointsToKind = uex->operand->value.typeKind;
            ptr->pointsTo = uex->operand->value.any;

            var->value.ptr = ptr;
            var->value.typeKind = Type::DT_POINTER;

        } else if (op == OP_GET_VALUE) {

            // TODO : view binary version
            if (!isIndexable(uex->operand->value.typeKind)) {
                // TODO : eerorr
            }

            var->value.any = uex->operand->value.ptr->pointsTo;
            var->value.typeKind = uex->operand->value.ptr->pointsToKind;

        } else {

            inheritType(var, &uex->operand->value);

        }

        return Err::OK;

    }

    Err::Err resolveMember(BinaryExpression* bex, Variable* var) {

        Variable* parent = bex->left;
        Variable* member = bex->right;

        const Type::Kind parentType = parent->value.typeKind;
        const Type::Kind memberType = member->value.typeKind;

        String* memberName = (String*) &member->name;
        // TODO : add validation of members path name, doesnt suppose to have one
        //  but not sure it should happen here...

        if (parentType == Type::DT_ARRAY) {

            // TODO : for now this way, but think to chenge the operator or
            //   expression type, a bit of fragmentation, but thats what happens
            //   with additional alloc/dealloc
            if (Strings::compare(memberName, String(Lex::KWS_ARRAY_LENGTH))) {
                bex->base.base.type = EXT_GET_LENGTH;
                ((GetLength*) bex)->arr = parent;
            } else if (Strings::compare(memberName, String(Lex::KWS_ARRAY_SIZE))) {
                bex->base.base.type = EXT_GET_SIZE;
                ((GetSize*) bex)->arr = parent;
            } else {
                Logger::log(logErr, "Unknown member of array!");
                return Err::UNEXPECTED_SYMBOL;
            }

            var->value.typeKind = Type::DT_U64;
            // TODO : I guess we can just alias GetLength and GetSize
            //  to a BinaryExpression and change the type, so we can
            //  just keep the data and clean them when the time comes
            //  as we may need dealloc to work in arena case for the last
            //  allocated pointer, therefore we cannot free here as we corrupt
            //  the arena.
            // dealloc(alc, member);

            return Err::OK;

        }

        TypeDefinition* td;
        if (parentType == Type::DT_POINTER) {

            Pointer* ptr = parent->value.ptr;

            if (ptr->pointsToKind != Type::DT_CUSTOM || !ptr->pointsTo) {
                Logger::log(logErr, "Invalid type of dereferenced pointer for member selection!", var->base.span, var->name.len);
                return Err::INVALID_TYPE_CONVERSION;
            }

            bex->base.opType = OP_DEREFERENCE_MEMBER_SELECTION;
            td = (TypeDefinition*) ptr->pointsTo;

        } else if (parentType == Type::DT_CUSTOM) {

            td = (TypeDefinition*) parent->value.any;

        } else {

            Logger::log(logErr, "Invalid type for member selection!", bex->right->base.span, bex->right->name.len);
            return Err::INVALID_TYPE_CONVERSION;

        }

        Variable* attribute = Ast::Find::inArray(td->vars, td->varCount, (String*) &member->name);
        if (!attribute) {
            Logger::log(logErr, "TODO: Type doesnt have requested attribute!");
            return Err::INVALID_ATTRIBUTE_NAME;
        }

        var->value.typeKind = attribute->value.typeKind;
        var->value.any = attribute->value.any;

        return Err::OK;

        /*
        if (dtypeA == DT_ENUM) {

            Enumerator* en = parent->value.enm;
            Variable* var = ctx->reg->Find.inArray(&en->vars, memberName);
            if (!var) {
                Logger::log(logErr, "Unable to find member of enum!", member->base.span, 0);
                return Err::UNEXPECTED_SYMBOL;
            }

            return Err::OK;

        }

        if (dtypeA == DT_POINTER) {

            Variable* var = (Variable*) bex->right;
            Pointer* ptr = (Pointer*) customDtypeA;

            if (ptr->pointsToEnum != DT_CUSTOM || !ptr->pointsTo) {
                Logger::log(logErr, "Invalid type of dereferenced pointer for member selection!", var->base.span, var->name.len);
                return Err::INVALID_TYPE_CONVERSION;
            }

            bex->base.opType = OP_DEREFERENCE_MEMBER_SELECTION;
            customDtypeA = (TypeDefinition*)(ptr->pointsTo);

        } else if (dtypeA != DT_CUSTOM) {
            Logger::log(logErr, "Invalid type for member selection!", bex->right->base.span, bex->right->name.len);
            return Err::INVALID_TYPE_CONVERSION;
        }

        Variable* var = (Variable*)bex->right;
        Variable* ans = ctx->reg->Find.inArray(&customDtypeA->vars, (String*)&var->name);
        if (!ans) {
            Logger::log(logErr, Err::str(Err::INVALID_ATTRIBUTE_NAME), var->base.span, var->name.len, var->name.len, var->name.buff);
            return Err::INVALID_ATTRIBUTE_NAME;
        }

        ctx->reg->Node.copy(var, ans);

        rdtype = ans->value.typeKind;
        break;
        */
    }

    Err::Err resolveResultType(BinaryExpression* bex, Variable* var) {

        Type::Kind lDtype = bex->left->value.typeKind;
        Type::Kind rDtype = bex->right->value.typeKind;

        // Usually the result type can be derived from
        // operands ranks but in few cases operator can
        // influence the output type (arr[i])
        if (bex->base.opType == OP_SUBSCRIPT) {

            // TODO : move from this to a direct check, as we
            //   may need different behavior for each type later
            if (!isIndexable(lDtype)) {
                // TODO : throw error
            }

            Pointer* ptr = bex->left->value.ptr;
            var->value.any = ptr->pointsTo;
            var->value.typeKind = ptr->pointsToKind;

            return Err::OK;

        } else if (bex->base.opType == OP_MEMBER_SELECTION) {

            // we also may want to change the operator to
            // OP_DEREFERENCE_MEMEBER_SELECTION here if needed,
            // so its simpler to compile
            resolveMember(bex, var);
            return Err::OK;

        }

        // TODO:
        // validateImplicitCast(lDtype, rDtype);

        if (Type::basicTypes[lDtype].rank > Type::basicTypes[rDtype].rank) {
            inheritType(var, &bex->left->value);
        } else {
            inheritType(var, &bex->left->value);
        }

        return Err::OK;

    }

    Err::Err resolveResultType(FunctionCall* fex, Variable* var) {
        var->value.typeKind = fex->outArg->value.typeKind;
        return Err::OK;
    }

    Err::Err resolveTypes(Variable* var, Variable* target) {

        Err::Err err;

        Expression* ex = var->expression;
        if (!ex) {
            return Err::OK;
        }

        switch (ex->type) {

            case EXT_UNARY: {

                UnaryExpression* uex = (UnaryExpression*) ex;

                err = resolveTypes(uex->operand, target);
                if (err != Err::OK) return err;

                err = resolveResultType(uex, var);
                if (err != Err::OK) return err;

                break;

            }

            case EXT_BINARY: {

                BinaryExpression* bex = (BinaryExpression*) ex;

                err = resolveTypes(bex->left, target);
                if (err != Err::OK) return err;

                err = resolveTypes(bex->right, target);
                if (err != Err::OK) return err;

                err = resolveResultType(bex, var);
                if (err != Err::OK) return err;

                break;

            }

            case EXT_FUNCTION_CALL: {

                // we lookup the last argument in fcn
                // and check if we deal with vardic args
                // if so we lazily compute runtime types
                // so called 'any'
                // TODO : grammar
                FunctionCall* fex = (FunctionCall*) ex;
                Function* fcn = fex->fcn;

                int callArgCount = fex->inArgCount;
                int fixedCount = fcn->prototype.inArgCount;

                if (fixedCount > 0) {
                    VariableDefinition* lastArg = fcn->prototype.inArgs[fixedCount - 1];
                    if (lastArg->var->value.typeKind == Type::DT_MULTIPLE_TYPES) {
                        fixedCount--;
                    }
                }

                int i = 0;
                for (; i < fixedCount && i < callArgCount; i++) {
                    Variable* var = fex->inArgs[i];
                    err = resolveTypes(var, target);
                }

                for (; i < callArgCount; i++) {
                    Variable* var = fex->inArgs[i];
                    err = resolveTypes(var, target);
                    Runtime::getType(var);
                }

                resolveResultType(fex, var);

                break;

            }

            case EXT_SLICE: {

                Slice* slice = (Slice*) ex;

                err = resolveTypes(slice->bidx, target);
                if (err != Err::OK) return err;
                if (!isInt(slice->bidx->value.typeKind)) {
                    Logger::log(logErr, "TODO");
                    return Err::INVALID_DATA_TYPE;
                }

                err = resolveTypes(slice->eidx, target);
                if (err != Err::OK) return err;
                if (!isInt(slice->eidx->value.typeKind)) {
                    Logger::log(logErr, "TODO");
                    return Err::INVALID_DATA_TYPE;
                }

                break;

            }

            case EXT_STRING_INITIALIZATION: {

                StringInitialization* init = (StringInitialization*) ex;

                var->value.typeKind = Type::DT_STRING;

                return Err::OK;

            }

            case EXT_ARRAY_INITIALIZATION: {

                ArrayInitialization* init = (ArrayInitialization*) ex;

                Value* dominantVal = NULL;
                int dominantIdx = 0;
                bool isStatic = true;

                Variable** buffer = (Variable**) init->attributes;

                for (int i = 0; i < init->attributeCount; i++) {

                    Variable* arg = *(buffer + i);
                    err = resolveTypes(arg, target);
                    if (err != Err::OK) return err;

                    if (isStatic && !arg->value.hasValue) {
                        isStatic = false;
                    }

                    // we want to make array type of the most 'dominant' type
                    if (!dominantVal || Type::basicTypes[dominantVal->typeKind].rank < Type::basicTypes[arg->value.typeKind].rank) {
                        dominantVal = &arg->value;
                        dominantIdx = i;
                    }

                }

                if (isStatic) {
                    init->flags |= IS_CMP_TIME;
                }

                // now we check if we can cast all elments to the 'dominant' one
                for (int i = 0; i < init->attributeCount; i++) {
                    if (i == dominantIdx) continue;

                    Variable* arg = *(buffer + i);
                    err = applyImplicitCast(dominantVal, arg);
                    if (err != Err::OK) return err;

                }

                // TODO : for now we allocate new Array for each case
                var->value.typeKind = Type::DT_ARRAY;
                var->value.arr = Ast::Node::makeArray();
                var->value.arr->base.pointsToKind = dominantVal->typeKind;
                var->value.arr->base.pointsTo = dominantVal->any;
                var->value.arr->flags = IS_CMP_TIME;
                if (dominantVal->typeKind == Type::DT_POINTER || dominantVal->typeKind == Type::DT_ARRAY) {
                    var->value.arr->base.parentPointer = dominantVal->ptr->parentPointer;
                }

                Variable* len = Ast::Node::makeVariable();
                len->value.hasValue = 1;
                len->value.typeKind = Type::DT_U64;
                len->value.u64 = init->attributeCount;
                var->value.arr->length = len;

                return Err::OK;

            }

            case EXT_TYPE_INITIALIZATION: {

                TypeDefinition* td = (TypeDefinition*) target->value.any;
                TypeInitialization* init = (TypeInitialization*) ex;

                if (init->attributeCount > td->varCount) {
                    Logger::log(logErr, Err::str(Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH));
                    return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
                }

                if (td->base.type == NT_UNION && init->attributeCount > 1) {
                    Logger::log(logErr, "TODO : union initialization more than one attribute defined!");
                    return Err::UNEXPECTED_SYMBOL;
                }

                int i = 0;
                for (; i < init->attributeCount; i++) {

                    Variable* dest = td->vars[i];
                    Variable* src  = init->attributes[i];

                    err = resolveTypes(src, dest);
                    if (err != Err::OK) return err;

                    err = applyImplicitCast(&dest->value, src);
                    if (err != Err::OK) return err;

                }

                if (init->fillVar) {
                    for (; i < td->varCount; i++) {
                        Variable* dest = td->vars[i];

                        err = resolveTypes(init->fillVar, dest);
                        if (err != Err::OK) return err;

                        err = applyImplicitCast(&dest->value, init->fillVar);
                        if (err != Err::OK) return err;

                    }
                }

                var->value.typeKind = Type::DT_CUSTOM;
                var->value.any = (void*) td;

                return Err::OK;

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
            tmpNspace = Ast::Find::inScopeNamespace(sc, &name);

            if (!tmpNspace) {

                ErrorSet* tmpEset = Ast::Find::inScopeErrorSet(sc, &name);
                if (!tmpEset) {

                    // check implicit errors
                    // TODO

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

    Err::Err validateFunctionCalls(AstContext* ctx) {
        for (int i = 0; i < (int) ctx->reg->fcnCalls.size; i++) {
            const int err = validateFunctionCall((Variable*) DArray::get(&ctx->reg->fcnCalls, i));
            if (err < 0) return (Err::Err) err;
        }
    }

    // TODO : move to appropriate place
    // TODO : later use set for counts >= 20 or something
    bool checkUniqueNames(Variable** arr, uint32_t len) {
        // TODO : later use set for counts >= 20 or something
        for (int i = 0; i < len; i++) {
            Variable* var = arr[i];
            INamed* src = (INamed*) &var->name;

            for (int j = i; j < len; j++) {
                Variable* var = arr[i];
                INamed* dest = (INamed*) &var->name;
                if (cstrcmp(*src, *dest)) return false;
            }
        }

        return true;
    }

    bool checkUniqueNames(DArray::Container* arr) {
        return checkUniqueNames((Variable**) arr->buffer, arr->size);
    }

    Err::Err validateTypeDefinitions(AstContext* ctx) {

        for (int i = 0; i < ctx->reg->customDataTypes.size; i++) {

            TypeDefinition* const def = (TypeDefinition*) DArray::get(&ctx->reg->customDataTypes, i);
            const int isUnion = def->base.type == NT_UNION;

            if (!checkUniqueNames(def->vars, def->varCount)) {
                Logger::log(logErr, Err::str(Err::INVALID_ATTRIBUTE_NAME));
                return Err::INVALID_ATTRIBUTE_NAME;
            }

            for (int i = 0; i < def->varCount; i++) {
                Variable* const var = def->vars[i];

                if (isUnion && (var->expression || var->value.hasValue)) {
                    Logger::log(logErr, "Default values are not allowed within union initialization!", var->base.span, var->name.len);
                    return Err::INVALID_RVALUE;
                }
            }

        }

        return Err::OK;

    }

    Err::Err validateInitializations(AstContext* ctx) {

        for (int i = 0; i < ctx->reg->initializations.size; i++) {

            VariableDefinition* const varDef = (VariableDefinition*) DArray::get(&ctx->reg->initializations, i);
            Variable* var = varDef->var;

            const Value value = var->value;
            Type::Kind typeKindRef = var->value.typeKind;
            void* dtypeRef = var->value.any;

            void* dtype;
            // int typeKind = evaluateDataTypes(var, (TypeDefinition**) (&dtype), typeKindRef, (TypeDefinition*) (dtypeRef));
            Type::Kind typeKind = var->value.typeKind;
            if (typeKind < 0) return (Err::Err) typeKind;
            var->value = value;

            // validateCustomTypeInitialization(dtype, dtypeInit);
            Logger::mute = 1;
            const Err::Err err = validateImplicitCast(dtype, dtypeRef, (Type::Kind) typeKind, typeKindRef);
            Logger::mute = 0;
            if (err < 0) {
                if (typeKindRef == Type::DT_POINTER) {
                    Value* val = &((var->def) ? var->def->var->value : var->value);
                    const Err::Err err = validatePointerAssignment(val);
                    if (err < 0) return err;
                } else {
                    Logger::log(logErr, "TODO: Invalid type conversion!", var->base.span);
                    return err;
                }
            }

            if (typeKindRef == Type::DT_FUNCTION) {

                Variable* tmp;

                Function* fcn = findExactFunction(varDef->base.scope, (String*) &tmp->name, value.fcn);
                if (!fcn) {
                    Logger::log(logErr, "Function doesnt match pointer definition!", var->base.span, var->name.len);
                    return Err::NO_MATCHING_FUNCTION_FOUND;
                }

                tmp->value.fcn = &fcn->prototype;
                tmp->name.id = fcn->name.id;

            }

        }

    }

    Err::Err validateBranches(AstContext* ctx) {

        for (int i = 0; i < ctx->reg->branchExpressions.size; i++) {

            Variable* op = (Variable*) DArray::get(&ctx->reg->branchExpressions, i);
            // int dtype = evaluateDataTypes(op);
            Type::Kind dtype = op->value.typeKind;
            if (isInt(dtype) || dtype == Type::DT_POINTER) continue;

            Logger::log(logErr, Err::str(Err::INVALID_DATA_TYPE), op->base.span, 1);
            return Err::INVALID_DATA_TYPE;

        }

        return Err::OK;

    }

    Err::Err validateStatements(AstContext* ctx) {

        for (int i = 0; i < ctx->reg->statements.size; i++) {

            Statement* st = (Statement*) DArray::get(&ctx->reg->statements, i);
            Variable* op = st->operand;
            // int dtype = evaluateDataTypes(op);
            Type::Kind dtype = op->value.typeKind;
            if (dtype < 0) return (Err::Err) dtype;

        }

        return Err::OK;

    }

    Err::Err validateSwitchCases(AstContext* ctx) {

        Err::Err err;

        for (int i = 0; i < ctx->reg->switchCases.size; i++) {

            SwitchCase* const sw = (SwitchCase*) DArray::get(&ctx->reg->switchCases, i);

            // err = evaluateDataTypes(sw->switchExp);
            // if (err != Err::OK) return err;

            Type::Kind dtype = sw->switchExp->value.typeKind;
            if (dtype < 0) {
                Logger::log(logErr, "TODO error", sw->base.span, 1);
                return (Err::Err) dtype;
            }

            for (int i = 0; i < sw->caseExpCount; i++) {

                Variable* caseExp = sw->casesExp[i];
                // int dtype = evaluateDataTypes(caseExp);
                dtype = caseExp->value.typeKind;
                if (dtype < 0) {
                    Logger::log(logErr, "", sw->base.span, 1);
                    return (Err::Err) dtype;
                }

                dtype = caseExp->value.typeKind;// evaluate(caseExp);
                if (dtype < 0) return (Err::Err) dtype;

            }

        }

    }

    // used within validateTypeInitialization
    // think about better name
    Err::Err validateAttributeCast(Variable* var, Variable* attribute) {

        // Variable* op, TypeDefinition** customDtype, Type::Kind lvalueType, TypeDefinition* lvalueTypeDef
        const int id = attribute->name.id;
        char* const name = attribute->name.buff;
        const int nameLen = attribute->name.len;

        VariableDefinition* def = var->def;
        Type::Kind dtypeA = attribute->value.typeKind;
        // Type::Kind dtypeA = (Type::Kind) evaluateDataTypes(
        //    attribute,
        //    NULL,
        //    def ? def->var->value.typeKind : DT_UNDEFINED,
        //    def ? def->var->value.def : NULL
        //);

        attribute->name.id = id;
        attribute->name.buff = name;
        attribute->name.len = nameLen;

        Type::Kind dtypeB = var->value.typeKind;
        // Type::Kind dtypeB = (Type::Kind) evaluateDataTypes(var);

        if (!validateImplicitCast(dtypeB, dtypeA)) {
            return Err::INVALID_DATA_TYPE;
        }

        return Err::OK;

    }

    // assuming dtypeInit has at least one attribute
    // both TypeDefinition  has to be valid
    Err::Err validateTypeInitialization(TypeDefinition* dtype, TypeInitialization* dtypeInit) {

        Variable** attributes = (Variable**) dtypeInit->attributes;

        const int count = dtype->varCount;
        const int areNamed = dtypeInit->attributeCount == 0 || (attributes[0])->name.buff;

        if (count < dtypeInit->attributeCount) {
            Logger::log(logErr, Err::str(Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH), dtype->base.span, dtype->name.len, dtypeInit->attributeCount, dtype->varCount);
            return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
        }

        if (dtype->base.type == NT_UNION) {

            if (dtypeInit->attributeCount > 1) {
                Variable* var = attributes[1];
                Logger::log(logErr, "Only one attribute can be set while initializing union!", var->base.span, var->name.len);
                return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
            }

            if (dtypeInit->fillVar) {
                Variable* var = dtypeInit->fillVar;
                Logger::log(logErr, "Fill th rest option is not allowed while initializing union!", var->base.span, 2);
                return Err::TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH;
            }

            if (!areNamed) {
                Variable* var = attributes[0];
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

            for (int i = 0; i < dtypeInit->attributeCount; i++) {

                Variable* attribute = attributes[i];

                const int idx = Ast::Find::inArray(dtype->vars, dtype->varCount, (String*) &attribute->name, NULL);
                if (idx < 0) {
                    Logger::log(logErr, Err::str(Err::INVALID_ATTRIBUTE_NAME), attribute->base.span, attribute->name.len, attribute->name.len, attribute->name.buff);
                    return Err::INVALID_ATTRIBUTE_NAME;
                }

                Variable* var = dtype->vars[idx];

                dtypeInit->idxs[idx] = i;
                attribute->name.id = var->name.id;

                // typecheck
                // if (!attribute->expression) continue;

                const Err::Err err = validateAttributeCast(var, attribute);
                if (err < 0) return err;

            }

            if (dtypeInit->fillVar) {

                for (int i = 0; i < dtype->varCount; i++) {

                    Variable* attribute = dtype->vars[i];
                    if (dtypeInit->idxs[i] < 0) continue;

                    // typecheck
                    // if (!attribute->expression) continue;

                    const Err::Err err = validateAttributeCast(attribute, dtypeInit->fillVar);
                    if (err < 0) return err;

                }

            }

            return Err::OK;

        }

        for (int i = 0; i < dtypeInit->attributeCount; i++) {

            Variable* attribute = dtypeInit->attributes[i];
            Variable* var = dtype->vars[i];

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
        FunctionPrototype* fcn = fcnCall->fcn ? &fcnCall->fcn->prototype : fcnCall->fptr->value.fcn;
        const int fcnInCount = fcn->inArgCount;
        // const int fcnCallInCount = fcnCall->inArgs.size();

        VariableDefinition** fcnInArgs = fcn->inArgs;
        const int variableNumberOfArguments = fcnInCount > 0 && (fcnInArgs[fcnInCount - 1])->var->value.typeKind == Type::DT_MULTIPLE_TYPES;

        // note:
        //  array argument is parsed as two arguments (pointer and length) in definition

        int j = 0;
        for (int i = 0; i < fcnInCount - variableNumberOfArguments; i++) {

            if (j >= fcnCall->inArgCount) {
                Logger::log(logErr, Err::str(Err::NOT_ENOUGH_ARGUMENTS), fcnCallOp->base.span, fcnCall->name.len);
                return Err::NOT_ENOUGH_ARGUMENTS;
            }

            VariableDefinition* fcnVarDef = fcnInArgs[i];
            Variable* fcnVar = fcnVarDef->var;
            Variable* fcnCallVar = fcnCall->inArgs[j];

            int fcnCallVarDtype;
            if (fcnCallVar->expression) {
                // TODOD : TypeDefinition** customDtype, Type::Kind lvalueType, TypeDefinition* lvalueTypeDef
                // fcnCallVarDtype = evaluateDataTypes(fcnCallVar);
                fcnCallVarDtype = fcnCallVar->value.typeKind;
                if (fcnCallVarDtype < Err::OK) return (Err::Err) fcnCallVarDtype;
            } else {
                fcnCallVarDtype = fcnCallVar->value.typeKind;
            }

            if (fcnVar->value.typeKind == Type::DT_ARRAY) {

                if (!(fcnCallVar->value.typeKind == Type::DT_ARRAY)) {
                    Logger::log(logErr, Err::str(Err::ARRAY_EXPECTED), fcnCallVar->base.span, 1);
                    return Err::ARRAY_EXPECTED;
                }

                Variable* var = fcnCallVar;
                if (!(var->def) || !(var->def->var->value.arr)) {
                    Logger::log(logErr, "TODO error: array expected!", fcnCallVar->base.span, fcnCallVar->name.len);
                    return Err::ARRAY_EXPECTED;
                }

                //const Err::Err err = evaluateArrayLength(var);
                //if (err < 0) return err;

                fcnCall->inArgs[j] = var;

                /*
                Variable* lenVar = new Variable();
                const int err = evaluateArrayLength(var, lenVar);
                if (err < 0) return err;

                Value* val = new Value();
                val->arr

                var->value.arr->length = lenVar;
                var->value.arr->length->value.typeKind = DT_UINT_64;
                var->value.arr->length->flags |= IS_LENGTH;
                */
                /*
                Array* arr = var->def->var->value.arr;
                if (arr->flags & IS_ARRAY_LIST) {
                    fcnCall->inArgs.insert(fcnCall->inArgs.begin() + j + 1, arr->length);
                    i++;
                    j++;
                    continue;
                }

                // fcnVar->value.arr->length = arr->length;
                fcnCall->inArgs.insert(fcnCall->inArgs.begin() + j + 1, arr->length); // fcnCallVar->def->var->value.arr->length);
                // j++;
                */

            }

            if (!validateImplicitCast((Type::Kind) fcnCallVarDtype, fcnVar->value.typeKind)) {
                // error : cannot cast
                // TODO: Logger::log(logErr, Err::str(Err::INVALID_TYPE_CONVERSION), fcnCallVar->base.span, 1, (dataTypes + fcnVar->value.typeKind)->name, (dataTypes + fcnCallVarDtype)->name);
                return Err::INVALID_TYPE_CONVERSION;
            }

            j++;

        }

        const int fcnCallInCount = fcnCall->inArgCount;

        if (j < fcnCallInCount && !variableNumberOfArguments) {
            Logger::log(logErr, Err::str(Err::TOO_MANY_ARGUMENTS), fcnCallOp->base.span, fcnCall->name.len);
            return Err::TOO_MANY_ARGUMENTS;
        }

        for (; j < fcnCallInCount; j++) {

            Variable* var = fcnCall->inArgs[j];

            int err;
            if (var->def) {
                // TODO : ALLOC case for now

                Variable* tmp = var->def->var;
                const Value tmpValue = tmp->value;
                const Type::Kind tmpDtype = tmp->value.typeKind;

                err = var->value.typeKind;// evaluateDataTypes(var, NULL, tmp->value.typeKind, tmp->value.def);

                if (tmp->value.typeKind == Type::DT_CUSTOM) {
                    // seems like evaluateDataTypes does the same thing
                    // validateTypeInitialization(tmp->value.def, (TypeInitialization*) ((WrapperExpression*) (var->expression))->operand->expression);
                } else {
                    const Err::Err err = validateImplicitCast(var->value.any, tmpValue.any, var->value.typeKind, tmpValue.typeKind);
                    if (err < 0) return err;
                    // tmp->value.typeKind = tmpDtype;
                }

                tmp->value = tmpValue;

            } else {
                err = var->value.typeKind; // evaluateDataTypes(var);
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

    // TODO : naming could be better
    // TODO : use dest/src?
    Err::Err validateImplicitCast(const Type::Kind dtype, const Type::Kind dtypeRef) {

        const bool basicTypes = (dtype != Type::DT_VOID && dtypeRef != Type::DT_VOID) && (dtype <= Type::DT_F64 && dtypeRef <= Type::DT_F64);
        const bool arrayToPointer = (dtype == Type::DT_ARRAY && dtypeRef == Type::DT_POINTER);
        const bool pointerToArray = (dtypeRef == Type::DT_ARRAY && dtype == Type::DT_POINTER);
        const bool sliceToArray = dtype == Type::DT_SLICE && dtypeRef == Type::DT_ARRAY;
        // this case shouldnt ever happen except for 'printf', so we can just check it like this
        const bool stringToString = dtype == Type::DT_STRING && dtypeRef == Type::DT_STRING;

        if (basicTypes && dtype == dtypeRef ||
            dtype == Type::DT_POINTER && dtypeRef == Type::DT_POINTER
        ) {
            return (Err::Err) EXACT_MATCH;
        }

        const bool ans = ((basicTypes || arrayToPointer || pointerToArray || sliceToArray || stringToString));
        return (Err::Err) ((int64_t) ans - 1);

    }

    Err::Err validateImplicitCast(void* dtype, void* dtypeRef, Type::Kind typeKind, Type::Kind typeKindRef) {

        if (validateImplicitCast(typeKind, typeKindRef) >= 0) {
            return Err::OK;
        }

        if (typeKindRef == Type::DT_ARRAY && typeKind == Type::DT_STRING) {

            Array* arr = (Array*) dtypeRef;
            StringInitialization* str = (StringInitialization*) dtype;

            const int arrDtypeSize = Type::basicTypes[arr->base.pointsToKind].size;
            const int strDtypeSize = Type::basicTypes[str->wideType].size;

            if (arrDtypeSize < strDtypeSize) {
                Logger::log(logErr, "TODO error: ");
                return Err::INVALID_TYPE_CONVERSION;
            }

            if (validateImplicitCast(arr->base.pointsToKind, str->wideType) < 0) {
                Logger::log(logErr, "TODO error: ");
                return Err::INVALID_TYPE_CONVERSION;
            }

            if (strDtypeSize < arrDtypeSize) {
                Logger::log(logWrn, "TODO warning: One can use smaller dtype");
            }

            return Err::OK;

        } else if (typeKindRef == Type::DT_ARRAY && typeKind == Type::DT_ARRAY) {

            Array* arrRef = (Array*) dtypeRef;
            Array* arr = (Array*) dtype;

            int levelRef = 0;
            const Type::Kind arrDtypeRef = (Type::Kind) getFirstNonArrayDtype(arrRef, -1, &levelRef);

            const int maxLevel = levelRef;
            levelRef = 0;
            const Type::Kind arrDtype = (Type::Kind) getFirstNonArrayDtype(arr, maxLevel, &levelRef);

            return validateImplicitCast(arrDtype, arrDtypeRef);


        } else if (typeKindRef == Type::DT_ARRAY) {

            Array* arr = (Array*) dtypeRef;
            const Type::Kind arrDtype = (Type::Kind) getFirstNonArrayDtype(arr);

            return validateImplicitCast(typeKind, arrDtype);

        }

        Logger::log(logErr, "TODO : invalid type conversion!");
        return Err::INVALID_TYPE_CONVERSION;

    }

    #define GENERATE_TARGET_SWITCH(dest, src) \
    switch (dest) { \
        case Type::DT_I8:  val->i8  = (int8_t)  (src); break; \
        case Type::DT_U8:  val->u8  = (uint8_t) (src); break; \
        case Type::DT_I16: val->i16 = (int16_t) (src); break; \
        case Type::DT_U16: val->u16 = (uint16_t)(src); break; \
        case Type::DT_I32: val->i32 = (int32_t) (src); break; \
        case Type::DT_U32: val->u32 = (uint32_t)(src); break; \
        case Type::DT_I64: val->i64 = (int64_t) (src); break; \
        case Type::DT_U64: val->u64 = (uint64_t)(src); break; \
        case Type::DT_F32: val->f32 = (float)   (src); break; \
        case Type::DT_F64: val->f64 = (double)  (src); break; \
        default: { \
            Logger::log(logErr, Err::str(Err::UNEXPECTED_ERROR)); \
            exit(213); \
        } \
    }

    void castLiteral(Value* val, Type::Kind toDtype) {

        if (val->typeKind == toDtype) return;

        switch (val->typeKind) {

            case Type::DT_U8: {
                uint8_t src = val->u8;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case Type::DT_U16: {
                uint16_t src = val->u16;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case Type::DT_U32: {
                uint32_t src = val->u32;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case Type::DT_U64: {
                uint64_t src = val->u64;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case Type::DT_I8: {
                int8_t src = val->i8;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case Type::DT_I16: {
                int16_t src = val->i16;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case Type::DT_I32: {
                int32_t src = val->i32;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case Type::DT_I64: {
                int64_t src = val->i64;
                GENERATE_TARGET_SWITCH(toDtype, src);
                break;
            }

            case Type::DT_F32: {
                float src = val->f32;
                GENERATE_TARGET_SWITCH(toDtype, src)
                break;
            }

            case Type::DT_F64: {
                double src = val->f64;
                GENERATE_TARGET_SWITCH(toDtype, src)
                break;
            }

            default: {
                // TODO
                Logger::log(logErr, Err::str(Err::UNEXPECTED_ERROR));
                exit(123);
                break;
            }

        }

        val->typeKind = toDtype;

    }

    bool isBasicDtype(Type::Kind dtype) {
        return (dtype > Type::DT_VOID && dtype <= Type::DT_F64);
    }



   // ================
   //  MISCELLANEOUS

    // TODO
    int computeSizeOfDataType(TypeDefinition* const def) {

        if (def->typeInfo->base.size > 0) return def->typeInfo->base.size;

        const int isUnion = def->base.type == NT_UNION;

        int accSize = 0;
        for (int i = 0; i < def->varCount; i++) {

            Variable* const var = def->vars[i];

            int size = 0;
            switch (var->value.typeKind) {

                case Type::DT_CUSTOM: {
                    size = computeSizeOfDataType(var->value.def);
                    break;
                }

                case Type::DT_ARRAY: {

                    Array* const arr = var->value.arr;
                    // evaluate(arr->length);

                    if (!(arr->length->value.hasValue)) {
                        Logger::log(logErr, "Was unable to compute array length while computing size of the %.*s!", var->base.span, var->name.len, def->name.len, def->name.buff);
                        return Err::COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED;
                    }

                    int chunkSize = 0;
                    if (arr->base.pointsToKind == Type::DT_CUSTOM) {
                        chunkSize = computeSizeOfDataType((TypeDefinition*) arr->base.pointsTo);
                    } else {
                        chunkSize = Type::basicTypes[arr->base.pointsToKind].size;
                    }

                    size = chunkSize * arr->length->value.i64;

                    break;
                }

                default: {
                    size = Type::basicTypes[var->def->var->value.typeKind].size;
                    break;
                }

            }

            if (isUnion) accSize = size > accSize ? size : accSize;
            else accSize += size;

        }

        def->typeInfo->base.size = accSize;
        return accSize;

    }

    // level has to be 0, if its output matters
    int getFirstNonArrayDtype(Array* arr, const int maxLevel, int* level) {

        const int dtype = arr->base.pointsToKind;

        if (maxLevel > 0 && *level >= maxLevel) return dtype;
        if (dtype != Type::DT_ARRAY) return dtype;

        if (level) *level = *level + 1;
        return getFirstNonArrayDtype((Array*) arr->base.pointsTo, maxLevel, level);

    }


    Variable* findErrorInErrorSet(ErrorSet* eset, QualifiedName* var) {

        Namespace* tmpNspace;

        int i = 0;
        const int len = var->pathSize;

        for (i = 0; i < len; i++) {

            INamed* nm = var->path + i;
            Variable* tmp = Ast::Find::inArray(eset->vars, eset->varCount, nm);
            if (!tmp) return NULL;

            eset = tmp->value.err;

        }

        return Ast::Find::inArray(eset->vars, eset->varCount, (String*) &var);

    }

    // scope cannot be NULL
    Variable* findDefinition(Scope* scope, QualifiedName* const inVar, int idx) {

        const String name = { inVar->buff, (uint64_t) inVar->len };
        // int idx = inVar->parentIdx;

        while (scope) {

            SyntaxNode* node = Ast::Find::inArray(scope->definitions, scope->definitionCount, (String*) &name);
            if (node) {

                if (node->definitionIdx < idx) {
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

                // TODO
                /*
                for (int i = 0; i < scope->usings.size; i++) {

                    Using* usng = (Using*) DArray::get(&scope->usings.base, i);
                    if (usng->var->type != NT_FUNCTION) continue;

                    Variable* err = findErrorInErrorSet(((Function*) (usng->var))->errorSet, inVar);
                    if (err) return err;

                }
                */

            }

            idx = scope->base.definitionIdx;
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
                Logger::log(logErr, Err::str(Err::SYMBOL_ALREADY_DEFINED), span, named->len);
                return Err::SYMBOL_ALREADY_DEFINED;
            }
            //item->flags |= IS_UNIQUE;

        }

        return Err::OK;

    }

    int isUnique(Scope* sc, INamed* named, Span* span) {

        // TODO
        /*
        if (isUniqueInCollection(&sc->defs, named, span, getMemberOffset(Variable, name)) < 0) return Err::SYMBOL_ALREADY_DEFINED;
        if (isUniqueInCollection(&sc->fcns, named, span, getMemberOffset(Function, name)) < 0) return Err::SYMBOL_ALREADY_DEFINED;
        if (isUniqueInCollection(&sc->customDataTypes, named, span, getMemberOffset(TypeDefinition, name)) < 0) return Err::SYMBOL_ALREADY_DEFINED;
        if (isUniqueInCollection(&sc->labels, named, span, getMemberOffset(Label, name)) < 0) return Err::SYMBOL_ALREADY_DEFINED;
        */

        return Err::OK;

    }

    int match(FunctionPrototype* const fptrA, FunctionPrototype* const fptrB) {

        if (fptrA->inArgCount != fptrB->inArgCount) return 0;

        for (int i = 0; i < fptrA->inArgCount; i++) {
            const Type::Kind defA = fptrA->inArgs[i]->var->value.typeKind;
            const Type::Kind defB = fptrB->inArgs[i]->var->value.typeKind;
            if (defA != defB) return 0;
        }

        return 1;

    }

    Function* findExactFunction(Scope* scope, INamed* const name, FunctionPrototype* const fptr) {

        while (scope) {

            if (scope->base.type != NT_NAMESPACE) {
                continue;
            }

            Namespace* nspace = (Namespace*) scope;
            Function* fcns    = ((Function*) nspace->fcns);

            for (int i = 0; i < nspace->fcnCount; i++) {
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

            if (scope->base.type != NT_NAMESPACE) {
                continue;
            }

            Namespace* const nspace = (Namespace*) scope;
            Function** const fcns   = nspace->fcns;

            for (int i = 0; i < nspace->fcnCount; i++) {

                Function* fcn = fcns[i];
                const String fcnName = { fcn->name.buff, fcn->name.len };

                if (!cstrcmp(callName, fcnName)) {
                    continue;
                }

                const int fcnInCnt = fcn->prototype.inArgCount;
                const int callInCnt = call->inArgCount;
                const int lastArgDtype = fcnInCnt > 0 ? 0 : 0;// TODO (fcns + i)->prototype.inArgs[fcnInCnt - 1]->var->value.typeKind : DT_UNDEFINED;

                if (
                    fcnInCnt == callInCnt ||
                    (fcnInCnt < callInCnt && lastArgDtype == Type::DT_MULTIPLE_TYPES)
                ) {
                    FunctionScore tmp = { fcn, 0 };
                    DArray::push(&fCandidates, &tmp);
                }

            }

            scope = nspace->scope.base.scope;

        }

    }

    int findClosestFunction(Variable* callOp, Function** outFcn) {

        Scope* scope = callOp->base.scope;
        FunctionCall* call = (FunctionCall*)callOp->expression;
        const int callInCnt = call->inArgCount;

        findCandidateFunctions(scope, call);

        for (int j = 0; j < fCandidates.size; j++) {

            Function* fcn = ((FunctionScore*) DArray::get(&fCandidates, j))->fcn;
            int score = (fcn->prototype.inArgCount == 0) ? 100 : 0;
            const int fcnInCnt = fcn->prototype.inArgCount;

            int k = 0;
            for (int i = 0; i < fcn->prototype.inArgCount; i++) {

                if (k >= callInCnt) {
                    score = 0;
                    break;
                }

                Variable* fArg = fcn->prototype.inArgs[i]->var;
                Variable* cArg = call->inArgs[k];

                k++;

                const int fDtype = fArg->value.typeKind;
                const int cDtype = cArg->value.typeKind;// evaluateDataTypes(cArg);

                if (
                    (fDtype == Type::DT_CUSTOM && cDtype != Type::DT_CUSTOM) ||
                    (fDtype != Type::DT_CUSTOM && cDtype == Type::DT_CUSTOM)
                    ) {
                    score = 0;
                    break;
                }
                else if (fDtype == Type::DT_CUSTOM) {
                    if (fArg->value.def != cArg->value.def) {
                        score = 0;
                        break;
                    }
                }

                if (fDtype == Type::DT_MULTIPLE_TYPES) {
                    break;
                }

                if (fDtype == cDtype) {
                    score += FOS_EXACT_MATCH;
                    continue;
                }

                if (Type::isSignedInt(cDtype) && Type::isSignedInt(fDtype)) {
                    if (cDtype < fDtype) {
                        score += FOS_PROMOTION;
                    }
                    else {
                        score += FOS_SIZE_DECREASE;
                    }
                    continue;
                }

                if (Type::isUnsignedInt(cDtype) && Type::isUnsignedInt(fDtype)) {
                    if (cDtype < fDtype) {
                        score += FOS_PROMOTION;
                    }
                    else {
                        score += FOS_SIZE_DECREASE;
                    }
                    continue;
                }

                if (
                    Type::isSignedInt(cDtype) && Type::isUnsignedInt(fDtype) ||
                    Type::isSignedInt(cDtype) && Type::isUnsignedInt(fDtype)
                ) {

                    if ((cDtype - fDtype <= Type::DT_U64 - Type::DT_I64)) {
                        score += FOS_SIGN_CHANGE;
                    } else {
                        score += FOS_SIZE_DECREASE;
                    }

                    continue;

                }

                if (Type::isFloat(cDtype) && Type::isFloat(fDtype)) {
                    if ((cDtype - fDtype <= Type::DT_U64 - Type::DT_I64)) {
                        score += FOS_PROMOTION;
                    }
                    else {
                        score += FOS_SIZE_DECREASE;
                    }
                    continue;
                }

                if (Type::isInt(cDtype) && Type::isFloat(fDtype)) {
                    score += FOS_SIGN_CHANGE;
                    continue;
                }

                if (Type::isFloat(cDtype) && Type::isInt(fDtype)) {
                    score += FOS_SIZE_DECREASE;
                    continue;
                }

                if (validateImplicitCast((Type::Kind)cDtype, (Type::Kind)fDtype)) {
                    score += FOS_IMPLICIT_CAST;
                    continue;
                }

                if (fDtype == Type::DT_ARRAY && cDtype == Type::DT_STRING) {
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
                //Logger::log(logErr, Err::str(Err::NO_MATCHING_FUNCTION_FOUND));
                return Err::NO_MATCHING_FUNCTION_FOUND;
            }

            //Logger::log(logErr, Err::str(Err::MORE_THAN_ONE_OVERLOAD_MATCH), callOp->span, 1);
            return Err::MORE_THAN_ONE_OVERLOAD_MATCH;

        }

        return Err::OK;

    }

}
