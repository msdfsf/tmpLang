#include "data_types.h"
#include "globals.h"
#include "operators.h"
#include "translator.h"
#include "syntax.h"
#include "ansi_colors.h"

#include <cstdio>
#include <cstdint>

void debugPrintNode(FILE* file, int level, SyntaxNode* const node, Variable* lvalue);
void debugPrintExpression(FILE* file, int level, Expression* const expr, Variable* lvalue);

static void printIndent(FILE* file, int level) {
    for (int i = 0; i < level; ++i) fprintf(file, "  ");
}

static void printAttributeName(FILE* file, int level, const char* name) {
    printIndent(file, level);
    fprintf(file, AC_BOLD_CYAN "%s: " AC_RESET, name);
}

static void printName(FILE* file, INamed* name, bool newLine = false) {
    fprintf(file, "%.*s%s", (int) name->len, name->buff, newLine ? "\n" : "");
}

static void printQualifiedName(FILE* file, QualifiedName* name, bool newLine = false) {
    if (!name || !file) return;

    for (uint16_t i = 0; i < name->pathSize; i++) {
        String* part = &name->path[i];

        printName(file, part);
        if (i < name->pathSize - 1) {
            fprintf(file, "::");
        }
    }

    printName(file, (INamed*) name, newLine);
}

static void printName(FILE* file, INamedEx* name, bool newLine = false) {
    printName(file, (INamed*) name, newLine);
}

static void printVariableDefinition(FILE* file, VariableDefinition* def) {
    if (!def) {
        fprintf(file, "void");
        return;
    }

    if (def->dtype) {
        printQualifiedName(file, def->dtype);
    } else if (def->var) {
        fprintf(file, "%s", Type::str(def->var->value.typeKind));
    } else {
        fprintf(file, "unknown_type");
    }

    if (def->lastPtr) {
        fprintf(file, "*");
    }

    // 3. Print Variable Name
    if (def->var) {
        fprintf(file, " ");
        printQualifiedName(file, &def->var->name);
    }
}

static void printFunctionPrototype(FILE* file, FunctionPrototype* proto) {
    if (!file || !proto) return;

    if (proto->outArg) {
        printVariableDefinition(file, proto->outArg);
    } else {
        fprintf(file, "void");
    }

    fprintf(file, "(");

    for (uint32_t i = 0; i < proto->inArgCount; i++) {
        if (proto->inArgs && proto->inArgs[i]) {
            printVariableDefinition(file, proto->inArgs[i]);
        }

        // Print comma separator
        if (i < proto->inArgCount - 1) {
            fprintf(file, ", ");
        }
    }

    fprintf(file, ");\n");
}

static void printValue(FILE* file, Value* val) {
    if (!file || !val) return;

    if (!val->hasValue) {
        fprintf(file, "<no value>");
        return;
    }

    switch (val->typeKind) {
        case Type::DT_I8:
            fprintf(file, "%hhi", val->i8);
            break;
        case Type::DT_I16:
            fprintf(file, "%hi", val->i16);
            break;
        case Type::DT_I32:
            fprintf(file, "%i", val->i32);
            break;
        case Type::DT_I64:
            fprintf(file, "%lli", (long long) val->i64);
            break;
        case Type::DT_U8:
            fprintf(file, "%hhu", val->u8);
            break;
        case Type::DT_U16:
            fprintf(file, "%hu", val->u16);
            break;
        case Type::DT_U32:
            fprintf(file, "%u", val->u32);
            break;
        case Type::DT_U64:
            fprintf(file, "%llu", (unsigned long long) val->u64);
            break;
        case Type::DT_F32:
            fprintf(file, "%g", (double) val->f32);
            break;
        case Type::DT_F64:
            fprintf(file, "%g", val->f64);
            break;
        case Type::DT_POINTER:
            fprintf(file, "ptr(%p)", val->ptr);
            break;
        case Type::DT_ARRAY:
            fprintf(file, "array(%p)", val->arr);
            break;
        case Type::DT_SLICE:
            fprintf(file, "slice(%p)", val->slc);
            break;
        case Type::DT_STRING:
            if (val->str) {
                String* s = (String*) val->str;
                fprintf(file, "\"%.*s\"", (int) s->len, s->buff);
            } else {
                fprintf(file, "null");
            }
            break;
        case Type::DT_ENUM:
            fprintf(file, "enum_val");
            break;
        case Type::DT_ERROR:
            fprintf(file, "error_set");
            break;
        case Type::DT_FUNCTION:
            if (val->fcn) {
                printFunctionPrototype(file, val->fcn);
            }
            break;
        default:
            fprintf(file, "unknown_value");
            break;
    }
}

void debugPrintNode(FILE* file, int level, SyntaxNode* const node, Variable* lvalue) {
    if (!node) return;

    printIndent(file, level);
    fprintf(file, "[" AC_BOLD_MAGENTA "%s" AC_RESET "] (Flags: %llu)\n", Ast::Node::str(node->type), node->flags);

    switch (node->type) {
        case NT_SCOPE: {
            Scope* scope = (Scope*) node;

            printAttributeName(file, level, "Children Count");
            fprintf(file, "%u\n", scope->childrenCount);

            printAttributeName(file, level, "Definition Count");
            fprintf(file, "%u\n", scope->definitionCount);

            for (uint32_t i = 0; i < scope->childrenCount; i++) {
                debugPrintNode(file, level + 1, scope->children[i], NULL);
            }

            break;
        }

        case NT_VARIABLE: {
            Variable* var = (Variable*) node;

            printAttributeName(file, level, "Name");
            printQualifiedName(file, &var->name, true);

            if (var->value.hasValue) {
                printAttributeName(file, level, "Value");
                printValue(file, &var->value);
                fprintf(file, "\n");
            }

            if (var->expression) {
                debugPrintExpression(file, level + 1, var->expression, var);
            }

            break;
        }

        case NT_VARIABLE_DEFINITION: {
            VariableDefinition* def = (VariableDefinition*) node;

            printAttributeName(file, level, "Name");
            printQualifiedName(file, &def->var->name, true);

            printAttributeName(file, level, "Type Name");
            printQualifiedName(file, def->dtype, true);

            if (def->var) debugPrintNode(file, level + 1, (SyntaxNode*)def->var, NULL);

            break;
        }

        case NT_VARIABLE_ASSIGNMENT: {
            VariableAssignment* ass = (VariableAssignment*)node;
            debugPrintNode(file, level + 1, (SyntaxNode*) ass->lvar, NULL);
            debugPrintNode(file, level + 1, (SyntaxNode*) ass->rvar, NULL);
            break;
        }

        case NT_FUNCTION: {
            Function* fcn = (Function*) node;

            printAttributeName(file, level, "Name");
            printName(file, &fcn->name, true);

            printIndent(file, level);
            printFunctionPrototype(file, &fcn->prototype);

            if (fcn->bodyScope) debugPrintNode(file, level + 1, (SyntaxNode*) fcn->bodyScope, NULL);
            break;
        }

        case NT_BRANCH: {
            Branch* b = (Branch*)node;
            fprintf(file, " (If/Else Chain: %u scopes)\n", b->scopeCount);
            for (uint32_t i = 0; i < b->scopeCount; i++) {
                if (i < b->expressionCount) debugPrintNode(file, level + 1, (SyntaxNode*)b->expressions[i], NULL);
                debugPrintNode(file, level + 1, (SyntaxNode*)b->scopes[i], NULL);
            }
            break;
        }

        case NT_SWITCH_CASE: {
            SwitchCase* sc = (SwitchCase*)node;
            fprintf(file, " (Switch)\n");
            debugPrintNode(file, level + 1, (SyntaxNode*)sc->switchExp, NULL);
            for (uint32_t i = 0; i < sc->caseCount; i++) {
                debugPrintNode(file, level + 1, (SyntaxNode*)sc->cases[i], NULL);
            }
            if (sc->elseCase) debugPrintNode(file, level + 1, (SyntaxNode*)sc->elseCase, NULL);
            break;
        }

        case NT_WHILE_LOOP: {
            WhileLoop* wl = (WhileLoop*)node;
            fprintf(file, " (While)\n");
            debugPrintNode(file, level + 1, (SyntaxNode*)wl->expression, NULL);
            debugPrintNode(file, level + 1, (SyntaxNode*)wl->bodyScope, NULL);
            break;
        }

        case NT_LOOP: {
            Loop* l = (Loop*)node;
            fprintf(file, " (For-In/Loop)\n");
            if (l->array) debugPrintNode(file, level + 1, (SyntaxNode*)l->array, NULL);
            debugPrintNode(file, level + 1, (SyntaxNode*)l->bodyScope, NULL);
            break;
        }

        case NT_RETURN_STATEMENT: {
            ReturnStatement* rs = (ReturnStatement*)node;
            fprintf(file, " (Return)\n");
            if (rs->var) debugPrintNode(file, level + 1, (SyntaxNode*)rs->var, NULL);
            break;
        }

        case NT_TYPE_DEFINITION: {
            TypeDefinition* td = (TypeDefinition*) node;

            printAttributeName(file, level, "Name");
            printName(file, &td->name, true);

            printAttributeName(file, level, "Member Count");
            fprintf(file, "%u\n", td->varCount, true);

            for (uint32_t i = 0; i < td->varCount; i++) {
                debugPrintNode(file, level + 1, (SyntaxNode*)td->vars[i], NULL);
            }

            break;
        }

        case NT_NAMESPACE: {
            Namespace* ns = (Namespace*)node;
            fprintf(file, " (Namespace)\n");
            debugPrintNode(file, level + 1, (SyntaxNode*)&ns->scope, NULL);
            break;
        }

        case NT_IMPORT: {
            ImportStatement* imp = (ImportStatement*)node;
            fprintf(file, " (Import: %s)\n", (char*)imp->fname);
            break;
        }

        case NT_BREAK_STATEMENT: fprintf(file, " (Break)\n"); break;
        case NT_CONTINUE_STATEMENT: fprintf(file, " (Continue)\n"); break;

        default:
            fprintf(file, " (Unimplemented Node Detail)\n");
            break;
    }

}

void debugPrintExpression(FILE* file, int level, Expression* const exp, Variable* lvalue) {
    if (!exp) return;

    printIndent(file, level);
    fprintf(file, AC_BOLD_GREEN "[%s] " AC_RESET, Ast::Node::str(exp->type));

    switch ((ExpressionType) exp->type) {
        case EXT_BINARY: {
            BinaryExpression* bex = (BinaryExpression*) exp;

            printAttributeName(file, level, "Operator");
            fprintf(file, "%s", OperatorToStr(bex->base.opType));

            debugPrintNode(file, level + 1, (SyntaxNode*) bex->left, NULL);
            debugPrintNode(file, level + 1, (SyntaxNode*) bex->right, NULL);
            break;
        }

        case EXT_UNARY: {
            UnaryExpression* uex = (UnaryExpression*) exp;

            printAttributeName(file, 0, "Operator");
            fprintf(file, "%s\n", OperatorToStr(uex->base.opType));

            debugPrintNode(file, level + 1, (SyntaxNode*) uex->operand, NULL);
            break;
        }

        case EXT_TERNARY: {
            TernaryExpression* ter = (TernaryExpression*) exp;

            fprintf(file, "TODO\n");

            debugPrintNode(file, level + 1, (SyntaxNode*)ter->condition, NULL);
            debugPrintNode(file, level + 1, (SyntaxNode*)ter->trueExp, NULL);
            debugPrintNode(file, level + 1, (SyntaxNode*)ter->falseExp, NULL);
            break;
        }

        case EXT_FUNCTION_CALL: {
            FunctionCall* call = (FunctionCall*) exp;

            printAttributeName(file, level, "Function");
            printQualifiedName(file, &call->name, true);

            for (uint32_t i = 0; i < call->inArgCount; i++) {
                debugPrintNode(file, level + 1, (SyntaxNode*)call->inArgs[i], NULL);
            }

            break;
        }

        case EXT_TYPE_INITIALIZATION: {
            TypeInitialization* ti = (TypeInitialization*) exp;
            fprintf(file, "[Struct Init: %d attrs]\n", ti->attributeCount);
            for (uint32_t i = 0; i < ti->attributeCount; i++) {
                debugPrintNode(file, level + 1, (SyntaxNode*)ti->attributes[i], NULL);
            }
            break;
        }

        case EXT_STRING_INITIALIZATION: {
            StringInitialization* si = (StringInitialization*) exp;
            fprintf(file, "[String: \"%s\"]\n", (char*)si->rawStr);
            break;
        }

        case EXT_ARRAY_INITIALIZATION: {
            ArrayInitialization* ai = (ArrayInitialization*) exp;
            fprintf(file, "[Array Init: %d elements]\n", ai->attributeCount);
            for (uint32_t i = 0; i < ai->attributeCount; i++) {
                debugPrintNode(file, level + 1, (SyntaxNode*)ai->attributes[i], NULL);
            }
            break;
        }

        case EXT_SLICE: {
            Slice* sl = (Slice*) exp;
            fprintf(file, "[Slice]\n");
            debugPrintNode(file, level + 1, (SyntaxNode*)sl->arr, NULL);
            debugPrintNode(file, level + 1, (SyntaxNode*)sl->bidx, NULL);
            debugPrintNode(file, level + 1, (SyntaxNode*)sl->eidx, NULL);
            break;
        }

        case EXT_CAST: {
            Cast* c = (Cast*) exp;
            fprintf(file, "[Cast to Kind: %d]\n", c->target);
            debugPrintNode(file, level + 1, (SyntaxNode*)c->operand, NULL);
            break;
        }

        case EXT_ALLOC: {
            Alloc* al = (Alloc*) exp;
            fprintf(file, "[Alloc]\n");
            debugPrintNode(file, level + 1, (SyntaxNode*)al->def, NULL);
            break;
        }

        case EXT_FREE: {
            Free* fr = (Free*) exp;
            fprintf(file, "[Free]\n");
            debugPrintNode(file, level + 1, (SyntaxNode*)fr->var, NULL);
            break;
        }

        case EXT_GET_LENGTH:
        case EXT_GET_SIZE: {
            GetLength* gs = (GetLength*) exp; // Shared layout with GetSize
            fprintf(file, "[%s]\n", exp->type == EXT_GET_LENGTH ? "Len" : "Size");
            debugPrintNode(file, level + 1, (SyntaxNode*)gs->arr, NULL);
            break;
        }

        case EXT_CATCH: {
            Catch* ct = (Catch*) exp;
            fprintf(file, "[Catch]\n");
            debugPrintExpression(file, level + 1, (Expression*)ct->call, NULL);
            if (ct->scope) debugPrintNode(file, level + 1, (SyntaxNode*)ct->scope, NULL);
            break;
        }

        default:
            fprintf(file, "[Unhandled Expression Type]\n");
            break;
    }
}



Translator translatorDebug = {
    .mainFile  = stdout,
    .debugInfo = 1,      // debugInfo level

    .init = [](char* const dirName) {
        printf("--- Initializing Debug Print for: %s ---\n", dirName);
    },

    .printNode = debugPrintNode,
    .printExpression = debugPrintExpression,

    .printForeignCode = []() {
        printf("/* Foreign Code Block */\n");
    },

    .exit = []() {
        printf("--- End of AST Dump ---\n");
    }
};
