// #pragma once

#include "itself_console_translator.h"
#include "lexer.h"


#define CSP_MAX_TAB_LEVEL 4
#define CSP_GEN_TAB(str, n) for (int i = 0; i < (n); i++) str[i] = '\t'; str[(n)] = '\0';



static void printScope(FILE* file, int level, Scope* const node, Variable* lvalue = NULL);
static void printVariableDefinition(FILE* file, int level, VariableDefinition* const node, Variable* lvalue = NULL);
static void printVariableAssignment(FILE* file, int level, VariableAssignment* const node, Variable* lvalue = NULL);
static void printTypeDefinition(FILE* file, int level, TypeDefinition* const node, Variable* lvalue = NULL);
static void printUnion(FILE* file, int level, Union* const node, Variable* lvalue = NULL);
static void printErrorSet(FILE* file, int level, ErrorSet* const node, Variable* lvalue = NULL);
static void printSwitchCase(FILE* file, int level, SwitchCase* const node, Variable* lvalue = NULL);
static void printLoop(FILE* file, int level, Loop* const node, Variable* lvalue = NULL);
static void printReturnStatement(FILE* file, int level, ReturnStatement* const node, Variable* lvalue = NULL);
static void printContinueStatement(FILE* file, int level, ContinueStatement* const node, Variable* lvalue = NULL);
static void printBreakStatement(FILE* file, int level, BreakStatement* const node, Variable* lvalue = NULL);
static void printGotoStatement(FILE* file, int level, GotoStatement* const node, Variable* lvalue = NULL);
static void printLabel(FILE* file, int level, Label* const node, Variable* lvalue = NULL);
static void printNamespace(FILE* file, int level, Namespace* const node, Variable* lvalue = NULL);
static void printStatement(FILE* file, int level, Statement* const node, Variable* lvalue = NULL);
static void printTypeInitialization(FILE* file, int level, TypeInitialization* const node, Variable* lvalue = NULL);
static void printEnumerator(FILE* file, int level, Enumerator* const node, Variable* lvalue = NULL);
static void printVariable(FILE* file, int level, Variable* const node, Variable* lvalue = NULL);
static void printFunction(FILE* file, int level, Function* const node, Variable* lvalue = NULL);
static void printBranch(FILE* file, int level, Branch* const node, Variable* lvalue = NULL);
static void printWhileLoop(FILE* file, int level, WhileLoop* const node, Variable* lvalue = NULL);
static void printForLoop(FILE* file, int level, ForLoop* const node, Variable* lvalue = NULL);
static void printExpression(FILE* file, int level, Expression* const node, Variable* lvalue = NULL);
static void printWrapperExpression(FILE* file, int level, WrapperExpression* const node, Variable* lvalue = NULL);
static void printConstExpression(FILE* file, int level, ConstExpression* const node, Variable* lvalue = NULL);
static void printCatchExpression(FILE * file, int level, Catch* const node, Variable * lvalue);
static void printOperatorExpression(FILE* file, int level, OperatorExpression* const node, Variable* lvalue = NULL);
static void printUnaryExpression(FILE* file, int level, UnaryExpression* const node, Variable* lvalue = NULL);
static void printBinaryExpression(FILE* file, int level, BinaryExpression* const node, Variable* lvalue = NULL);
static void printTernaryExpression(FILE* file, int level, TernaryExpression* const node, Variable* lvalue = NULL);
static void printFunctionCall(FILE* file, int level, FunctionCall* const node, Variable* lvalue = NULL);
static void printOperand(FILE* file, int level, Operand* const node, Variable* lvalue = NULL);
static void printUnaryOperator(FILE* file, int level, UnaryOperator* const node, Variable* lvalue = NULL);
static void printBinaryOperator(FILE* file, int level, BinaryOperator* const node, Variable* lvalue = NULL);
static void printTernaryOperator(FILE* file, int level, TernaryOperator* const node, Variable* lvalue = NULL);

static void printStringInitialization(FILE* file, int level, StringInitialization* const node, Variable* lvalue);
static void printArrayInitialization(FILE* file, int level, ArrayInitialization* const node, Variable* lvalue);











void printForeignLangFunction(FILE* file, Function* node) {

}



void init (char* const dirName) {

}

void print(FILE* file, int level, SyntaxNode* node, Variable* lvalue = NULL) {
    
    switch (node->type) {
        
        case NT_SCOPE :
            printScope(file, level, (Scope*) node, lvalue);
            break;
        case NT_VARIABLE_DEFINITION :
            printVariableDefinition(file, level, (VariableDefinition*) node, lvalue);
            break;
        case NT_VARIABLE_ASSIGNMENT :
            printVariableAssignment(file, level, (VariableAssignment*) node, lvalue);
            break;
        case NT_TYPE_DEFINITION :
            printTypeDefinition(file, level, (TypeDefinition*) node, lvalue);
            break;
        case NT_TYPE_INITIALIZATION :
            printTypeInitialization(file, level, (TypeInitialization*) node, lvalue);
            break;
        case NT_UNION :
            printUnion(file, level, (Union*) node, lvalue);
            break;
        case NT_ERROR :
            printErrorSet(file, level, (ErrorSet*) node, lvalue);
            break;
        case NT_ENUMERATOR :
            printEnumerator(file, level, (Enumerator*) node, lvalue);
            break;
        case NT_VARIABLE :
            printVariable(file, level, (Variable*) node, lvalue);
            break;
        case NT_FUNCTION :
            printFunction(file, level, (Function*) node, lvalue);
            break;
        case NT_BRANCH :
            printBranch(file, level, (Branch*) node, lvalue);
            break;
        case NT_SWITCH_CASE :
            printSwitchCase(file, level, (SwitchCase*) node, lvalue);
            break;
        case NT_WHILE_LOOP :
            printWhileLoop(file, level, (WhileLoop*) node, lvalue);
            break;
        case NT_FOR_LOOP :
            printForLoop(file, level, (ForLoop*) node, lvalue);
            break;
        case NT_LOOP :
            printLoop(file, level, (Loop*) node, lvalue);
            break;
        case NT_RETURN_STATEMENT :
            printReturnStatement(file, level, (ReturnStatement*) node, lvalue);
            break;
        case NT_CONTINUE_STATEMENT :
            printContinueStatement(file, level, (ContinueStatement*) node, lvalue);
            break;
        case NT_BREAK_STATEMENT :
            printBreakStatement(file, level, (BreakStatement*) node, lvalue);
            break;
        case NT_GOTO_STATEMENT :
            printGotoStatement(file, level, (GotoStatement*) node, lvalue);
            break;
        case NT_LABEL :
            printLabel(file, level, (Label*) node, lvalue);
            break;
        case NT_NAMESPACE :
            printNamespace(file, level, (Namespace*) node, lvalue);
            break;
        case NT_STATEMENT :
            printStatement(file, level, (Statement*) node, lvalue);
            break;
        case NT_FUNCTION_CALL :
            printFunctionCall(file, level, (FunctionCall*) node, lvalue);
            break;
        case NT_OPERAND :
            printOperand(file, level, (Operand*) node, lvalue);
            break;
        case NT_UNARY_OPERATOR :
            printUnaryOperator(file, level, (UnaryOperator*) node, lvalue);
            break;
        case NT_BINARY_OPERATOR :
            printBinaryOperator(file, level, (BinaryOperator*) node, lvalue);
            break;
        case NT_TERNARY_OPERATOR :
            printTernaryOperator(file, level, (TernaryOperator*) node, lvalue);
            break;
        case NT_EXPRESSION_WRAPPER :
            break;     
    
        default:
            break;
    
    }

}

void exit() {

}

void printDataType(const DataTypeEnum dtypeEnum) {

    DataType* const dtype = dataTypes + dtypeEnum;

    if (dtypeEnum == DT_CUSTOM) {

        printf("%.*s", dtype->nameLen, dtype->name);    
    
    } else if (dtypeEnum == DT_POINTER) {

        // cant do anything..
    
    } else {

        switch (dtypeEnum) {

            case DT_INT :
                printf("int");
                break;
                
            case DT_I32 :
                printf("i32");
                break;
                
            case DT_I64 :
                printf("i64");
                break;
                
            case DT_F32 :
                printf("f32");
                break;
                
            case DT_F64 :
                printf("f64");
                break;

            default :
                printf("%s", dtype->name);
        
        }
    
    }

}

void printDataType(const DataTypeEnum dtypeEnum, void* dtype) {
    
    if (dtypeEnum == DT_POINTER) {

        Pointer* const ptr = ((Pointer*) dtype);
        printDataType(ptr->pointsToEnum, ptr->pointsTo);
        putchar('^');
    
    } else if (dtypeEnum == DT_ARRAY) {
        
        Array* const arr = (Array*) dtype;
        printDataType(arr->pointsToEnum, arr->pointsTo);

    } else if (dtypeEnum == DT_CUSTOM) {
        
        TypeDefinition* td = (TypeDefinition*) (dtype);
        printf("%.*s", td->nameLen, td->name);
        // fprintf(stdout, "%.*s", td->nameLen, td->name)

    } else {

        printDataType(dtypeEnum);
    
    }

}

void printOperandValue(Operand* op) {

    switch (op->cvalue.dtypeEnum) {

        case DT_INT : {

        }

        case DT_I32 : {
            printf("%i", op->cvalue.i32);
            break;
        }

        case DT_I64 : {
            printf("%li", op->cvalue.i64);
            break;
        }

        case DT_F32 : {
            printf("%.2f", op->cvalue.f32);
            break;
        }

        case DT_F64 : {
            printf("%.2f", op->cvalue.f64);
            break;
        }

        case DT_STRING : {
            StringInitialization* init = (StringInitialization*) op->cvalue.str;
            printf("\"%.*s\"", init->rawPtrLen, init->rawPtr);
            break;
        }

        default : {
            printf("<unknown type>");
        }
    
    }

}

// TODO : maybe abstract??? will it benefit??
void printDataType(DataType* const dtype, const DataTypeEnum dtypeEnum) {

    if (dtypeEnum == DT_CUSTOM) {

        printf("%.*s", dtype->name, dtype->nameLen);
        return;

    } else if (dtypeEnum == DT_POINTER) {

        Pointer* const ptr = (Pointer*) dtype;
        DataType* ptrDtype = dataTypes + DT_POINTER;

        printDataType((DataType*) ptr->pointsTo, ptr->pointsToEnum);
        printf("%s", ptrDtype->name);
        
        return;
    
    }

    switch (dtypeEnum) {

    case DT_INT:
        printf("int");
        break;

    case DT_I8:
        printf("int8_t");
        break;

    case DT_I16:
        printf("int16_t");
        break;

    case DT_I32:
        printf("int32_t");
        break;

    case DT_I64:
        printf("int64_t");
        break;

    case DT_U8:
        printf("uint8_t");
        break;

    case DT_U16:
        printf("uint16_t");
        break;

    case DT_U32:
        printf("uint32_t");
        break;

    case DT_U64:
        printf("uint64_t");
        break;

    case DT_F32:
        printf("float");
        break;

    case DT_F64:
        printf("double");
        break;

    case DT_POINTER:
        printf("void*");
        break;

    case DT_ERROR:
        printf("int");
        break;

    default:
        printf("%s", (dataTypes + dtypeEnum)->name);

    }

}

void printOperator(OperatorEnum opType) {

    switch (opType) {
        
        case OP_UNARY_PLUS :
            putchar('+');    
            break;
        
        case OP_UNARY_MINUS :
            putchar('-');
            break;
        
        case OP_ADDITION :
            putchar('+');
            break;

        case OP_SUBTRACTION :
            putchar('-');
            break;

        case OP_MULTIPLICATION :
            putchar('*');
            break;

        case OP_DIVISION :
            putchar('/');;
            break;

        case OP_MODULO :
            putchar('%');
            break;

        case OP_GET_ADDRESS :
            putchar('&');
            break;

        case OP_GET_VALUE :
            putchar('*');            
            break;

        case OP_BITWISE_AND :
            putchar('&');
            break;

        case OP_EQUAL :
            putchar('=');
            putchar('=');
            break;
        
        case OP_NOT_EQUAL :
            putchar('!');
            putchar('=');
            break;

        case OP_LESS_THAN :
            putchar('<');
            break;

        case OP_GREATER_THAN :
            putchar('>');
            break;

        case OP_LESS_THAN_OR_EQUAL :
            putchar('<');
            putchar('=');
            break;
        
        case OP_BOOL_AND :
            putchar('&');
            putchar('&');
            break;

        case OP_BOOL_OR :
            putchar('|');
            putchar('|');
            break;

        case OP_GREATER_THAN_OR_EQUAL :
            putchar('>');
            putchar('=');
            break;

        case OP_INCREMENT :
            putchar('+');
            putchar('+');
            break;

        case OP_DECREMENT :
            putchar('-');
            putchar('-');
            break;

        case OP_SUBSCRIPT :
            putchar('[');
            break;

        case OP_MEMBER_SELECTION :
            putchar('.');
            break;
    }

}





void printScope(FILE* file, int level, Scope* const node, Variable* lvalue) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);
    
    printf("%s{\n", tab);
    for (int i = 0; i < (int) node->children.size(); i++) {
        print(file, level + 1, node->children[i]);
    }
    printf("%s}\n", tab);

}

void printVariableDefinition(FILE* file, int level, VariableDefinition* const node, Variable* lvalue) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%s", tab);
    if (node->flags & IS_CONST) {
        printf("const ");
        printDataType((DataType*) node->var->cvalue.any, node->var->cvalue.dtypeEnum);
    } else {
        printDataType((DataType*) node->var->cvalue.any, node->var->cvalue.dtypeEnum);
    }

    printf(" %.*s = ", node->var->nameLen, node->var->name);
    
    // again, somehow have to get rid of this check
    if (node->var->expression) {
        
        Expression* ex = node->var->expression;
        printExpression(file, level, ex);

    } else {
        //var->print(level);
        printOperandValue(node->var);
    }

    printf(";\n");

}

void printVariableAssignment(FILE* file, int level, VariableAssignment* const node, Variable* lvalue) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%s", tab);
    printVariable(file, level, node->lvar);
    printf(" = ");
    //printf("%s%.*s = ", tab, var->nameLen, var->name);
    // again, somehow have to get rid of this check
    if (node->rvar->expression) {
        
        Expression* ex = node->rvar->expression;
        printExpression(file, level, ex);

    } else {
        printOperandValue(node->rvar);
    }

    printf(";\n");

}

void printTypeDefinition(FILE* file, int level, TypeDefinition* const node, Variable* lvalue) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%sdef %.*s \n%s{\n", tab, node->nameLen, node->name, tab);
    
    const int lastIdx = (int) node->vars.size() - 1;
    
    for (int i = 0; i < lastIdx; i++) {
        
        Variable* const var = node->vars[i];
        
        // TODO : cleanup
        if (var->cvalue.dtypeEnum != DT_CUSTOM) {
            DataType* const dtype = dataTypes + var->cvalue.dtypeEnum; 
            printf("%s\t%.*s %.*s = %li,\n", tab, dtype->nameLen, dtype->name, var->nameLen, var->name, var->cvalue.i64);
        } else {
            TypeDefinition* const dtype = var->cvalue.def; 
            printf("%s\t%.*s %.*s = %li,\n", tab, dtype->nameLen, dtype->name, var->nameLen, var->name, var->cvalue.i64);    
        }

    }

    if (lastIdx - 1 >= 0) {
        
        Variable* const var = node->vars[lastIdx];
        
        if (var->cvalue.dtypeEnum != DT_CUSTOM) {
            DataType* const dtype = dataTypes + var->cvalue.dtypeEnum; 
            printf("%s\t%.*s %.*s = %li\n", tab, dtype->nameLen, dtype->name, var->nameLen, var->name, var->cvalue.i64);
        } else {
            TypeDefinition* const dtype = var->cvalue.def; 
            printf("%s\t%.*s %.*s = %li\n", tab, dtype->nameLen, dtype->name, var->nameLen, var->name, var->cvalue.i64);
        }
    }
    
    printf("%s}\n", tab);

}

void printTypeInitialization(FILE* file, int level, TypeInitialization* const node, Variable* lvalue) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("{\n");

    for (int i = 0; i < (int) node->attributes.size() - 1; i++) {
        
        Variable* const var = node->attributes[i];
        
        printf("%s\t", tab);

        if (var->nameLen > 0) {
            printf("%.*s = ", var->nameLen, var->name);
            if (var->expression) {
                printExpression(file, level + 1, var->expression);
            } else {
                //printf("%s", tab);
                printOperandValue(var);
            }
        } else {
            if (var->expression) {
                printExpression(file, level + 1, var->expression);
            } else {
                //printf("%s", tab);
                printOperandValue(var);
            }
        }

        printf(",\n");

    }

    if ((int) node->attributes.size() > 0) {
        
        Variable* const var = node->attributes[(int) node->attributes.size() - 1];
        
        printf("%s\t", tab);

        if (var->nameLen > 0) {
            printf("%.*s = ", var->nameLen, var->name);
            if (var->expression) {
                printExpression(file, level + 1, var->expression);
            } else {
                //printf("%s", tab);
                printOperandValue(var);
            }
        } else {
            if (var->expression) {
                printExpression(file, level + 1, var->expression);
            } else {
                //printf("%s", tab);
                printOperandValue(var);
            }
        }

    }

    printf("\n%s}", tab);

}

void printStringInitialization(FILE* file, int level, StringInitialization* const node, Variable* lvalue) {

}

void printArrayInitialization(FILE* file, int level, ArrayInitialization* const node, Variable* lvalue) {

}

void printEnumerator(FILE* file, int level, Enumerator* const node, Variable* lvalue) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    DataType* const dt = dataTypes + node->dtype;

    printf("%senum %.*s %.*s \n%s{\n", tab, node->nameLen, node->name, dt->nameLen, dt->name, tab);
    
    const int lastIdx = (int) node->vars.size() - 1;
    
    for (int i = 0; i < lastIdx; i++) {
        Variable* const var = node->vars[i];
        printf("%s\t%.*s = %li,\n", tab, var->nameLen, var->name, var->cvalue.i64);
    }

    if (lastIdx - 1 >= 0) {
        Variable* const var = node->vars[lastIdx];
        printf("%s\t%.*s = %li\n", tab, var->nameLen, var->name, var->cvalue.i64);
    }
    
    printf("%s}\n", tab);

}

void printVariable(FILE* file, int level, Variable* const node, Variable* lvalue) {
    
    if (node->def && node->def->flags & IS_CMP_TIME) {
        printOperandValue(node->def->var);
    } else if (node->nameLen > 0) {
        printf("%.*s_%i", node->nameLen, node->name, node->id);
    } else {
        if (node->expression) printExpression(file, level, node->expression);
        else printOperandValue(node);
    }

}


void printFunction(FILE* file, int level, Function* const node, Variable* lvalue) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%s%.*s(", tab, node->nameLen, node->name);

    // in args
    for (int i = 0; i < ((int) node->inArgs.size()) - 1; i++) {
        Variable* const var = node->inArgs[i]->var;
        printf("%s %.*s, ", (dataTypes + var->cvalue.dtypeEnum)->name, var->nameLen, var->name);
    }
    if ((int) node->inArgs.size() - 1 >= 0) {
        Variable* const var = node->inArgs[node->inArgs.size() - 1]->var;
        printf("%s %.*s) => (", (dataTypes + var->cvalue.dtypeEnum)->name, var->nameLen, var->name);
    } else {
        printf(") => (");
    
    }

    // out arg
    const int outDtype = node->outArg->var->cvalue.dtypeEnum;
    if (outDtype == DT_VOID) {
        printf(")\n");
    } else {
        const DataType dtype = dataTypes[outDtype];
        printf("%s)\n", dtype.name);
    }

    // TODO : empty body crash
    printScope(file, level, node->bodyScope);

}


void printBranch(FILE* file, int level, Branch* const node, Variable* lvalue) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    // basic if branche
    printf("%s%s ", tab, Lex::KWS_IF);
    printVariable(file, level, node->expressions[0]);
    putchar('\n');
    printScope(file, level, node->scopes[0]);
    
    // if elses
    int i = 1;
    for (; i < node->expressions.size(); i++) {
        printf("%s%s %s ", tab, Lex::KWS_ELSE, Lex::KWS_IF);
        printVariable(file, level, node->expressions[i]);
        putchar('\n');
        printScope(file, level, node->scopes[i]);
    }

    // final else if present
    if (i < node->scopes.size()) {
        printf("%s%s\n", tab, Lex::KWS_ELSE);
        printScope(file, level, node->scopes[(int) node->scopes.size() - 1]);
    }

}

void printSwitchCase(FILE* file, int level, SwitchCase* const node, Variable* lvalue) {

}

void printWhileLoop(FILE* file, int level, WhileLoop* const node, Variable* lvalue) {

    level = (level > CSP_MAX_TAB_LEVEL) ? CSP_MAX_TAB_LEVEL : level;
    char tab[CSP_MAX_TAB_LEVEL + 1];
    CSP_GEN_TAB(tab, level);

    printf("%s%s ", tab, Lex::KWS_WHILE);
    printVariable(file, level, node->expression);
    putchar('\n');
    printScope(file, level, node->bodyScope);

}


void printForLoop(FILE* file, int level, ForLoop* const node, Variable* lvalue) {

}

void printLoop(FILE* file, int level, Loop* const node, Variable* lvalue) {

}

void printReturnStatement(FILE* file, int level, ReturnStatement* const node, Variable* lvalue) {

    printf("return ");
    printVariable(file, level, node->var);
    putchar(',');
    printVariable(file, level, node->err);

}

void printContinueStatement(FILE* file, int level, ContinueStatement* const node, Variable* lvalue) {

    printf("continue;");

}

void printBreakStatement(FILE* file, int level, BreakStatement* const node, Variable* lvalue) {

    printf("break;");

}

void printGotoStatement(FILE* file, int level, GotoStatement* const node, Variable* lvalue) {

    printf("goto %.*s;", node->nameLen, node->name);

}

void printLabel(FILE* file, int level, Label* const node, Variable* lvalue) {

    printf(">%.*s;", node->nameLen, node->name);

}

void printNamespace(FILE* file, int level, Namespace* const node, Variable* lvalue) {

}

void printExpression(FILE* file, int level, Expression* const node, Variable* lvalue) {
    
    switch (node->type) {

        case EXT_WRAPPER:
            printWrapperExpression(file, level, (WrapperExpression*)node, lvalue);
            break;
        case EXT_UNARY:
            printUnaryExpression(file, level, (UnaryExpression*)node, lvalue);
            break;
        case EXT_BINARY:
            printBinaryExpression(file, level, (BinaryExpression*)node, lvalue);
            break;
        case EXT_TERNARY:
            printTernaryExpression(file, level, (TernaryExpression*)node, lvalue);
            break;
        case EXT_FUNCTION_CALL:
            printFunctionCall(file, level, (FunctionCall*)node, lvalue);
            break;
        case EXT_ARRAY_INITIALIZATION:
            printTypeInitialization(file, level, (TypeInitialization*)node, lvalue);
            break;
        case EXT_TYPE_INITIALIZATION:
            printTypeInitialization(file, level, (TypeInitialization*)node, lvalue);
            break;
        case EXT_STRING_INITIALIZATION:
            printStringInitialization(file, level, (StringInitialization*)node, lvalue);
            break;
        case EXT_CATCH:
            printCatchExpression(file, level, (Catch*)node, lvalue);
            break;

    }

}

void printForeignCode() {

}

void printWrapperExpression(FILE* file, int level, WrapperExpression* const node, Variable* lvalue) {

    printVariable(file, level, node->operand);

}

void printConstExpression(FILE* file, int level, ConstExpression* const node, Variable* lvalue) {

}

void printOperatorExpression(FILE* file, int level, OperatorExpression* const node, Variable* lvalue) {

}

void printCatchExpression(FILE* file, int level, Catch* const node, Variable* lvalue) {

    printf("catch");

}

void printUnaryExpression(FILE* file, int level, UnaryExpression* const node, Variable* lvalue) {

    printf("(");
    
    // node->oper->print(thisTranslator);
    printOperator(node->operType);

    if (node->operand->unrollExpression && node->operand->expression) {
        printExpression(file, level, node->operand->expression);
    } else {
        printVariable(file, level, node->operand);
    }

    printf(")");

}

void printBinaryExpression(FILE* file, int level, BinaryExpression* const node, Variable* lvalue) {

    printf("(");
    
    if (node->operandA->unrollExpression && node->operandA->expression) {
        printExpression(file, level, node->operandA->expression);
    } else {
        printVariable(file, level, node->operandA); 
    }

    // node->oper->print(thisTranslator, 1);
    printOperator(node->operType);

    if (node->operandB->unrollExpression && node->operandB->expression) {
        printExpression(file, level, node->operandB->expression);
    } else {
        printVariable(file, level, node->operandB);
    }

    if (node->operType == OP_SUBSCRIPT) putchar(']');

    printf(")");

}

void printTernaryExpression(FILE* file, int level, TernaryExpression* const node, Variable* lvalue) {

}

void printStatement(FILE* file, int level, Statement* const node, Variable* lvalue) {
    
    printVariable(file, level, node->op);
    putchar(';');

}

void printFunctionCall(FILE* file, int level, FunctionCall* const node, Variable* lvalue) {

    if (node->fptr) {

        printf("%.*s_%i(", node->nameLen, node->name, node->fptr->id);
    
    } else if (node->fcn->internalIdx <= 0) {
        
        printf("%.*s_%i(", node->nameLen, node->name, node->fcn->id);
    
    } else {

        printf("%.*s(", node->nameLen, node->name);

    }

    const int inArgsCnt = node->inArgs.size();
    int multipleTypes = 0;
    for (int i = 0; i < inArgsCnt; i++) {
        printVariable(file, level, node->inArgs[i]);
        
        if (!multipleTypes) {

            Variable* tmp;
            if (node->fcn) { 
                tmp = node->fcn->inArgs[i]->var;
            } else {
                tmp = node->fptr;
            }

            if (tmp->cvalue.dtypeEnum == DT_MULTIPLE_TYPES) {
                multipleTypes = 1;
            } else if (tmp->cvalue.dtypeEnum == DT_ARRAY && !(tmp->cvalue.arr->flags & IS_ARRAY_LIST)) {
                fputc(',', file);
                printVariable(file, 0, node->inArgs[i]->cvalue.arr->length);
            }
        
        }

        if (i < inArgsCnt - 1) {
            fputc(',', file);
            fputc(' ', file);
        }
        //Variable* const var = inArgs[i];
        //printf("%s %.*s, ", (dataTypes + var->dataTypeEnum)->word, var->nameLen, var->name);
    }

    fputc(')', file);

}

void printOperand(FILE* file, int level, Operand* const node, Variable* lvalue) {

    // maybe separate type?
    if (node->expression) {
        printExpression(file, level, node->expression);
    } else {
        printOperandValue(node);
    }

}

void printOperator(FILE* file, int spaces, Operator* const node) {

    //if (spaces) printf(" %c%c%c%c ", A, B, C, D);
    //else printf("%c%c%c%c", A, B, C, D);

}

void printUnion(FILE* file, int level, Union* const node, Variable* lvalue) {

}

void printErrorSet(FILE* file, int level, ErrorSet* const node, Variable* lvalue) {

}

void printUnaryOperator(FILE* file, int level, UnaryOperator* const node, Variable* lvalue) {

}

void printBinaryOperator(FILE* file, int level, BinaryOperator* const node, Variable* lvalue) {

}

void printTernaryOperator(FILE* file, int level, TernaryOperator* const node, Variable* lvalue) {

}



Translator translatorItselfConsole{
    NULL,
    0,
    &init,
    &print,
    &printExpression,
    &printForeignCode,
    &exit
};
