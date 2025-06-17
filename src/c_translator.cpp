// #pragma once

#include <stdio.h>

#include "c_translator.h"
#include "math.h"
#include <cstring>

#define min(x, y) ((x) < (y))
#define MAX_FILE_SIZE 256
#define MAX_ARRAY_ID_SIZE 4
#define INTERNAL_FOLDER "resources/"

const char  INTERNAL_ARRAY_LIST_STR[]   = "array_list.c";
const char  INTERNAL_TYPEDEF_STR[]      = "typedef.c";

const char  MAIN_FILE_STR[]         = "main.c";
const char  FUNC_DEFS_FILE_STR[]    = "functions.h";
const char  FUNC_FILE_STR[]         = "functions.c";
const char  TYPE_FILE_STR[]         = "typedefs.h";
const char  GLOB_FILE_STR[]         = "global_scope.c";
const char  VARS_FILE_STR[]         = "variables.c";
const char  FOREIGN_CODE_STR[]      = "foreign_code.c";


uint64_t lastLine = 0;


FILE* mFile     = NULL;
FILE* fdFile    = NULL;
FILE* fFile     = NULL;
FILE* tFile     = NULL;
FILE* gFile     = NULL;
FILE* vFile     = NULL;
FILE* fcFile    = NULL;


const char* const dtypePostfix[] = {
    "VOID",
    "I32",
    "I8",
    "I16",
    "I32",
    "I64",
    "U8",
    "U16",
    "U32",
    "U64",
    "F32",
    "F64",
    "Char",
    "Void",
    "DT_ARRAY",
    "DT_MULTIPLE_TYPES",
    "DT_CUSTOM",
    "DT_MEMBER",
    "DT_ENUM",
    "DT_UNDEFINED"
};




// 0 on success
int newDir(char* const path) {
    try {
        std::filesystem::create_directory(path);
        return 0;
    } catch (Expression* ex) {
        return 1;
    }
}

// 0 on success
int chdir(char* const path) {
    try {
        std::filesystem::current_path(path);
        return 0;
    } catch (Expression* ex) {
        return 1;
    }
}



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
static void printWrapperExpression(FILE* file, int level, WrapperExpression* const node, Variable* lvalue = NULL);
static void printExpressionWrapper(FILE* file, int level, ExpressionWrapper* const node, Variable* lvalue = NULL);
static void printConstExpression(FILE* file, int level, ConstExpression* const node, Variable* lvalue = NULL);
static void printOperatorExpression(FILE* file, int level, OperatorExpression* const node, Variable* lvalue = NULL);
static void printUnaryExpression(FILE* file, int level, UnaryExpression* const node, Variable* lvalue = NULL);
static void printBinaryExpression(FILE* file, int level, BinaryExpression* const node, Variable* lvalue = NULL);
static void printTernaryExpression(FILE* file, int level, TernaryExpression* const node, Variable* lvalue = NULL);
static void printFunctionCall(FILE* file, int level, FunctionCall* const node, Variable* lvalue = NULL, Variable* err = NULL);
static void printCatchExpression(FILE* file, int level, Catch* const node, Variable* lvalue = NULL, const int isGlobal = 0);
static void printOperand(FILE* file, int level, Operand* const node, Variable* lvalue = NULL);
static void printUnaryOperator(FILE* file, int level, UnaryOperator* const node, Variable* lvalue = NULL);
static void printBinaryOperator(FILE* file, int level, BinaryOperator* const node, Variable* lvalue = NULL);
static void printTernaryOperator(FILE* file, int level, TernaryOperator* const node, Variable* lvalue = NULL);

static void printArrayInitialization(FILE* file, int level, ArrayInitialization* const node, Variable* lvalue = NULL);
static void printStringInitialization(FILE* file, int level, StringInitialization* const node, Variable* lvalue = NULL);

static void printFunctionDefinition(FILE* file, Function* const node, const int printId = 1);
static void printDataType(FILE* file, const DataTypeEnum dtypeEnum);

static void printForeignBlockC(CodeBlock* const block);
static void printForeignFunctionC(ForeignFunction* const fcn);




// TODO : handle errors
void c_init(char* const dirName) {

    if (newDir(dirName)) {
        return;
    }

    if (chdir(dirName)) {
        return;
    }

    mFile = fopen(MAIN_FILE_STR, "w");
    if (!mFile) {
        return;
    }
    translatorC.mainFile = mFile;

    fdFile = fopen(FUNC_DEFS_FILE_STR, "w");
    if (!fdFile) {
        return;
    }

    fFile = fopen(FUNC_FILE_STR, "w");
    if (!fFile) {
        return;
    }

    tFile = fopen(TYPE_FILE_STR, "w");
    if (!tFile) {
        return;
    }

    vFile = fopen(VARS_FILE_STR, "w");
    if (!vFile) {
        return;
    }

    fcFile = fopen(FOREIGN_CODE_STR, "w");
    if (!fcFile) {
        return;
    }

    // fprintf(fFile, "#include \"stdint.h\"\n");
    // fprintf(mFile, "#pragma once\n");

    // translatorC.mainFile = stdout;
    if (internalFunctionUsed & (1 << (IF_PRINTF - 1))) {
        fprintf(mFile, "#include <stdio.h>\n");    
    }

    if (internalFunctionUsed & (1 << (IF_ALLOC - 1))) {
        fprintf(mFile, "#include <stdlib.h>\n");
    }

    if (1) {
        fprintf(mFile, "#include \"%s\"\n", INTERNAL_TYPEDEF_STR);
    }

    if (1) {
        fprintf(mFile, "#include \"%s\"\n", INTERNAL_ARRAY_LIST_STR);
    }

    fprintf(mFile, "#include \"stdint.h\"\n");
    fprintf(mFile, "#include \"%s\"\n", FOREIGN_CODE_STR);
    fprintf(mFile, "#include \"%s\"\n", TYPE_FILE_STR);
    fprintf(mFile, "#include \"%s\"\n", FUNC_DEFS_FILE_STR);
    fprintf(mFile, "#include \"%s\"\n", VARS_FILE_STR);
    fprintf(mFile, "#include \"%s\"\n", FUNC_FILE_STR);
    fprintf(mFile, "int main(int argc, char** argv)");

    //fprintf(fFile, "#pragma once\n");
    //fprintf(fFile, "#include \"globals.h\"\n");

}

void c_print(FILE* file, int level, SyntaxNode* const node, Variable* lvalue = NULL) {
    
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
            printExpressionWrapper(file, level, (ExpressionWrapper*) node, lvalue);
            break;   
    
        default:
            break;
    
    }

}

void c_printExpression(FILE* file, int level, Expression* const node, Variable* lvalue = NULL) {

    switch (node->type) {

        case EXT_WRAPPER:       
            printWrapperExpression(file, level, (WrapperExpression*) node, lvalue);
            break;
        case EXT_UNARY:
            printUnaryExpression(file, level, (UnaryExpression*) node, lvalue);
            break;
        case EXT_BINARY:
            printBinaryExpression(file, level, (BinaryExpression*) node, lvalue);
            break;
        case EXT_TERNARY:
            printTernaryExpression(file, level, (TernaryExpression*) node, lvalue);
            break;
        case EXT_FUNCTION_CALL: 
            printFunctionCall(file, level, (FunctionCall*) node, lvalue);
            break;
        case EXT_ARRAY_INITIALIZATION:
            printTypeInitialization(file, level, (TypeInitialization*)node, lvalue);
            break;
        case EXT_TYPE_INITIALIZATION: 
            printTypeInitialization(file, level, (TypeInitialization*) node, lvalue);
            break;
        case EXT_STRING_INITIALIZATION:
            printStringInitialization(file, level, (StringInitialization*) node, lvalue);
            break;
        case EXT_CATCH:
            printCatchExpression(file, level, (Catch*) node, lvalue);
            break;
    
    }

}

void c_printForeignCode() {

    for (int i = 0; i < SyntaxNode::codeBlocks.size(); i++) {
        
        CodeBlock* const block = SyntaxNode::codeBlocks[i];
        if (
            (strncmp(block->tagStr, "C", min(block->tagLen, 1)) == 0) || 
            (strncmp(block->tagStr, "C", min(block->tagLen, 1)) == 0)
        ) {
            printForeignBlockC(block);
        }

    }

    for (int i = 0; i < SyntaxNode::foreignFunctions.size(); i++) {
        
        ForeignFunction* const fcn = SyntaxNode::foreignFunctions[i];
        if (
            (strncmp(fcn->tagStr, "C", min(fcn->tagLen, 1)) == 0) || 
            (strncmp(fcn->tagStr, "C", min(fcn->tagLen, 1)) == 0)
        ) {
            printForeignFunctionC(fcn);
        }

    }

}

void c_exit() {

    fclose(mFile);
    fclose(fFile);
    // fclose(gFile);
    fclose(tFile);
    fclose(vFile);
    fclose(fdFile);
    fclose(fcFile);

}



char* escapePath(const std::filesystem::path& path) {

    std::string str = path.string();
    
    // compute extra size first
    int extraSize = 0;
    for (char ch : str) {
        if (ch == '\\' || ch == '\"' || ch == '\n' || ch == '\t') extraSize++;
    }

    // TODO : not sure what will happen if we are not in ASCII world
    char* outStr = (char*) malloc(str.length() + extraSize);
    
    int j = 0;
    for (int i = 0; i < str.length(); i++) {
        
        const char ch = str[i];

        if (ch == '\\' || ch == '\"' || ch == '\n' || ch == '\t') {
            outStr[j] = '\\';
            j++;
        }

        outStr[j] = ch;
        j++;
    
    }

    return outStr;

}

void printDebugLine(FILE* file, Location* loc) {
    if (!translatorC.debugInfo) return;
    if (loc && lastLine != loc->line) {
        if (!loc->file->absPathRaw) loc->file->absPathRaw = escapePath(loc->file->absPath);
        fprintf(file, "\n#line %i \"%s\"\n", loc->line, loc->file->absPathRaw);
    }

}






void printName(FILE* file, INamedEx* node) {
    fprintf(file, "%.*s_%i", node->nameLen, node->name, node->id);
}

void printArrayLenName(FILE* file, Variable* var) {
    fprintf(file, "%.*s_len_%i", var->nameLen, var->name, var->id);
}

void printArrayLenDef(FILE* file, Variable* var) {
    // c_printDataType(file, var->cvalue.arr->length->cvalue.dtypeEnum);
    fprintf(file, "uint64_t ");
    printArrayLenName(file, var);
    fputc('=', file);
    printVariable(file, 0, var->cvalue.arr->length);
    fputc(';', file);
}

void printOperandValue(FILE* file, Operand* op) {

    switch (op->cvalue.dtypeEnum) {

        case DT_INT : {

        }

        case DT_INT_32 : {
            fprintf(file, "%i", op->cvalue.i32);
            break;
        }

        case DT_POINTER :
        case DT_INT_64 : {
            fprintf(file, "%li", op->cvalue.i64);
            break;
        }

        case DT_UINT_8:
        case DT_UINT_16:
        case DT_UINT_32:
        case DT_UINT_64: {
            fprintf(file, "%lu", op->cvalue.i64);
            break;
        }

        case DT_FLOAT_32 : {
            fprintf(file, "%.9g%s", op->cvalue.f32, fmod(op->cvalue.f32, 1) == 0 ? ".f" : "f");
            break;
        }

        case DT_FLOAT_64 : {
            fprintf(file, "%.17g%s", op->cvalue.f64, fmod(op->cvalue.f64, 1) == 0 ? "." : "");
            break;
        }

        case DT_STRING : {
            fprintf(file, "\"%s\"", (char*) op->cvalue.str);
            break;
        }

        case DT_ERROR : {
            fprintf(file, "%lu", op->cvalue.i64);
            break;
        }

        default : {
            // fprintf(file, "<unknown type>");
        }
    
    }

}

void printDataType(FILE* file, const DataTypeEnum dtypeEnum) {

    switch (dtypeEnum) {

        case DT_INT :
            fprintf(file, "int");
            break;
            
        case DT_INT_8 :
            fprintf(file, "int8_t");
            break;
        
        case DT_INT_16:
            fprintf(file, "int16_t");
            break;

        case DT_INT_32 :
            fprintf(file, "int32_t");
            break;
            
        case DT_INT_64 :
            fprintf(file, "int64_t");
            break;

        case DT_UINT_8 :
            fprintf(file, "uint8_t");
            break;
        
        case DT_UINT_16 :
            fprintf(file, "uint16_t");
            break;

        case DT_UINT_32 :
            fprintf(file, "uint32_t");
            break;

        case DT_UINT_64 :
            fprintf(file, "uint64_t");
            break;

        case DT_FLOAT_32 :
            fprintf(file, "float");
            break;
            
        case DT_FLOAT_64 :
            fprintf(file, "double");
            break;

        case DT_POINTER :
            fprintf(file, "void*");
            break;

        case DT_ERROR :
            fprintf(file, "int");
            break;

        default :
            fprintf(file, "%s", (dataTypes + dtypeEnum)->name);
    
    }

}

void printDataType(FILE* file, const DataTypeEnum dtypeEnum, void* dtype) {
    
    if (dtypeEnum == DT_POINTER) {

        Pointer* const ptr = ((Pointer*) dtype);
        printDataType(file, ptr->pointsToEnum, ptr->pointsTo);
        fputc('*', file);
    
    } else if (dtypeEnum == DT_ARRAY) {
        
        Array* const arr = (Array*) dtype;
        printDataType(file, arr->pointsToEnum, arr->pointsTo);

    } else if (dtypeEnum == DT_CUSTOM) {
        
        TypeDefinition* td = (TypeDefinition*) (dtype);
        fprintf(file, "%.*s_%i", td->nameLen, td->name, td->id);
        // fprintf(stdout, "%.*s", td->nameLen, td->name)

    } else if (dtypeEnum == DT_FUNCTION) {

        FunctionPrototype* fptr = (FunctionPrototype*) dtype;
        
        printDataType(file, fptr->outArg->var->cvalue.dtypeEnum, fptr->outArg->var->cvalue.any);
        fprintf(file, "(*");
        printVariable(file, 0, fptr->outArg->var);
        fprintf(file, ")(");
        
        const int len = fptr->inArgs.size();
        for (int i = 0; i < len; i++) {
            Value* val = &(fptr->inArgs[i]->var->cvalue);
            printDataType(file, val->dtypeEnum, val->any);
            if (i != len - 1) fputc(',', file);
        }

        fputc(')', file);
    
    } else {

        printDataType(file, dtypeEnum);
    
    }

}

void printOperator(FILE* file, OperatorEnum opType) {

    switch (opType) {
        
        case OP_UNARY_PLUS :
            fputc('+', file);    
            break;
        
        case OP_UNARY_MINUS :
            fputc('-', file);
            break;
        
        case OP_ADDITION :
            fputc('+', file);
            break;

        case OP_SUBTRACTION :
            fputc('-', file);
            break;

        case OP_MULTIPLICATION :
            fputc('*', file);
            break;

        case OP_DIVISION :
            fputc('/', file);
            break;

        case OP_MODULO :
            fputc('%', file);
            break;

        case OP_GET_ADDRESS :
            fputc('&', file);
            break;

        case OP_GET_VALUE :
            fputc('*', file);            
            break;

        case OP_BITWISE_AND :
            fputc('&', file);
            break;

        case OP_BITWISE_OR:
            fputc('|', file);
            break;

        case OP_BITWISE_XOR:
            fputc('^', file);
            break;

        case OP_BITWISE_NEGATION:
            fputc('~', file);
            break;

        case OP_SHIFT_RIGHT:
            fputc('>', file);
            fputc('>', file);
            break;

        case OP_SHIFT_LEFT:
            fputc('<', file);
            fputc('<', file);
            break;

        case OP_NEGATION :
            fputc('!', file);
            break;

        case OP_EQUAL :
            fputc('=', file);
            fputc('=', file);
            break;
        
        case OP_NOT_EQUAL :
            fputc('!', file);
            fputc('=', file);
            break;

        case OP_LESS_THAN :
            fputc('<', file);
            break;

        case OP_GREATER_THAN :
            fputc('>', file);
            break;

        case OP_LESS_THAN_OR_EQUAL :
            fputc('<', file);
            fputc('=', file);
            break;
        
        case OP_BOOL_AND :
            fputc('&', file);
            fputc('&', file);
            break;

         case OP_BOOL_OR :
            fputc('|', file);
            fputc('|', file);
            break;

        case OP_GREATER_THAN_OR_EQUAL :
            fputc('>', file);
            fputc('=', file);
            break;

        case OP_INCREMENT :
            fputc('+', file);
            fputc('+', file);
            break;

        case OP_DECREMENT :
            fputc('-', file);
            fputc('-', file);
            break;

        case OP_SUBSCRIPT :
            fputc('[', file);
            break;

        case OP_MEMBER_SELECTION :
            fputc('.', file);
            break;

        case OP_DEREFERENCE_MEMBER_SELECTION :
            fputc('-', file);
            fputc('>', file);
            break;

    }

}



void printForeignBlockC(CodeBlock* const block) {

    fprintf(fcFile, "%.*s", block->codeLen, block->codeStr);

}

void printForeignFunctionC(ForeignFunction* const fcn) {
    
    printFunctionDefinition(fdFile, fcn, 0);
    fputc(';', fdFile);

    printFunctionDefinition(fFile, fcn, 0);
    fputc('{', fFile);
    fprintf(fFile, "%.*s", fcn->codeLen, fcn->codeStr);
    fputc('}', fFile);

}



void printScope(FILE* file, int level, Scope* const node, Variable* lvalue) {

    const int size = (int) node->children.size();

    fputc('{', file);

    for(int i = 0; i < (int) node->children.size(); i++) {
        c_print(file, 0, node->children[i]);
    }

    fputc('}', file);

}

void printFunctionPointer(FILE* file, FunctionPrototype* fptr, VariableDefinition* node) {
    
    printDataType(file, fptr->outArg->var->cvalue.dtypeEnum, fptr->outArg->var->cvalue.any);
    fprintf(file, "(*");
    fprintf(file, " %.*s_%i", node->var->nameLen, node->var->name, node->var->id);
    fprintf(file, ")(");

    const int len = fptr->inArgs.size();
    for (int i = 0; i < len; i++) {
        Value* val = &(fptr->inArgs[i]->var->cvalue);
        printDataType(file, val->dtypeEnum, val->any);
        if (i != len - 1) fputc(',', file);
    }

    fputc(')', file);

}

// 0 -> nothing
// 1 -> array-list, right side is not expected
// 2 -> global cmp-size-known array
int printVariableDefinitionLValue(FILE* file, int level, VariableDefinition* const node, int isFcnDef = 0, int printId = 1) {

    const int isGlobal = SyntaxNode::root == node->scope;

    const DataTypeEnum dtype = node->var->cvalue.dtypeEnum;
    if (dtype == DT_ARRAY) {

        Array* const arr = node->var->cvalue.arr;

        if (arr->flags & IS_ARRAY_LIST) {

            char* tmp = (char*)dtypePostfix[arr->pointsToEnum];

            if (isGlobal) fprintf(vFile, "ArrayList%s* %.*s_%i;", tmp, node->var->nameLen, node->var->name, node->var->id);
            else fprintf(file, "ArrayList%s* %.*s_%i;", tmp, node->var->nameLen, node->var->name, node->var->id);

            fprintf(file, "%.*s_%i = arrayListCreate%s(", node->var->nameLen, node->var->name, node->var->id, tmp);

            if (arr->length && arr->length->cvalue.hasValue) {
                printVariable(file, level, arr->length);
            }

            fputc(')', file);
            fputc(';', file);

            return 1;

        } else if (arr->flags & IS_ALLOCATED) {

            FILE* target = file;
            if (!isFcnDef) {
                //c_printDataType(file, arr->length->cvalue.dtypeEnum);
                //fputc(' ', file);
                if (isGlobal) {
                    fprintf(vFile, "uint64_t ");
                    //printVariable(vFile, level, arr->length, node->var);
                    printArrayLenName(vFile, node->var);
                    fputc(';', vFile);
                    target = vFile;
                } else {
                    fprintf(file, "uint64_t ");
                }
                
                printArrayLenName(file, node->var);
                fputc('=', file);

                if (arr->length) {
                    printVariable(file, 0, arr->length, node->var);
                } else {
                    fputc('0', file);
                }
                fputc(';', file);
            }

            printDataType(target, arr->pointsToEnum);
            fprintf(target, "* %.*s_%i", node->var->nameLen, node->var->name, node->var->id);
            if (isGlobal) {
                fputc(';', target);
                printDataType(file, arr->pointsToEnum);
                fprintf(file, "* %.*s_%i", node->var->nameLen, node->var->name, node->var->id);
            }

        } else {

            if (isGlobal) {
                printDataType(vFile, dtype, arr);
                fprintf(vFile, "* %.*s_%i", node->var->nameLen, node->var->name, node->var->id);
                fputc(';', vFile);
                fprintf(file, " %.*s_%i", node->var->nameLen, node->var->name, node->var->id);
                return 2;
            } else {
                printDataType(file, dtype, arr);
                fprintf(file, " %.*s_%i[%lu]", node->var->nameLen, node->var->name, node->var->id, arr->length->cvalue.i64);
            }

        }

    } else if (dtype == DT_FUNCTION) {

        FunctionPrototype* fptr = (FunctionPrototype*) node->var->cvalue.any;

        if (isGlobal) {
            
            printFunctionPointer(vFile, fptr, node);
            fputc(';', vFile);
            fprintf(file, " %.*s_%i", node->var->nameLen, node->var->name, node->var->id);
            
        } else {
            printFunctionPointer(file, fptr, node);
        }
    
    } else {

        if (isGlobal) {
            printDataType(vFile, node->var->cvalue.dtypeEnum, node->var->cvalue.any);
            fprintf(vFile, " %.*s_%i", node->var->nameLen, node->var->name, node->var->id);
            fputc(';', vFile);
        } else {
            printDataType(file, node->var->cvalue.dtypeEnum, node->var->cvalue.any);    
        }

        if (printId) {
            fprintf(file, " %.*s_%i", node->var->nameLen, node->var->name, node->var->id);
        } else {
            fprintf(file, " %.*s", node->var->nameLen, node->var->name);    
        }
    }

    return 0;

}

void initArray(Expression* var) {
    // only EXT_ARRAY_INITIALIZATION expected or DT_SLICE


}

inline void printArrayListLength(FILE* file, Variable* arr) {
    fprintf(file, "%.*s_%i->len", arr->nameLen, arr->name, arr->id);
}

// returns 1 if IS_RENDERED was hit, otherwise 0
int printForExpression(FILE* file, Variable* var, Variable* lvalue, int id) {

    Expression* ex = var->expression;
    if (!ex) {
        printVariable(file, 0, var);
        const int dtype = var->cvalue.dtypeEnum;
        if (dtype == DT_ARRAY || dtype == DT_POINTER) {
            fprintf(file, "[i]");
        }
        return 0;
    }

    switch (ex->type) {

        case EXT_UNARY : {
            
            UnaryExpression* uex = (UnaryExpression*) (var->expression);

            printOperator(file, uex->operType);

            if (printForExpression(file, uex->operand, lvalue, id)) {
                printVariable(file, 0, lvalue);
                fprintf(file, "[off%i+i]", id);
            }
            
            break;

        }

        case EXT_BINARY : {

            BinaryExpression* bex = (BinaryExpression*)(var->expression);
            if (bex->operType == OP_CONCATENATION) return 1;


            if (printForExpression(file, bex->operandA, lvalue, id)) {
                printVariable(file, 0, lvalue);
                fprintf(file, "[off%i+i]", id);
            }

            printOperator(file, bex->operType);

            fputc('(', file);
            printForExpression(file, bex->operandB, lvalue, id);
            fputc(')', file);

            break;

        }

        case EXT_WRAPPER : {
            WrapperExpression* wex = (WrapperExpression*) ex;
            if (wex->operand->flags & IS_RENDERED) return 1;
            printForExpression(file, wex->operand, lvalue, id);
            break;
        }

        case EXT_ARRAY_INITIALIZATION : {
            fprintf(file, "%s[i]", var->name);
            break;

        }

        case EXT_STRING_INITIALIZATION : {
            fprintf(file, "%s[i]", var->name);
            break;
        }

        case EXT_SLICE: {
            fprintf(file, "%s[i]", var->name);
            break;
        }

    }

    return 0;

}

// lengths and offsets are noted as off1 len1, off2 len1, etc.
// number coresponds with the block they are in, if node
// cannot provide length, it can be written as len1T, len2T etc.
int printArrayRValue(FILE* file, Variable* lvalue, Variable* var, Variable** arrLen, int prevId, int* pid) {

    Expression* ex = var->expression;
    if (!ex) {
        if (var->cvalue.dtypeEnum == DT_ARRAY) {
            *arrLen = var->cvalue.arr->length;
        }
        return -1;
    }

    switch (ex->type) {

        case EXT_UNARY : {

            UnaryExpression* uex = (UnaryExpression*) (var->expression);
            
            return printArrayRValue(file, lvalue, uex->operand, arrLen, prevId, pid);
            
            break;
        
        }

        case EXT_BINARY : {
            BinaryExpression* bex = (BinaryExpression*) (var->expression);
            if (bex->operType == OP_CONCATENATION) {

                *pid += 1;
                const int id = *pid;

                Variable* lenA = NULL;
                int idA = printArrayRValue(file, lvalue, bex->operandA, &lenA, prevId, pid);
                if (idA < 0) {
                    if (prevId < 0) prevId = 0;
                } else {
                    prevId = idA;
                }

                if (!(bex->operandA->flags & IS_RENDERED)) {

                    fprintf(file, "int off%i=off%i;", id, prevId);
                    fprintf(file, "tmp=off%i;", id);
                    
                    if (lenA) {
                        fprintf(file, "int len%i=", id);
                        printVariable(file, 0, lenA);
                        fputc(';', file);

                        //fprintf(file, "for (int i = 0; i <len%i", id);
                        //fprintf(file, "; i++){");
                    } else {
                        if (idA > 0) {
                            fprintf(file, "int len%i=len%i;", id, prevId);
                        } else {
                            fprintf(file, "int len%i=len%iT;", id, id);
                        }
                    }
                    fprintf(file, "for (int i = 0; i < len%i; i++){", id);
                    printVariable(file, 0, lvalue);
                    fprintf(file, "[off%i + i]=", id);
                    printForExpression(file, bex->operandA, lvalue, id);
                    fprintf(file, ";}");

                    if (lenA) {
                        fprintf(file, "off%i+=", id);
                        printVariable(file, 0, lenA);
                        fputc(';', file);
                    } else {
                        fprintf(file, "off%i+=len%i;", id, prevId);
                    }

                } else {

                    fprintf(file, "int off%i=off%i+len%i;tmp=off%i;", id, prevId, prevId, prevId);
                    fprintf(file, "int len%i=len%i;", id, prevId);
                
                }

                Variable* lenB = NULL;
                int idB = printArrayRValue(file, lvalue, bex->operandB, &lenB, id, pid);
                if (!(bex->operandB->flags & IS_RENDERED)) {
                    if (lenB) {
                        fprintf(file, "len%i+=", id);

                        printVariable(file, 0, lenB);
                        fputc(';', file);

                        fprintf(file, "for (int i = 0; i <");
                        printVariable(file, 0, lenB);
                        fprintf(file, "; i++){");
                    }
                    else {
                        fprintf(file, "len%i+=len%iT;", id, id);
                        fprintf(file, "for (int i = 0; i < len%iT; i++){", id);
                    }
                    printVariable(file, 0, lvalue);
                    fprintf(file, "[off%i+i]=", id);
                    printForExpression(file, bex->operandB, lvalue, id);
                    fprintf(file, ";}");
                
                }

                fprintf(file, "off%i=tmp;", id);

                var->flags |= IS_RENDERED;

                return id;
                
            }

            const int idA = printArrayRValue(file, lvalue, bex->operandA, arrLen, prevId, pid);
            const int idB = printArrayRValue(file, lvalue, bex->operandB, arrLen, prevId, pid);
            if (idA > 0) return idA;
            /*
            if (bex->operandA->flags & IS_RENDERED || bex->operandB->flags & IS_RENDERED) {
                var->flags |= IS_RENDERED;
            }
            */

            break;
        }

        case EXT_WRAPPER : {
            WrapperExpression* wex = (WrapperExpression*) ex;
            int id = printArrayRValue(file, lvalue, wex->operand, arrLen, prevId, pid);
            var->flags = wex->operand->flags;
            return id;
        }

        case EXT_ARRAY_INITIALIZATION : {
            
            ArrayInitialization* init = (ArrayInitialization*) ex;
            
            fprintf(file, "int len%iT=%i;", *pid, init->attributes.size());
            
            const int nameLen = 1 + MAX_ARRAY_ID_SIZE + 1;
            var->name = (char*) malloc(nameLen);
            sprintf(var->name, "a%i", var->id);
            
            printDataType(file, var->cvalue.dtypeEnum, var->cvalue.arr);
            fprintf(file, " %s[]=", var->name);
            printArrayInitialization(file, 0, init);
            fputc(';', file);

            break;

        }

        case EXT_STRING_INITIALIZATION : {
            

            StringInitialization* init = (StringInitialization*) ex;
            
            if (init->wideStr) {
                fprintf(file, "int len%iT=%i;", *pid, init->wideLen);
            } else {
                fprintf(file, "int len%iT=%i;", *pid, init->rawPtrLen);            
            }

            const int nameLen = 1 + MAX_ARRAY_ID_SIZE + 1;
            var->name = (char*) malloc(nameLen);
            sprintf(var->name, "a%i", var->id);

            printDataType(file, init->wideDtype);
            fprintf(file, " %s[]=", var->name);
            printStringInitialization(file, 0, init);
            fputc(';', file);

            break;

        }

        case EXT_SLICE : {

            Slice* slice = (Slice*) ex;

            const int nameLen = 1 + MAX_ARRAY_ID_SIZE + 1;
            var->name = (char*) malloc(nameLen);
            sprintf(var->name, "a%i", var->id);

            printDataType(file, slice->arr->cvalue.dtypeEnum, slice->arr->cvalue.any);
            if (slice->arr->cvalue.dtypeEnum != DT_POINTER) {
                fputc('*', file);
            }
            fprintf(file, " %s=", var->name);
            printVariable(file, 0, slice->arr);
            fprintf(file, "+");
            printVariable(file, 0, slice->bidx);
            fputc(';', file);

            // maybe just add length attribute to Slice
            BinaryExpression* lenEx = new BinaryExpression();
            lenEx->operandA = slice->eidx;
            lenEx->operandB = slice->bidx;
            lenEx->operType = OP_SUBTRACTION;

            Variable* len = new Variable();            
            len->expression = lenEx;

            *arrLen = len;

        }


    }

    return -1;

}

void printArray(FILE* file, Variable* lvalue, Variable* rvalue) {

    // file = stdout;
    Variable* var = lvalue->expression ? lvalue : rvalue;

    Expression* ex = lvalue->expression;
    const int exType = ex ? ex->type : EXT_WRAPPER;
    switch (exType) {
        
        case EXT_SLICE : {

            Slice* slice = (Slice*) ex;
            var = rvalue;
            lvalue = slice->arr;

            //fprintf(file, ";{int off0=""0;int len0=");

            fprintf(file, ";{int off0=");
            printVariable(file, 0, slice->bidx);

            
            fprintf(file, ";int len0=");
            if (slice->eidx->cvalue.dtypeEnum != DT_UNDEFINED) {
                fprintf(file, "1+");
                printVariable(file, 0, slice->eidx);
                fputc('-', file);
            } else {
                slice->eidx->cvalue.dtypeEnum = DT_INT_64;
                printVariable(file, 0, slice->eidx);
                fputc('+', file);
            }
            printVariable(file, 0, slice->bidx);
            fputc(';', file);

            break;

        }

        case EXT_ARRAY_INITIALIZATION : {
            fputc('=', file);
            fprintf(file, "((");
            printDataType(file, lvalue->cvalue.dtypeEnum, lvalue->cvalue.any);
            fprintf(file, "[%lu])", lvalue->cvalue.arr->length->cvalue.u64);
            printArrayInitialization(file, 0, (ArrayInitialization*) ex);
            fputc(')', file);
            fputc(';', file);
            return;
        }

        case EXT_STRING_INITIALIZATION : {
            fputc('=', file);
            printStringInitialization(file, 0, (StringInitialization*) ex, lvalue);
            fputc(';', file);
            return;
        }

        case EXT_FUNCTION_CALL : {
            // alloc
            fputc('=', file);
            printFunctionCall(file, 0, (FunctionCall*) ex, lvalue);
            fputc(';', file);
            return;
        }

        case EXT_BINARY: {
            
            BinaryExpression* bex = (BinaryExpression*) lvalue->expression;
            if (bex->operType == OP_SUBSCRIPT) {

                Array* arr = bex->operandA->cvalue.arr;

                fprintf(file, ";{int off0=");
                printArrayListLength(file, bex->operandA);
                fprintf(file, ";int len0=");
                arr->length->cvalue.dtypeEnum = DT_INT_64;
                printVariable(file, 0, arr->length);
                //arr->length->cvalue.dtypeEnum = DT_UNDEFINED;
                fputc(';', file);

                const char* postfix = dtypePostfix[arr->pointsToEnum];
                fprintf(file, "arrayListAppendAlloc%s(%.*s_%i,len0);", postfix, bex->operandA->nameLen, bex->operandA->name, bex->operandA->id);
                //c_printVariable(file, 0, arr->length);
                //fprintf(file, ");");

                var = rvalue;
                lvalue = bex->operandA;

                break;
            
            }

            if (bex->operType == OP_MEMBER_SELECTION) {

                fprintf(file, ";{int off0=""0;int len0=");
                printVariable(file, 0, bex->operandB->cvalue.arr->length);
                fputc(';', file);
                
                var = rvalue;

                break;
            }

        }
    
        default:
            fprintf(file, ";{int off0=""0;int len0=");
            printVariable(file, 0, lvalue->cvalue.arr->length);
            fputc(';', file);
            break;

    }

    Variable* arrLen = NULL;
    int id = 0;
    fprintf(file, "int tmp;");
    printArrayRValue(file, lvalue, var, &arrLen, -1, &id);

    fprintf(file, "for (int i = 0; i < len0; i++){");
    printVariable(file, 0, lvalue);
    fprintf(file, "[off0+i]=");
    if (printForExpression(file, var, lvalue, 0)) {
        printVariable(file, 0, lvalue);
        fprintf(file, "[off0+i];");
    }
    fprintf(file, ";}");

    fputc('}', file);
            
    // for startIdx -> endIdx


}

void printCatchExpression(FILE* const file, int level, Catch* node, Variable* lvalue, const int isGlobal) {
    
    Catch* const cex = node;
    Variable* const err = cex->err;
    FunctionCall* const call = cex->call;

    if (node->scope) {
        fprintf(isGlobal ? vFile : file, "int %.*s_%i = 0;", err->nameLen, err->name, err->id);
    } else {
        fprintf(file, "%.*s_%i = 0;", err->nameLen, err->name, err->id);
    }

    if (lvalue) {
        if (lvalue->def) {
            printVariableDefinitionLValue(file, level, lvalue->def);
        } else {
            printVariable(file, level, err, lvalue);
        }
        fputc('=', file);
    }

    printFunctionCall(file, level, call, lvalue, err);
    fputc(';', file);

    if (cex->scope) printScope(file, level, cex->scope, lvalue);

}

void printVariableDefinition(FILE* file, int level, VariableDefinition* const node, Variable* lvalue) {

    if (node->flags & IS_CMP_TIME && node->var->cvalue.dtypeEnum != DT_ARRAY) return;

    printDebugLine(file, node->loc);

    if (node->var->expression && node->var->expression->type == EXT_CATCH) {
        printCatchExpression(file, level, (Catch*) node->var->expression, node->var, SyntaxNode::root == node->scope);
        return;
    }

    const int ans = printVariableDefinitionLValue(file, level, node);
    if (ans == 1) return;
    
    if (node->var->cvalue.dtypeEnum == DT_ARRAY) {
        
        if (node->var->expression) {
            if ((node->var->expression->type == EXT_BINARY || node->var->expression->type == EXT_UNARY || node->var->expression->type == EXT_WRAPPER) && ans == 2) {
                fputc('=', file);
                fprintf(file, "((");
                printDataType(file, node->var->cvalue.dtypeEnum, node->var->cvalue.any);
                fprintf(file, "[%lu])", node->var->cvalue.arr->length->cvalue.u64);
                fprintf(file, "{}");
                fprintf(file, ")");
            }
            // printArray(file, node->var, NULL);
            printArray(file, node->var, node->var);
        } else {
            if (ans == 2) {
                fputc('=', file);
                fprintf(file, "((");
                printDataType(file, node->var->cvalue.dtypeEnum, node->var->cvalue.any);
                fprintf(file, "[%lu])", node->var->cvalue.arr->length->cvalue.u64);
                fprintf(file, "{}");
                fprintf(file, ")");    
            }
            fputc(';', file);
        }

        return;

    }

    if (node->var->expression) {
        fputc('=', file);
        c_printExpression(file, level, node->var->expression, node->var);
        // node->var->expression->print(&translatorC, file, level, node->var);
    } else if (node->var->cvalue.hasValue) {
        fputc('=', file);
        printOperandValue(file, node->var);
    }

    /*
    if (node->flags & IS_ARRAY) {
        
        fputc('[', file);
        
        if (node->flags & IS_CONST) {
            // c_printVariable(file, level, node->var->allocSize);
            fprintf(file, "%i", ((Array*) (node->var->dtype))->length);
        }
        
        fputc(']', file);
    
    }
    */

    fputc(';', file);
    
    const DataTypeEnum dtype = node->var->cvalue.dtypeEnum;
    if (dtype == DT_CUSTOM && !(node->var->expression)) {

        TypeDefinition* customDtype = node->var->cvalue.def;
        for (int i = 0; i < customDtype->vars.size(); i++) {
            
            Variable* var = customDtype->vars[i];
           
            if (!(var->expression)) continue;

            fprintf(file, " %.*s_%i.", node->var->nameLen, node->var->name, node->var->id);
            printVariable(file, level, var);

            fputc('=', file);

            c_printExpression(file, level, var->expression);
            
            fputc(';', file);
    
        }
    }

}

void printVariableAssignment(FILE* file, int level, VariableAssignment* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);

    if (!node->rvar) {
        printVariable(file, level, node->lvar);
        // node->lvar->print(&translatorC, file, level);
        fputc(';', file);
    }

    if (node->rvar->expression && node->rvar->expression->type == EXT_CATCH) {
        printCatchExpression(file, level, (Catch*)node->rvar->expression, node->lvar, 1);
        return;
    }

    if (node->lvar->cvalue.dtypeEnum == DT_ARRAY) {
        
        if (node->rvar->expression && node->rvar->expression->type == EXT_FUNCTION_CALL) {
            
            FunctionCall* call = (FunctionCall*) node->rvar->expression;
            if (call->fcn->internalIdx == IF_ALLOC) {

                printArrayLenName(file, node->lvar);
                fputc('=', file);
                printVariable(file, level, call->inArgs[0]->cvalue.arr->length, lvalue);
                fputc(';', file);
                //c_printArrayLenName(file, var);

                printVariable(file, level, node->lvar);
                // node->lvar->print(&translatorC, file, level);
                fputc('=', file);
                printFunctionCall(file, 0, (FunctionCall*)node->rvar->expression, node->lvar);
                fputc(';', file);
                return;
            
            }
            
        }

        printArray(file, node->lvar, node->rvar);
        return;

    }

    // meh
    if (node->lvar->expression && node->lvar->expression->type == EXT_BINARY) {

        BinaryExpression* bex = (BinaryExpression*)node->lvar->expression;
        Variable* opA = bex->operandA;

        const Value valA = bex->operandA->cvalue;
        if (valA.dtypeEnum == DT_ARRAY) {

            const uint64_t flags = valA.arr->flags;

            if (flags & IS_ALLOCATED && !(flags & IS_ARRAY_LIST)) {
                // update also length variable

            }

            if (valA.arr->flags & IS_ARRAY_LIST && bex->operType == OP_SUBSCRIPT) {
                fprintf(file, "arrayListInsert%s(%.*s_%i,", dtypePostfix[valA.arr->pointsToEnum], opA->nameLen, opA->name, opA->id);
                printVariable(file, level, bex->operandB);
                fputc(',', file);
                printVariable(file, level, node->rvar);
                fprintf(file, ");");
                return;
            }
        }

    }

    const int lvalIsSlice = (node->lvar->expression) ? node->lvar->expression->type == EXT_SLICE : 0;
    const int rvarEType = (node->rvar->expression) ? node->rvar->expression->type : -1;

    if (lvalIsSlice) {

        Slice* sliceL = (Slice*)node->lvar->expression;

        if (rvarEType == EXT_SLICE) {
            
            Slice* sliceR = (Slice*)node->rvar->expression;

            fprintf(file, "for(int i=");
            printVariable(file, level, sliceL->bidx);
            fprintf(file, ",j=");
            printVariable(file, level, sliceR->bidx);
            fprintf(file, "; i <= ");
            printVariable(file, level, sliceL->eidx);
            fprintf(file, ";i++, j++){");
            printVariable(file, level, sliceL->arr);
            fprintf(file, "[i]=");
            printVariable(file, level, sliceR->arr);
            fprintf(file, "[j];}");
            
            return;
        
        } else if (rvarEType == EXT_ARRAY_INITIALIZATION) {
            
            ArrayInitialization* aex = (ArrayInitialization*) (node->rvar->expression);

            fprintf(file, "{int j=");
            printVariable(file, level, sliceL->bidx);
            fputc(';', file);

            const int size = aex->attributes.size();
            for (int i = 0; i < size; i++) {
                printVariable(file, level, sliceL->arr);
                fprintf(file, "[j]=");
                printVariable(file, level, aex->attributes[i]);
                if (i < size - 1) fprintf(file, ";j++;");
            }
            fprintf(file, ";}");

            return;
        
        } 

        fprintf(file, "for(int i=");
        printVariable(file, level, sliceL->bidx);
        fprintf(file, ";i<=");
        printVariable(file, level, sliceL->eidx);
        fprintf(file, ";i++){");
        printVariable(file, level, sliceL->arr);
        fprintf(file, "[i]=");

    } else if (node->rvar->expression && node->rvar->expression->type == EXT_SLICE) {
        
        Slice* slice = (Slice*)node->rvar->expression;

        fprintf(file, "for(int i=");
        printVariable(file, level, slice->bidx, node->lvar);
        fprintf(file, ",j=0;i<=");
        printVariable(file, level, slice->eidx, node->lvar);
        fprintf(file, ";i++,j++){");
        printVariable(file, level, node->lvar);
        fprintf(file, "[j]=");
        printVariable(file, level, slice->arr);
        fprintf(file, "[i];}");

        return;

    } else {
    
        printVariable(file, level, node->lvar);
        // node->lvar->print(&translatorC, file, level);
        fputc('=', file);
    
    }

    if (rvarEType == EXT_TYPE_INITIALIZATION) {

        printTypeInitialization(file, level, (TypeInitialization*) node->rvar->expression, node->lvar);
        fputc(';', file);
        return;    
    
    }

    // node->lvar->print(&translatorC, file, level);
    // fputc('=', file);

    printVariable(file, level, node->rvar, node->lvar);
    fputc(';', file);
    
    /*
    TypeInitialization* tinit = (TypeInitialization*) (node->rvar->expression);
    TypeDefinition* dtype = (TypeDefinition*) node->lvar->def->var->dtype;
        
    for (int i = 0; i < tinit->attributes.size(); i++) {
            
        node->lvar->print(&translatorC, file, level);
        fputc('.', file);
        dtype->vars[i]->print(&translatorC, file, level);

        fputc('=', file);

        c_printVariable(file, level, tinit->attributes[i], node->lvar);
        fputc(';', file);
        
    }
    */

}

void printTypeDefinition(FILE* file, int level, TypeDefinition* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);

    if (node->type == NT_UNION) {
        fprintf(tFile, "typedef union %.*s_%i{", node->nameLen, node->name, node->id);
    } else {
        fprintf(tFile, "typedef struct %.*s_%i{", node->nameLen, node->name, node->id);
    }

    const int size = (int) node->vars.size();
    
    for (int i = 0; i < size; i++) {
        
        Variable* const var = node->vars[i];

        const DataTypeEnum dtype = var->cvalue.dtypeEnum;
        if (dtype == DT_ARRAY) {

            Array* const arr = var->cvalue.arr;

            printDataType(tFile, dtype, arr);
            fprintf(tFile, " %.*s_%i[", var->nameLen, var->name, var->id);
            printVariable(tFile, 0, arr->length, var);
            fprintf(tFile, "]");
        
        } else {

            printDataType(tFile, var->cvalue.dtypeEnum, var->cvalue.any);
            fprintf(tFile, " %.*s_%i", var->nameLen, var->name, var->id);

        }

        fputc(';', tFile);

    }
    
    fprintf(tFile, "}%.*s_%i;", node->nameLen, node->name, node->id);

}

void printTypeInitialization(FILE* file, int level, TypeInitialization* const node, Variable* lvalue) {

    // whatever...
    if (lvalue) {

        TypeDefinition* const td = lvalue->def ? (lvalue->def->var->cvalue.def) : lvalue->cvalue.def;
        fprintf(file, "((%.*s_%i){", td->nameLen, td->name, td->id);
        
        if (lvalue->type == NT_UNION) {

            if (node->attributes.size() > 0) {
                Variable* const var = node->attributes[0];
                fprintf(file, ".%.*s_%i= ", var->nameLen, var->name, var->id);
                if (var->expression) c_printExpression(file, level, var->expression);
                else printOperandValue(file, var);
            }

        } else if (node->fillVar) {

            const int size = td->vars.size();
            for (int i = 0; i < size; i++) {
                Variable* const var = td->vars[i];

                const int idx = node->idxs[i];
                if (idx >= 0) {
                    Variable* const var = node->attributes[idx];
                    // if (var->nameLen > 0) fprintf(file, "%.*s_%i: ", var->nameLen, var->name, var->id);
                    if (var->expression) c_printExpression(file, level, var->expression);
                    else printOperandValue(file, var);
                } else {
                    printVariable(file, level, node->fillVar);    
                }

                if (i < size - 1) fputc(',', file);
            }
        
        } else {

            const int size = node->attributes.size();
            for (int i = 0; i < size; i++) {
                Variable* const var = node->attributes[i];

                if (var->nameLen > 0) fprintf(file, "%.*s_%i: ", var->nameLen, var->name, var->id);
                if (var->expression) c_printExpression(file, level, var->expression);
                else printOperandValue(file, var);

                if (i < size - 1) fputc(',', file);
            }    
        
        }

        fprintf(file, "})");

    } else {

        fputc('{', file);
    
        const int size = node->attributes.size();
        for (int i = 0; i < size; i++) {
            Variable* const var = node->attributes[i];

            if (var->nameLen > 0) fprintf(file, "%.*s_%i: ", var->nameLen, var->name, var->id);
            if (var->expression) c_printExpression(file, level, var->expression);
            else printOperandValue(file, var);

            if (i < size - 1) fputc(',', file);
        }

        fputc('}', file);

    }

}

void printStringInitialization(FILE* file, int level, StringInitialization* const node, Variable* lvalue) {

    if (!(node->wideStr)) {
        fprintf(file, "\"%.*s\"", node->rawPtrLen, node->rawPtr);
        return;
    }

    if (lvalue->scope == SyntaxNode::root) {
        fprintf(file, "(");
        printDataType(file, lvalue->cvalue.dtypeEnum, lvalue->cvalue.any);
        fprintf(file, "[%lu])", lvalue->cvalue.arr->length->cvalue.u64);
    }
    
    fputc('{', file);

    switch (node->wideDtype) {

        case DT_UINT_8: {
            uint8_t* arr = (uint8_t*) node->wideStr;

            for (int i = 0; i < node->wideLen - 1; i++) {
                fprintf(file, "%u,", arr[i]);
            }
            fprintf(file, "%u", arr[node->wideLen - 1]);

            break;
        }

        case DT_UINT_16: {
            uint16_t* arr = (uint16_t*) node->wideStr;

            for (int i = 0; i < node->wideLen - 1; i++) {
                fprintf(file, "%u,", arr[i]);
            }
            fprintf(file, "%u", arr[node->wideLen - 1]);

            break;
        }

        case DT_UINT_32: {
            uint32_t* arr = (uint32_t*) node->wideStr;

            for (int i = 0; i < node->wideLen - 1; i++) {
                fprintf(file, "%u,", arr[i]);
            }
            fprintf(file, "%u", arr[node->wideLen - 1]);

            break;
        }

        case DT_UINT_64: {
            uint64_t* arr = (uint64_t*) node->wideStr;

            for (int i = 0; i < node->wideLen - 1; i++) {
                fprintf(file, "%lu,", arr[i]);
            }
            fprintf(file, "%lu", arr[node->wideLen - 1]);

            break;
        }

    }

    fputc('}', file);

}

void printArrayInitialization(FILE* file, int level, ArrayInitialization* const node, Variable* lvalue) {

    fputc('{', file);

    for (int i = 0; i < (int)node->attributes.size() - 1; i++) {

        Variable* const var = node->attributes[i];

        if (var->nameLen > 0) {
            fprintf(file, "%.*s_%i = ", var->nameLen, var->name, var->id);
            if (var->expression) {
                c_printExpression(file, level, var->expression);
            } else {
                printOperandValue(file, var);
            }
        } else {
            if (var->expression) {
                c_printExpression(file, level, var->expression);
            } else {
                printOperandValue(file, var);
            }
        }

        fputc(',', file);

    }

    if ((int) node->attributes.size() > 0) {

        Variable* const var = node->attributes[(int) node->attributes.size() - 1];

        if (var->nameLen > 0) {
            if (var->expression) {
                c_printExpression(file, level, var->expression);
            } else {
                printOperandValue(file, var);
            }
        } else {
            if (var->expression) {
                c_printExpression(file, level, var->expression);
            } else {
                printOperandValue(file, var);
            }
        }

    }

    fputc('}', file);

}

void printEnumerator(FILE* file, int level, Enumerator* const node, Variable* lvalue) {

    return;

    fprintf(tFile, "typedef enum %.*s_%id {", node->nameLen, node->name, node->id);
    
    for (int i = 0; i < node->vars.size(); i++) {
        
        Variable* var = node->vars[i];
        printVariable(tFile, level, var);

        if (var->expression) {
            fputc('=', tFile);
            c_printExpression(tFile, level, var->expression);
        }

        fputc(',', tFile);

    }

    fprintf(tFile, "} %.*s;", node->nameLen, node->name);

}

void printVariable(FILE* file, int level, Variable* const node, Variable* lvalue) {

    /*
    if (node->parentStruct) {
        node->parentStruct->print(&translatorC, file, level);
        fputc('.', file);
    }
    */

    /*
    for (int i = 0; i < node->scopeNames.size(); i++) {
        ScopeName* name = node->scopeNames[i];
        if (name->type != SC_ENUM) {
            fprintf(file, "%.*s_%i.", name->nameLen, name->name, name->id);
        }
    }
    */

    if (node->def && node->def->flags & IS_ARRAY_LIST) {
        if (node->snFlags & IS_SIZE) {
            Variable* tmp = node->def->var;
            fprintf(file, "(%.*s_%i->size)", tmp->nameLen, tmp->name, tmp->id);
        } else if (node->snFlags & IS_LENGTH) {
            Variable* tmp = node->def->var;
            fprintf(file, "(%.*s_%i->len)", tmp->nameLen, tmp->name, tmp->id);
        } else {
            fprintf(file, "(%.*s_%i->data)", node->nameLen, node->name, node->id);
        }
        return;
    }
    
    // maybe separate type?
    if (node->def && node->def->flags & IS_CMP_TIME && node->def->var->cvalue.dtypeEnum != DT_ARRAY) {
        printOperandValue(file, node->def->var);
    } else if (node->nameLen > 0) {
        if (node->cvalue.dtypeEnum == DT_FUNCTION) {
            fprintf(file, "&%.*s_%i", node->nameLen, node->name, node->id);
        } else {
            fprintf(file, "%.*s_%i", node->nameLen, node->name, node->id);
        }
    } else {

        if (node->snFlags & IS_LENGTH && node->def && node->def->var != lvalue) {
            Variable* const tmp = node->def->var;
            if (tmp->cvalue.arr->length && tmp->cvalue.arr->length->cvalue.hasValue) {
                printVariable(file, 0, tmp->cvalue.arr->length);
            } else {
                printArrayLenName(file, tmp);
            }
        } else if (node->expression && !node->cvalue.hasValue) {
            c_printExpression(file, level, node->expression, lvalue);
        } else {
            printOperandValue(file, node);
        }

        // if (node->expression) node->expression->print(&translatorC, file);
        // else c_printOperandValue(file, node);
    }

}

void printFullDataType(FILE* file, Variable* var) {

    // base dtype
    if (var->def->lastPtr) {
        Pointer* const ptr = var->def->lastPtr;
        printDataType(file, ptr->pointsToEnum, ptr->pointsTo);
    } else {
        printDataType(file, var->cvalue.dtypeEnum, var->cvalue.any);
        return;
    }

    Pointer* ptr = var->cvalue.ptr;
    int type = var->cvalue.dtypeEnum;

    // pointers
    while (ptr && type == DT_POINTER) {
        fputc('*', file);
        ptr = ptr->parentPointer;
        if (ptr) type = ptr->pointsToEnum;
    }

    // arrays
    while (ptr && type == DT_ARRAY) {
        fputc('[', file);
        printVariable(file, 0, ((Array*) ptr)->length);
        fputc(']', file);
        ptr = ptr->parentPointer;
        if (ptr) type = ptr->pointsToEnum;
    }

}

void printFunctionDefinition(FILE* file, Function* const node, const int printId) {

    printFullDataType(file, node->outArg->var);

    fprintf(file, " %.*s_%i(", node->nameLen, node->name, node->id);

    const int inArgCnt = ((int)node->inArgs.size());
    for (int i = 0; i < inArgCnt; i++) {
        
        VariableDefinition* const varDef = node->inArgs[i];

        printVariableDefinitionLValue(file, 0, varDef, 1, printId);
        // c_printDataType(file, varDef->var->cvalue.dtypeEnum, varDef->var->dtype);
        // fprintf(file, " %.*s_%i", varDef->var->nameLen, varDef->var->name, varDef->var->id);

        if (varDef->flags & IS_ARRAY) {
            fputc('[', fFile);
            if (varDef->flags & IS_CMP_TIME) {
                printOperandValue(file, varDef->var->cvalue.arr->length);
            }
            fprintf(file, "]");
        }

        if (varDef->var->cvalue.dtypeEnum == DT_ARRAY) {
            if (!(varDef->var->cvalue.arr->flags & IS_ARRAY_LIST)) {
                fputc(',', file);
                fprintf(file, "uint64_t ");
                printArrayLenName(file, varDef->var);
            }
        }

        if (i < inArgCnt - 1) {
            fputc(',', file);
        }
    
    }

    if (node->errorSet) {
        fprintf(file, ",int* err");
    }

    fputc(')', file);

}

void printFunction(FILE* file, int level, Function* const node, Variable* lvalue) {

    if (node->snFlags & IS_RENDERED) return;
    if (node->internalIdx == -1) return;

    printDebugLine(file, node->loc);

    printFunctionDefinition(fdFile, node);
    fputc(';', fdFile);

    printFunctionDefinition(fFile, node);
    printScope(fFile, level, node->bodyScope);

    node->snFlags |= IS_RENDERED;

}


void printBranch(FILE* file, int level, Branch* const node, Variable* lvalue) {

    // basic if branch
    // LOOK AT : maybe get rid of '()', as expression should have them already
    printDebugLine(file, node->loc);

    fprintf(file, "if (");
    printVariable(file, level, node->expressions[0]);
    fputc(')', file);
    printScope(file, level, node->scopes[0]);
    
    // if elses
    int i = 1;
    for (; i < node->expressions.size(); i++) {
        fprintf(file, "else if (");
        printVariable(file, level, node->expressions[i]);
        fputc(')', file);
        printScope(file, level, node->scopes[i]);
    }

    // final else if present
    if (i < node->scopes.size()) {
        fprintf(file, "else");
        printScope(file, level, node->scopes[(int) node->scopes.size() - 1]);
    }

}

void printSwitchCase(FILE* file, int level, SwitchCase* const node, Variable* lvalue) {
    
    printDebugLine(file, node->loc);
    fprintf(file, "switch (");
    printVariable(file, level, node->switchExp);
    fprintf(file, "){");

    for (int i = 0; i < node->cases.size(); i++) {
        fprintf(file, "case ");
        printVariable(file, level, node->casesExp[i]);
        fputc(':', file);
        printScope(file, level, node->cases[i]);
        fprintf(file, "break;");
    }

    fprintf(file, "default:");
    if (node->elseCase) printScope(file, level, node->elseCase);
    
    fputc('}', file);

}

void printWhileLoop(FILE* file, int level, WhileLoop* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);
    
    fprintf(file, "while(");
    printOperand(file, level, node->expression);
    fprintf(file, ")");
    
    printScope(file, level, node->bodyScope);

}


void printForLoop(FILE* file, int level, ForLoop* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);

    fprintf(file, "for(");

    if (node->initEx) printOperand(file, level, node->initEx);
    fputc(';', file);
    
    if (node->conditionEx) printOperand(file, level, node->conditionEx);
    fputc(';', file);
    
    if (node->actionEx) printOperand(file, level, node->actionEx);
    
    fputc(')', file);    
    printScope(file, level, node->bodyScope);

}

void printLoop(FILE* file, int level, Loop* node, Variable* lvalue) {

    printDebugLine(file, node->loc);

    fprintf(file, "for(");

    Variable* idxVar;
    Variable* sizeVar;

    if (node->idx) {
        idxVar = node->idx;
        printVariable(file, level, node->idx);
        fputc(';', file);
    } else {
        idxVar = node->idxDef->var;
        printVariableDefinition(file, level, node->idxDef);
    }

    printVariable(file, level, idxVar);
    fputc('<', file);
    // c_printOperand(file, level, node->array->allocSize);
    if (node->array->cvalue.arr->length) {
        printVariable(file, 0, node->array->cvalue.arr->length);
    } else {
        printArrayLenName(file, node->array);
    }
    // fprintf(file, "%i;", node->array->cvalue.arr->length);
    fputc(';', file);
    //c_printVariable(file, level, ((Array*) node->array); 
    
    //fputc(';', file);
    
    printVariable(file, level, idxVar);
    fprintf(file, "++");
    
    fputc(')', file);    
    printScope(file, level, node->bodyScope);

}

void printReturnStatement(FILE* file, int level, ReturnStatement* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);

    if (node->err) {
        fprintf(file, "*err = ");
        printVariable(file, level, node->err);
        fputc(';', file);
    }

    if (node->var) {
        fprintf(file, "return ");
        printVariable(file, level, node->var);
        fputc(';', file);
    } else {
        fprintf(file, "return;");
    }

}

void printContinueStatement(FILE* file, int level, ContinueStatement* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);
    fprintf(file, "continue;");

}

void printBreakStatement(FILE* file, int level, BreakStatement* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);
    fprintf(file, "break;");

}

void printGotoStatement(FILE* file, int level, GotoStatement* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);
    fprintf(file, "goto %.*s;", node->nameLen, node->name);

}

void printLabel(FILE* file, int level, Label* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);
    fprintf(file, "%.*s:", node->nameLen, node->name);

}

void printNamespace(FILE* file, int level, Namespace* const node, Variable* lvalue) {

    if (node->snFlags & IS_RENDERED) return;

    printDebugLine(file, node->loc);
    
    for (int i = 0; i < node->children.size(); i++) {
        // meh..
        // have to be accessble from parent 
        node->children[i]->scope = node->scope;
        c_print(file, level, node->children[i]);
        // node->children[i]->print(&translatorC, vFile, level);
    }

    node->snFlags |= IS_RENDERED;

}

void printWrapperExpression(FILE* file, int level, WrapperExpression* const node, Variable* lvalue) {
    
    printVariable(file, level, node->operand, lvalue);

}

void printExpressionWrapper(FILE *file, int level, ExpressionWrapper* const node, Variable *lvalue) {

    printVariable(file, level, node->operand);
    fputc(';', file);

}

void printConstExpression(FILE* file, int level, ConstExpression* const node, Variable* lvalue) {

}

void printOperatorExpression(FILE* file, int level, OperatorExpression* const node, Variable* lvalue) {

}

void printUnaryExpression(FILE* file, int level, UnaryExpression* const node, Variable* lvalue) {
    
    fputc('(', file);

    printOperator(file, node->operType);
    // node->oper->print(&translatorC, level);

    if (node->operand->unrollExpression && node->operand->expression) {
        c_printExpression(file, level, node->operand->expression);
    } else {
        printVariable(file, level, node->operand);
    }

    fputc(')', file);

}

void printBinaryExpression(FILE* file, int level, BinaryExpression* const node, Variable* lvalue) {

    if (node->operandA->cvalue.dtypeEnum == DT_ARRAY && IS_MEMBER_SELECTION(node->operType)) {
        
        fputc('(', file);
        printVariable(file, 0, node->operandA->cvalue.arr->length);
        fputc(')', file);

        return;
        
    }

    fputc('(', file);
    
    if (node->operandA->unrollExpression && node->operandA->expression) {
        c_printExpression(file, level, node->operandA->expression);
    } else {
        printVariable(file, level, node->operandA);
    }

    printOperator(file, node->operType);
    // node->oper->print(&translatorC, 1);
    
    if (node->operandB->unrollExpression && node->operandB->expression) {
        c_printExpression(file, level, node->operandB->expression);
    } else {
        printVariable(file, level, node->operandB);
    }

    if (node->operType == OP_SUBSCRIPT) fputc(']', file);

    fputc(')', file);

}

void printTernaryExpression(FILE* file, int level, TernaryExpression* const node, Variable* lvalue) {

}

void printStatement(FILE* file, int level, Statement* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);
    printVariable(file, level, node->op);
    fputc(';', file);

}

void printFunctionCall(FILE* file, Function* const node, Variable* lvalue) {
}

void printFunctionCall(FILE* file, int level, FunctionCall* const node, Variable* lvalue, Variable* err) {

    if (node->fptr) {

        fprintf(file, "%.*s_%i(", node->nameLen, node->name, node->fptr->id);
    
    } else if (node->fcn->internalIdx <= 0) {
        
        fprintf(file, "%.*s_%i(", node->nameLen, node->name, node->fcn->id);
    
    } else if (node->fcn->internalIdx == IF_ALLOC) {
        // first

        Variable* var = node->inArgs[0];
        const DataTypeEnum dtype = var->cvalue.dtypeEnum;

        fprintf(file, "malloc(sizeof(");

        if (dtype == DT_CUSTOM) {

            TypeDefinition* customDtype = var->cvalue.def;
            fprintf(file, "%.*s_%i", customDtype->nameLen, customDtype->name, customDtype->id);


            fprintf(file, "));");

            TypeDefinition* lvalueDtype = (TypeDefinition*) (lvalue->cvalue.ptr->pointsTo);
            if (var->expression) {
                TypeInitialization* typeInit = (TypeInitialization*)((WrapperExpression*)(var->expression))->operand->expression;
                for (int i = 0; i < typeInit->attributes.size(); i++) {
                    printVariable(file, level, lvalue);
                    fputc('-', file);
                    fputc('>', file);
                    printVariable(file, level, lvalueDtype->vars[typeInit->idxs[i]]);
                    fputc('=', file);
                    c_printExpression(file, level, typeInit->attributes[i]->expression);
                    fputc(';', file); // overprodeces ';', but who cares
                }
            }

        } else if (dtype == DT_ARRAY) {

            Array* arr = var->cvalue.arr;
            //printDataType(file, arr->pointsToEnum);
            printDataType(file, arr->pointsToEnum, arr->pointsTo);
            
            fprintf(file, ")*");
            //printArrayLenName(file, lvalue);
            printVariable(file, level, arr->length, lvalue);

            fprintf(file, ");");

            if (var->expression) {
                lvalue->expression = var->expression;
                printArray(file, lvalue, var);
            }

        } else {

            printDataType(file, dtype);
            fprintf(file, "));");
            if (var->cvalue.hasValue) {
                printVariable(file, level, lvalue);
                fputc('=', file);
                printVariable(file, level, var);
            } else if (var->expression) {
                fputc('*', file);
                printVariable(file, level, lvalue);
                fputc('=', file);
                printVariable(file, level, var);
            }

        }
        
        return;
    
    } else {
        
        fprintf(file, "%.*s(", node->nameLen, node->name);
    
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

    if (err) {
        fputc(',', file);
        fputc('&', file);
        printVariable(file, 0, err);
    }

    fputc(')', file);

}

void printOperand(FILE* file, int level, Operand* const node, Variable* lvalue) {

    // maybe separate type?
    if (node->expression) {
        c_printExpression(file, level, node->expression);
    } else {
        printOperandValue(file, node);
    }

}

void printUnion(FILE* file, int level, Union* const node, Variable* lvalue) {

    printTypeDefinition(file, level, node, lvalue);

}

void printErrorSet(FILE* file, int level, ErrorSet* const node, Variable* lvalue) {

    printDebugLine(file, node->loc);
    
    fprintf(vFile, "const int %.*s_%i=%lu;", node->nameLen, node->name, node->id, node->value);
    
    for (int i = 0; i < node->vars.size(); i++) {
        Variable* const var = node->vars[i];
        if (!(node->vars[i]->cvalue.hasValue)) continue;
        fprintf(vFile, "const int %.*s_%i=%lu;", var->nameLen, var->name, var->id, var->cvalue.u64 );
    }

}

void printUnaryOperator(FILE* file, int level, UnaryOperator* const node, Variable* lvalue) {

}

void printBinaryOperator(FILE* file, int level, BinaryOperator* const node, Variable* lvalue) {

}

void printTernaryOperator(FILE* file, int level, TernaryOperator* const node, Variable* lvalue) {

}



Translator translatorC {
    mFile,
    0,
    &c_init,
    &c_print,
    &c_printExpression,
    &c_printForeignCode,
    &c_exit
};
