// TODO : maybe exchange FILE for something generic and custom...
// NOTE : for now std::vectors/arrays are main collections as 
//        it's easy to work with and debug them. Later they will
//        be exchanged for more appropriate collections.

#pragma once

#include <vector>
#include <string>
#include <cstdint>
#include <array>
#include <unordered_map>

#include "globals.h"

#define IS_MEMBER_SELECTION(x) ((x) == OP_MEMBER_SELECTION || ((x) == OP_DEREFERENCE_MEMBER_SELECTION))

struct Translator;

enum InternalFunction : uint32_t;

enum KeyWordType : int;
struct KeyWord;

struct SyntaxNode;
struct Scope;
struct FunctionPrototype;
struct Namespace;
struct VariableDefinition;
struct VariableAssignment;
struct TypeDefinition;
struct TypeInitialization;
struct StringInitialization;
struct ArrayInitialization;
struct Enumerator;
struct Variable;
struct Function;
struct Branch;
struct SwitchCase;
struct WhileLoop;
struct ForLoop;
struct Loop;
struct ErrorSet;
struct ReturnStatement;
struct ContinueStatement;
struct BreakStatement;
struct GotoStatement;
struct Label;
struct WrapperExpression;
struct ExpressionWrapper;
struct Expression;
struct ConstExpression;
struct OperatorExpression;
struct UnaryExpression;
struct BinaryExpression;
struct TernaryExpression;
struct Statement;
struct Operator;
struct Operand;
struct FunctionCall;
struct UnaryOperator;
struct BinaryOperator;
struct TernaryOperator;
struct ImportStatement;
struct Union;
struct GetLength;
struct GetSize;
struct Catch;
struct Using;

struct ScopeName;
enum ScopeType : int;

struct IForeignCode;
struct CodeBlock;
struct ForeignFunction;
struct LangDef;

enum DataTypeEnum : int;
struct DataType;
struct TypeDefinition;
struct Pointer;
struct Array;
struct Slice;

enum ExpressionType : int;



// for now here because of cross reference stuff 
struct Translator {

    FILE* mainFile;
    int debugInfo;

    void (*init)                        (char* const dirName);
    void (*printNode)                   (FILE* file, int level, SyntaxNode* const node, Variable* lvalue);
    void (*printExpression)             (FILE* file, int level, Expression* const node, Variable* lvalue);
    void (*printForeignCode)            ();
    void (*exit)                        ();

};



enum NodeType : int {
    NT_SCOPE,
    NT_VARIABLE_DEFINITION,
    NT_VARIABLE_ASSIGNMENT,
    NT_TYPE_DEFINITION,
    NT_TYPE_INITIALIZATION,
    NT_ENUMERATOR,
    NT_VARIABLE,
    NT_FUNCTION,
    NT_BRANCH,
    NT_SWITCH_CASE,
    NT_WHILE_LOOP,
    NT_FOR_LOOP,
    NT_LOOP,
    NT_RETURN_STATEMENT,
    NT_CONTINUE_STATEMENT,
    NT_BREAK_STATEMENT,
    NT_GOTO_STATEMENT,
    NT_LABEL,
    NT_NAMESPACE,
    NT_STATEMENT,
    NT_FUNCTION_CALL,
    NT_OPERAND,
    NT_UNARY_OPERATOR,
    NT_BINARY_OPERATOR,
    NT_TERNARY_OPERATOR,
    NT_CODE_BLOCK,
    NT_EXPRESSION_WRAPPER,
    NT_ERROR,
    NT_UNION,
    NT_USING,
    NT_IMPORT,
};



// each bit corresponds with the InternaFunction enum
// indicates if function was used at least once in code
extern uint64_t internalFunctionUsed;

enum InternalFunction : uint32_t {
    IF_PRINTF = 1,
    IF_ALLOC  = 2,
    IF_FREE   = 3,
};



// ================================= //
//  Section:
//    KEY WORDS
// ================================= //

enum KeyWordType : int {
    KW_VOID,
    KW_INT,
    KW_INT_8,
    KW_INT_16,
    KW_INT_32,
    KW_INT_64,
    KW_UINT_8,
    KW_UINT_16,
    KW_UINT_32,
    KW_UINT_64,
    KW_FLOAT_32,
    KW_FLOAT_64,
    KW_STRING,
    KW_CMP_TIME,
    KW_CONST,
    KW_FUNCTION,
    KW_IF,
    KW_FOR,
    KW_WHILE,
    KW_GOTO,
    KW_ENUM,
    KW_TYPE_DEF,
    KW_RETURN,
    KW_CONTINUE,
    KW_LOOP,
    KW_BREAK,
    KW_USING,
    KW_NAMESPACE,
    KW_ELSE,
    KW_SWITCH_CASE,
    KW_SWITCH_CASE_CASE,
    KW_ALLOC,
    KW_FREE,
    KW_ERROR,
    KW_UNION,
    KW_CATCH,
    KW_IMPORT,
    KW_SCOPE,
    KW_AUTON,
    KW_MUTON,
};

enum TypedefKeyWord {
    TKW_STRUCT,
    TKW_UNION,
};

enum DirectiveKeyWord : int {
    DKW_LANG_DEF,
    DKW_IMPORT
};

struct KeyWord {
    int type;
    const char* str;
};

enum InternalVariablesEnum : int {
    IV_NULL,
    IV_TRUE,
    IV_FALSE
};

// internal Variables such as null, true, false etc...
extern Variable* internalVariables[];
extern const int internalVariablesCount;

// ================================= //
//  Section:
//    OPERATORS
// ================================= //

// also indexes operators array
enum OperatorEnum {
    OP_NONE = -1,
    OP_UNARY_PLUS = 0,
    OP_UNARY_MINUS,
    OP_ADDITION,
    OP_SUBTRACTION,
    OP_MULTIPLICATION,
    OP_DIVISION,
    OP_MODULO,
    OP_GET_ADDRESS,
    OP_GET_VALUE,
    OP_BITWISE_AND,
    OP_BITWISE_OR,
    OP_BITWISE_XOR,
    OP_BITWISE_NEGATION,
    OP_SHIFT_RIGHT,
    OP_SHIFT_LEFT,
    OP_EQUAL,
    OP_NOT_EQUAL,
    OP_LESS_THAN,
    OP_GREATER_THAN,
    OP_LESS_THAN_OR_EQUAL,
    OP_GREATER_THAN_OR_EQUAL,
    OP_BOOL_AND,
    OP_BOOL_OR,
    OP_INCREMENT,
    OP_DECREMENT,
    OP_SUBSCRIPT,
    OP_MEMBER_SELECTION,
    OP_DEREFERENCE_MEMBER_SELECTION,
    OP_NEGATION,
    OP_CONCATENATION,
    // OP_CAST // to tie cast to dtype, not sure about it as operator, but lets see
};
/*
struct OperatorMap {

    int (*unaryPlus) (Operand* op);
    int (*unaryMinus) (Operand* op);
    int (*addition) (Operand* a, Operand* b);
    int (*subtraction) (Operand* a, Operand* b);
    int (*multiplication) (Operand* a, Operand* b);
    int (*division) (Operand* a, Operand* b);
    int (*modulo) (Operand* a, Operand* b);
    int (*address) (Operand* op);
    int (*subscript) (Operand* a, Operand* b);
    int (*memberSelection) (Operand* ans, Operand* a, Operand* b);
    int (*equal) (Operand* a, Operand* b);
    int (*notEqual) (Operand* a, Operand* b);
    int (*lessThan) (Operand* a, Operand* b);
    int (*greaterThan) (Operand* a, Operand* b);
    int (*lessThanOrEqual) (Operand* a, Operand* b);
    int (*greaterThanOrEqual) (Operand* a, Operand* b);
    int (*boolAnd) (Operand* a, Operand* b);
    int (*boolOr) (Operand* a, Operand* b);

    constexpr OperatorMap() : 
        unaryPlus(NULL), 
        unaryMinus(NULL),
        addition(NULL), 
        subtraction(NULL),
        multiplication(NULL), 
        division(NULL),
        modulo(NULL),
        address(NULL),
        subscript(NULL),
        memberSelection(NULL),
        equal(NULL),
        notEqual(NULL),
        lessThan(NULL),
        greaterThan(NULL),
        lessThanOrEqual(NULL),
        greaterThanOrEqual(NULL),
        boolAnd(NULL),
        boolOr(NULL)
    {
    };
    
};
*/

struct Operator {
    
    uint32_t word;
    int rank; // precedence of operator, zero->positive-whatever, high->low *(precedence seems too long for usage)
    uint64_t flag;

    constexpr Operator(uint32_t w, int r, uint64_t f) : word(w), rank(r), flag(f) {}
    
};

// indexed by OperatorEnum
// !!! CAUTION : on change update :
//               operatorFunctions in interpreter.cpp
//               ... maybe some other stuff ...
constexpr auto operators = std::to_array<Operator>({
    
    // OP_UNARY_PLUS
    {
        '+',
        1,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_UNARY_MINUS
    {
        '-',
        1,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_ADDITION
    {
        '+',
        3,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_SUBTRACTION
    {
        '-',
        3,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_MULTIPLICATION
    {
        '*',
        2,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_DIVISION
    {
        '/',
        2,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_MODULO
    {
        '%',
        2,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_GET_ADDRESS
    {
        '&',
        1,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_GET_VALUE
    {
        '^',
        1,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_BITWISE_AND
    {
        '&',
        7,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_BITWISE_OR
    {
        '|',
        9,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_BITWISE_XOR
    {
        '^',
        8,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_BITWISE_NEGATION
    {
        '~',
        1,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_SHIFT_RIGHT
    {
        toDoubleChar('>', '>'),
        4,
        IS_BINARY | IS_TWO_CHAR
    },

    // OP_SHIFT_LEFT
    {
        toDoubleChar('<', '<'),
        4,
        IS_BINARY | IS_TWO_CHAR
    },

    // OP_EQUAL
    {
        toDoubleChar('=', '='),
        6,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_NOT_EQUAL
    {
        toDoubleChar('!', '='),
        6,
        IS_BINARY | IS_TWO_CHAR
    },

    // OP_LESS_THAN
    {
        '<',
        5,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_GREATER_THAN
    {
        '>',
        5,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_LESS_THAN_OR_EQUAL
    {
        toDoubleChar('<', '='),
        5,
        IS_BINARY | IS_TWO_CHAR
    },

    // OP_GREATER_THAN_OR_EQUAL
    {
        toDoubleChar('>', '='),
        5,
        IS_BINARY | IS_TWO_CHAR
    },

    // OP_BOOL_AND
    {
        toDoubleChar('&', '&'),
        10,
        IS_BINARY | IS_TWO_CHAR
    },

    // OP_BOOL_OR
    {
        toDoubleChar('|', '|'),
        11,
        IS_UNARY | IS_TWO_CHAR
    },

    // OP_INCREMENT
    {
        toDoubleChar('+', '+'),
        0,
        IS_UNARY | IS_TWO_CHAR
    },

    // OP_DECREMENT
    {
        toDoubleChar('-', '-'),
        0,
        IS_UNARY | IS_TWO_CHAR
    },

    // OP_SUBSCRIPT
    {
        '[',
        0,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_MEMBER_SELECTION
    {
        '.',
        0,
        IS_BINARY | IS_ONE_CHAR
    },

    // OP_DEREFERENCE_MEMBER_SELECTION
    {
        '.',
        0,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_NEGATION
    {
        '!',
        1,
        IS_UNARY | IS_ONE_CHAR
    },

    // OP_CONCATENATION
    {
        toDoubleChar('.', '.'),
        4,
        IS_BINARY | IS_TWO_CHAR
    }

});

const std::size_t OPERATORS_COUNT = operators.size();



// ================================= //
//  Section:
//    SYNTAX NODES
// ================================= //

struct SyntaxNode {

    static Scope* root;
    static INamed* dir;

    static std::vector<LangDef*> langDefs;
    static std::vector<CodeBlock*> codeBlocks;
    static std::vector<ForeignFunction*> foreignFunctions;

    static std::vector<Variable*> variables;
    static std::vector<Variable*> fcnCalls;
    static std::vector<Function*> fcns;
    static std::vector<VariableDefinition*> customDataTypesReferences;
    static std::vector<VariableAssignment*> variableAssignments;
    static std::vector<Variable*> cmpTimeVars;
    static std::vector<Variable*> arrays;
    static std::vector<Loop*> loops;
    static std::vector<Label*> labels;
    static std::vector<Variable*> branchExpressions;
    static std::vector<Statement*> statements;
    static std::vector<VariableDefinition*> initializations;
    static std::vector<ReturnStatement*> returnStatements;
    static std::vector<SwitchCase*> switchCases;
    static std::vector<VariableDefinition*> variableDefinitions;

    static std::vector<ErrorSet*> customErrors;
    static std::vector<Union*> unions;

    static std::vector<Slice*> slices;

    static std::vector<VariableAssignment*> arraysAllocations;

    static std::vector<ImportStatement*> imports;

    static std::vector<TypeDefinition*> customDataTypes;
    static std::vector<Enumerator*> enumerators;

    static std::vector<GotoStatement*> gotos;

    // as files are parsed only once and linked via pointers
    // need option to get access to the original node
    SyntaxNode* ogNode = NULL;
    
    NodeType type;
    Scope* scope;
    Location* loc;

    int parentIdx;
    uint64_t snFlags; // sn as syntax node

    SyntaxNode(NodeType nodeType) { type = nodeType; snFlags = 0; };

};

enum ScopeType : int {
    SC_GLOBAL = 1,
    SC_COMMON = 0
};

struct Scope : SyntaxNode {

    // the function this scope is in
    // NULL if main
    Function* fcn; // TODO : remove

    std::vector<SyntaxNode*> children;

    // LOOK AT : unite?
    //std::vector<Variable*> vars;
    //std::vector<VariableDefinition*> defs;
    //std::vector<Scope*> scopes;

    // helps to link variables with definitions.
    // nodes such as Enumerator, Scope, VariableDefinition...
    // so we can track their order and dont have to go through all childrens.
    // std::vector<SyntaxNode*> defSearch;
    std::unordered_map<std::string_view, SyntaxNode*> defSearch;

    std::vector<Variable*> defs; // as vars, so can be searched by name
    std::vector<Function*> fcns;
    std::vector<Union*> unions;
    std::vector<Label*> labels;
    std::vector<ErrorSet*> customErrors;
    std::vector<TypeDefinition*> customDataTypes; // TODO : do we need it?
    std::vector<Enumerator*> enums; // LOOK AT : maybe unite enum under Variable interface or something
    std::vector<Namespace*> namespaces;
    std::vector<GotoStatement*> gotos;
    std::vector<Using*> usings;

    Scope() : SyntaxNode(NT_SCOPE) {};

};

struct Namespace : Scope, INamedEx {

};

// TODO : think of better name
struct INamedVar : INamedEx {
    std::vector<INamedLoc*> scopeNames;
};

int validateScopeNames(Scope* sc, std::vector<INamedLoc*> names, Namespace** nspace, ErrorSet** eset);
int getLValueLocation(DataTypeEnum dtype, void* dtypeDef, Location* loc); // for error purposes, for now here
/*
struct ScopeName : INamedEx {
    // we need to know which scope name we are dealing with to adjust render as 
    // different languages can have different symbols for accessing different 
    // things, or dont have any at all (enums in C)
    ScopeType type;

    // hmm actualy dont know where to put this function and if its needed
    // but for now lets keep it here, so its not waving around as lil lost kitty
    static int validateScopeNames(std::vector<ScopeName*> names, TypeDefinition** dtype);
};

enum ScopeType : int {
    SC_ENUM,
    SC_STRUCT,
    SC_NAMESPACE
};
*/

// only as defSearch
struct Using : SyntaxNode {
    Using() : SyntaxNode(NT_USING) {};
    SyntaxNode* var; // node that is being included
};

// used for interoperability to keep code of the foreign language
struct IForeignCode {
    
    Location* tagLoc;

    char* tagStr;
    int tagLen;

    char* codeStr;
    int codeLen;

};

struct CodeBlock : IForeignCode, SyntaxNode {

    CodeBlock() : SyntaxNode(NT_CODE_BLOCK) {};

};

struct Enumerator : SyntaxNode, INamedEx {
    
    DataTypeEnum dtype;
    std::vector<Variable*> vars;

    Enumerator() : SyntaxNode(NT_ENUMERATOR) {};

};

struct Value {

    DataTypeEnum dtypeEnum;
    int hasValue = 0;
    union {
        int8_t      i8;
        int16_t     i16;
        int32_t     i32;
        int64_t     i64;
        uint8_t     u8;
        uint16_t    u16;
        uint32_t    u32;
        uint64_t    u64;
        float_t     f32;
        double_t    f64;
        Pointer*    ptr;
        Array*      arr;
        Slice*      slc;
        ErrorSet*   err;
        Enumerator* enm;
        TypeDefinition* def;
        FunctionPrototype* fcn;
        void*       str;
        void*       any;
    };

};

struct Operand : SyntaxNode {

    VariableDefinition* def;

    Value cvalue; // c as compiler
    Value ivalue; // i as interpreter
    // TODO : isnt it part of cvalue?
    
    // void* dtype; // !!! if DT_POINTER points to Pointer struct, if DT_ARRAY points to Array struct, if DT_CUSTOM points to TypeDefinition otherwise points to DataType !!!

    std::vector<Value> istack;
    // std::vector<ScopeName*> scopeNames;

    int unrollExpression; // DEPRECATED: should be removed so code should rly fully on Value interface!!!!! LOOk AT : maybe get rid of Operand itself and use Variable instead as before? or have two types of operand constant and dynamic?
    Expression* expression;

    Operand();
    Operand(Scope* scope);

};

enum ExpressionType :int {
    EXT_FUNCTION_CALL,
    EXT_UNARY,
    EXT_BINARY,
    EXT_TERNARY,
    EXT_TYPE_INITIALIZATION,
    EXT_STRING_INITIALIZATION,
    EXT_ARRAY_INITIALIZATION,
    EXT_SLICE,
    EXT_CATCH,
    EXT_GET_LENGTH,
    EXT_GET_SIZE,
    EXT_WRAPPER,
};

enum ExpressionQualifire {
    EXQ_VARIABLE,
    EXQ_CONSTANT,
    EXQ_COMPILE_TIME
};

struct Expression {

    ExpressionType type;

};

struct Statement : SyntaxNode {
    Variable* op;
    Statement() : SyntaxNode(NT_STATEMENT) {};
};
// LOOK AT : is there a better way
/*
struct VariableDefiniton : SyntaxNode {
    
    Variable* var;
    int flags;

    VariableDefiniton() {};
    VariableDefiniton(Location* loc);
    virtual void print(int level);

};
*/

struct TypeInitialization : Expression {

    TypeInitialization() {
        type = EXT_TYPE_INITIALIZATION;
    };

    std::vector<Variable*> attributes;
    int* idxs; // maps attributes to og indicies in TypeDefinition 

    // to fill the rest of the attributes with the same value
    // if NULL then no filling
    Variable* fillVar;

};

struct StringInitialization : Expression {

    StringInitialization() {
        type = EXT_STRING_INITIALIZATION;
    };

    // pointer to start of the string in source file
    char* rawPtr;
    int rawPtrLen;

    // already escaped
    std::string rawStr;

    void* wideStr;
    DataTypeEnum wideDtype;
    int wideLen;
    
};

struct ArrayInitialization : Expression {

    ArrayInitialization() {
        type = EXT_ARRAY_INITIALIZATION;
    };

    std::vector<Variable*> attributes;

};

struct Catch : Expression {
    
    Catch() {
        type = EXT_CATCH;
    };

    FunctionCall* call;
    
    // its either one of these, other is NULL
    Variable* err;
    Scope* scope;

};

struct GetLength : Expression {
    Array* arr;
};

struct GetSize : Expression {
    Array* arr;
};

struct VariableDefinition : SyntaxNode {
     
    Variable* var; // it may be enough
    int flags;


    // only for custom data types as they will be linked at the end
    INamedVar* dtype;
    // char* dtypeName;
    //int dtypeNameLen;
    
    // if we have Pointer like definition as for example Foo****
    // then lastPtr points to the Foo*
    Pointer* lastPtr;
    // DataTypeEnum dtypeEnum;
    // TypeDefinition* dtype;

    VariableDefinition();
    VariableDefinition(Location* loc);
    VariableDefinition(Variable* var, int flags);

};

struct VariableAssignment : SyntaxNode {
    
    Variable* lvar;
    Variable* rvar;
    // Operand* offsetVar; // for arrays and maybe something else

    VariableAssignment();
    VariableAssignment(Location* loc);

};

extern Variable* zero;
struct Variable : INamedVar, Operand {

    // Variable* parentStruct;
    // Variable* attribute;
    // Variable* allocSize; // LOOK AT : dunno about this

    // std::vector<INamed*> scopeNames;

    uint64_t flags = 0; // TODO : get rid of, didn't helped to solve the problem

    Variable();
    Variable(Location* loc);
    Variable(Scope* scope);
    Variable(Scope* const sc, DataTypeEnum dtype);
    Variable(Scope* const sc, DataTypeEnum dtype, Location* loc);
    Variable(Scope* const sc, DataTypeEnum dtype, char* name, int nameLen);
    Variable(Scope* const sc, Value value, char* name, int nameLen);
    Variable(Variable* var);

};

struct FunctionPrototype {
    int inArgsCnt; // number of arguments form prespective of the language
    std::vector<VariableDefinition*> inArgs;
    VariableDefinition* outArg;
};

struct Function : SyntaxNode, FunctionPrototype, INamedEx {
    
    //std::vector<VariableDefinition*> inArgs;
    //Value outArg;
    //std::vector<DataTypeEnum> outArgs; // TODO : !!!!
    std::vector<ReturnStatement*> returns;

    Scope* bodyScope;
    
    int internalIdx; // if it is > 0, then its internal function, and value represents unique id, otherwise should be ignored ***** TODO : for now value: -1 is used as identifer to not render function, fix it later ***** 
    
    int icnt = 0; // counter of function usage by interpreter
    int istackIdx = 0; // 0 is neutral value, no additional stack is used, so indexing is from 1
    
    INamedVar* errorSetName;
    ErrorSet* errorSet;
    /*
    char* tagStr;
    int tagLen;

    char* codeStr;
    int codeLen;
    */

    Function();
    Function(Scope* sc, char* name, int nameLen, std::vector<VariableDefinition*> inArgs, VariableDefinition* outArg, int internalIdx);

};

struct ForeignFunction : Function, IForeignCode {

};

struct FunctionCall : Expression, INamedVar {

    FunctionCall() {
        type = EXT_FUNCTION_CALL;
    };

    Function* fcn;
    Variable* fptr; // for function pointers, meh...
    int inArgsCnt; // same as Function::inArgsCnt
    std::vector<Variable*> inArgs;
    Variable* outArg;

};

struct Branch : SyntaxNode {

    std::vector<Scope*> scopes;
    std::vector<Variable*> expressions;
    
    Branch() : SyntaxNode(NT_BRANCH) {};

};

struct SwitchCase : SyntaxNode {
   
    Variable* switchExp;
    std::vector<Variable*> casesExp;

    std::vector<Scope*> cases;
    Scope* elseCase;

    SwitchCase() : SyntaxNode(NT_SWITCH_CASE) {};

};

struct WhileLoop : SyntaxNode {

    Scope* bodyScope;
    Variable* expression;

    WhileLoop() : SyntaxNode(NT_WHILE_LOOP) {};
    
};

struct ForLoop : SyntaxNode {

    Scope* bodyScope;

    Variable* initEx;
    Variable* conditionEx;
    Variable* actionEx;

    ForLoop() : SyntaxNode(NT_FOR_LOOP) {};
    
};

struct Loop : SyntaxNode {
    
    Scope* bodyScope;

    Variable* array;
    Variable* idx;
    VariableDefinition* idxDef;

    Loop() : SyntaxNode(NT_LOOP) {};

};

struct ReturnStatement : SyntaxNode {

    Function* fcn;
    Variable* var;
    Variable* err;
    // std::vector<Variable*> vars;

    int idx; // indexes itself in Function.returns

    ReturnStatement() : SyntaxNode(NT_RETURN_STATEMENT) {};
    
};

struct ContinueStatement : SyntaxNode {

    ContinueStatement() : SyntaxNode(NT_CONTINUE_STATEMENT) {};
    
};

struct BreakStatement : SyntaxNode {

    BreakStatement() : SyntaxNode(NT_BREAK_STATEMENT) {};

};

struct GotoStatement : SyntaxNode, INamed {

    Location* loc;
    Label* label;
    
    GotoStatement() : SyntaxNode(NT_GOTO_STATEMENT) {};

};

struct Label : SyntaxNode, INamed {

    Location* loc;

    Label() : SyntaxNode(NT_LABEL) {};

};

// LOOK AT : think about better name
// what about ArithmeticExpression??
struct OperatorExpression : Expression {
    OperatorEnum operType;
    // Operator* oper;
};

struct ExpressionWrapper : SyntaxNode {
    Variable* operand;
    ExpressionWrapper() : SyntaxNode(NT_EXPRESSION_WRAPPER) {};
};

// LOOK AT : think about better name
struct WrapperExpression : Expression {
    
    WrapperExpression() {
        type = EXT_WRAPPER;
    };

    Variable* operand;
    
};

struct UnaryExpression : OperatorExpression {
    
    UnaryExpression() {
        type = EXT_UNARY;
    };

    Variable* operand;

};

struct BinaryExpression : OperatorExpression {

    BinaryExpression() {
        type = EXT_BINARY;
    };

    Variable* operandA;
    Variable* operandB;

};

struct TernaryExpression : BinaryExpression {

    TernaryExpression() {
        type = EXT_TERNARY;
    };

    Variable* operandA;
    Variable* operandB;
    Variable* operandC;

};



// ================================= //
//  Section:
//    DATA TYPES
// ================================= //

enum DataTypeEnum : int {
    DT_VOID = 0,
    DT_INT,
    DT_I8,
    DT_I16,
    DT_I32,
    DT_I64,
    DT_U8,
    DT_U16,
    DT_U32,
    DT_U64,
    DT_F32,
    DT_F64,
    DT_STRING,
    DT_POINTER,
    DT_ARRAY,
    DT_SLICE,
    DT_MULTIPLE_TYPES,
    DT_CUSTOM,
    DT_UNION,
    DT_ERROR,
    DT_MEMBER, // dont know about this one, represents the right side of member reference operator 'point . x'
    DT_ENUM,
    DT_FUNCTION, // FunctionPrototype
    DT_UNDEFINED
};

const int DATA_TYPES_COUNT = DT_UNDEFINED + 1;

#define IS_INT(x) ((x) >= DT_INT && (x) <= DT_UINT_64)
#define IS_SIGNED_INT(x) ((x) >= DT_INT && (x) <= DT_INT_64)
#define IS_UNSIGNED_INT(x) ((x) >= DT_UINT_8 && (x) <= DT_UINT_64)
#define IS_FLOAT(x) ((x) >= DT_FLOAT_32 && (x) <= DT_FLOAT_64)

struct DataType : INamedEx {
    
    int size; // in bytes
    int rank;

    constexpr DataType() : 
            
        size(0), 
        rank(0),
        
        INamedEx(NULL, 0, 0)

    {
    
    };

    constexpr DataType(char* const wd, const int wdLen, const int sz, const int rk) : 
        
        size(sz), 
        rank(rk),
        
        INamedEx(wd, wdLen, 0)

    {

    };

    ~DataType() {};

};

extern DataType dataTypes[DATA_TYPES_COUNT];

struct TypeDefinition : DataType, SyntaxNode {
    
    std::vector<Variable*> vars;

    Function* unaryPlus;
    Function* unaryMinus;
    Function* addition;
    Function* subtraction;
    Function* multiplication;
    Function* division;
    Function* modulo;
    Function* address;
    Function* subscript;

    TypeDefinition() : SyntaxNode(NT_TYPE_DEFINITION) {};

};

// TODO : unite under TypeDefinition?
struct Union : TypeDefinition {

    Union() : TypeDefinition() {
        type = NT_UNION;
    };

};

struct ErrorSet : SyntaxNode, INamedEx {

    uint64_t value;
    std::vector<Variable*> vars;

    ErrorSet() : SyntaxNode(NT_ERROR) {};

};

struct Pointer {
    Pointer* parentPointer; // so we can walk back
    void* pointsTo;
    DataTypeEnum pointsToEnum;
};

struct Array : Pointer {
    Variable* length;
    int flags;
};

struct Slice : Expression {

    Slice() {
        type = EXT_SLICE;
    }

    Variable* arr;
    Variable* bidx;
    Variable* eidx;

    Variable* len = NULL;

};






struct LangDef {

    String tag;
    
    // compile command used to compile foreign language
    String cmpCommand;

    // 
    String fcnFormat;

    // 
    String fcnFormatInArgs;

    // 
    String fcnFormatOutArgs;

    // for now just simple solution, type is used as length
    KeyWord* dtypeMap;
    int dtypeMapLen;

};

struct ImportStatement : SyntaxNode {

    // name of the importing file
    String fname;

    // defines how the file file content is wrapped
    // for now only KW_NAMESPACE, -1 is used for no wrapp
    KeyWordType keyWord;

    // additional parrameter for ketWord
    // for KW_NAMESPACE its the name of the namespace
    String param;

    // root scope of the imports file
    // Scope* root;

    ImportStatement() : SyntaxNode(NT_IMPORT) {};

};

