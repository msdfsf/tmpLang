// This header contains all syntax related stuff that directly
// reprsents it or helps in some way.
// Most of stuff here are core parts of the system, so they are
// in one file as mainly used together anyway.
// May be splited or wrapped into more files/namespaces, but
// for now I feel like this is the most comfortable layaut at least
// for my self.

#pragma once

#include <string>
#include <cstdint>
#include <array>

#include "globals.h"
#include "operators.h"
#include "data_types.h"
#include "keywords.h"
#include "array_list.h"
#include "dynamic_arena.h"
#include "ordered_dict.h"




// =========================================
// FORWARD DECLARATIONS
// ===

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
struct Expression;
struct ConstExpression;
struct OperatorExpression;
struct UnaryExpression;
struct BinaryExpression;
struct TernaryExpression;
struct Statement;
struct FunctionCall;
struct ImportStatement;
struct Union;
struct GetLength;
struct GetSize;
struct Catch;
struct Using;

struct ScopeName;

struct IForeignCode;
struct CodeBlock;
struct ForeignFunction;
struct LangDef;

struct TypeDefinition;
struct Pointer;
struct Array;
struct Slice;

namespace Interpreter {
    struct ExeBlock;
}



// ======================================
// ENUMS & BASIC TYPES
// ===

typedef OrderedDict::Container ODictSyntaxNode;

struct DArrayVariable            { DArray::Container base; };
struct DArrayFunction            { DArray::Container base; };
struct DArrayUnion               { DArray::Container base; };
struct DArrayLabel               { DArray::Container base; };
struct DArrayErrorSet            { DArray::Container base; };
struct DArrayTypeDefinition      { DArray::Container base; };
struct DArrayEnumerator          { DArray::Container base; };
struct DArrayNamespace           { DArray::Container base; };
struct DArrayINamedLoc           { DArray::Container base; };
struct DArrayGotoStatement       { DArray::Container base; };
struct DArrayUsing               { DArray::Container base; };
struct DArrayValue               { DArray::Container base; };
struct DArrayScope               { DArray::Container base; };
struct DArraySyntaxNode          { DArray::Container base; };
struct DArrayLangDef             { DArray::Container base; };
struct DArrayCodeBlock           { DArray::Container base; };
struct DArrayForeignFunction     { DArray::Container base; };
struct DArrayVariableDefinition  { DArray::Container base; };
struct DArrayVariableAssignment  { DArray::Container base; };
struct DArrayLoop                { DArray::Container base; };
struct DArrayStatement           { DArray::Container base; };
struct DArrayReturnStatement     { DArray::Container base; };
struct DArraySwitchCase          { DArray::Container base; };
struct DArraySlice               { DArray::Container base; };
struct DArrayImportStatement     { DArray::Container base; };



typedef int AllocType;

enum NodeType : AllocType {
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
    NT_CODE_BLOCK,
    NT_ERROR,
    NT_UNION,
    NT_USING,
    NT_IMPORT,
    NT_COUNT
};

enum AllocExType : AllocType {
    AT_BYTE = NT_COUNT,
    AT_EXT_FUNCTION_CALL,
    AT_EXT_OPERATION,
    AT_EXT_UNARY,
    AT_EXT_BINARY,
    AT_EXT_TERNARY,
    AT_EXT_TYPE_INITIALIZATION,
    AT_EXT_STRING_INITIALIZATION,
    AT_EXT_ARRAY_INITIALIZATION,
    AT_EXT_SLICE,
    AT_EXT_CATCH,
    AT_EXT_CAST,
    AT_EXT_GET_LENGTH,
    AT_EXT_GET_SIZE,
    AT_INAMED,
    AT_POINTER,
    AT_ARRAY,
    AT_QUALIFIED_NAME,
    AT_FUNCTION_PROTOTYPE,
    AT_FOREIGN_FUNCTION,
    AT_COUNT
};

/*
enum ScopeType {
    SC_ENUM,
    SC_STRUCT,
    SC_NAMESPACE
};
*/

enum ScopeType {
    SC_GLOBAL = 1,
    SC_COMMON = 0
};

enum ExpressionType {
    EXT_FUNCTION_CALL = AT_EXT_FUNCTION_CALL,
    EXT_OPERATION,
    EXT_UNARY,
    EXT_BINARY,
    EXT_TERNARY,
    EXT_TYPE_INITIALIZATION,
    EXT_STRING_INITIALIZATION,
    EXT_ARRAY_INITIALIZATION,
    EXT_SLICE,
    EXT_CATCH,
    EXT_CAST,
    EXT_GET_LENGTH,
    EXT_GET_SIZE,
};

enum ExpressionQualifier {
    EXQ_VARIABLE,
    EXQ_CONSTANT,
    EXQ_COMPILE_TIME
};

struct QualifiedName : INamedEx {
    uint16_t pathSize;
    INamed* path;
};



// ======================================
//  SYNTAX NODES
// ===

struct SyntaxNode {

    static Scope* root;
    static INamed dir;

    // as files are parsed only once and linked via pointers
    // need option to get access to the original node
    SyntaxNode* ogNode = NULL;

    NodeType type;
    Scope* scope = NULL;
    Span* span;

    int parentIdx;
    uint64_t flags; // sn as syntax node

};

struct Scope {

    SyntaxNode base;

    // the function this scope is in
    // NULL if main
    Function* fcn = 0; // TODO : remove

    DArraySyntaxNode children;

    // helps to link variables with definitions.
    // nodes such as Enumerator, Scope, VariableDefinition...
    // so we can track their order and dont have to go through all childrens.
    // std::vector<Handler> defSearch;
    ODictSyntaxNode defSearch;

    // TODO : doing a lot of small alocations doesnt
    //        feel right, its better to treat it as block
    //        and create custom allocator underhood.
    union {
        DArray::Container arrays[10];
        struct {
            DArrayVariable       defs; // as vars, so can be searched by name
            DArrayFunction       fcns;
            DArrayUnion          unions;
            DArrayLabel          labels;
            DArrayErrorSet       customErrors;
            DArrayTypeDefinition customDataTypes; // TODO : do we need it?
            DArrayEnumerator     enums; // LOOK AT : maybe unite enum under Variable interface or something
            DArrayNamespace      namespaces;
            DArrayGotoStatement  gotos;
            DArrayUsing          usings;
        };
    };
};

struct Namespace {
    Scope scope;
    INamedEx name;
};

// only as defSearch
struct Using {
    SyntaxNode base;
    SyntaxNode* var; // node that is being included
};

// used for interoperability to keep code of the foreign language
struct IForeignCode {
    Span* tagLoc;

    String tagStr;
    String codeStr;
};

struct CodeBlock {
    SyntaxNode base;
    IForeignCode code;
};

struct Enumerator {
    SyntaxNode base;
    INamedEx name;

    DataTypeEnum dtype;
    DArrayVariable vars;
};

struct Statement {
    SyntaxNode base;
    Variable* operand;
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

struct Expression {
    ExpressionType type;
};

struct TypeInitialization {
    Expression base;

    DArrayVariable attributes;
    int* idxs; // maps attributes to og indicies in TypeDefinition

    // to fill the rest of the attributes with the same value
    // if NULL then no filling
    Variable* fillVar;
};

struct StringInitialization {
    Expression base;

    // pointer to start of the string in source file
    char* rawPtr;
    int rawPtrLen;

    // already escaped
    std::string rawStr;

    void* wideStr;
    DataTypeEnum wideDtype;
    int wideLen;
};

struct ArrayInitialization {
    Expression base;
    DArrayVariable attributes;
};

struct Catch {
    Expression base;

    FunctionCall* call;

    // its either one of these, other is NULL
    Variable* err;
    Scope* scope;
};

struct Cast {
    Expression base;
    DataTypeEnum target;
    Variable* operand;
};

struct GetLength {
    Expression base;
    Array* arr;
};

struct GetSize {
    Expression base;
    Array* arr;
};

struct VariableDefinition {
    SyntaxNode base;

    Variable* var; // it may be enough
    // int flags;


    // only for custom data types as they will be linked at the end
    QualifiedName* dtype;
    // char* dtypeName;
    //int dtypeNameLen;

    // if we have Pointer like definition as for example Foo****
    // then lastPtr points to the Foo*
    Pointer* lastPtr;
    // DataTypeEnum dtypeEnum;
    // TypeDefinition* dtype;
    //

    // local offset in vm
    uint64_t vmOffset;
};

struct VariableAssignment {
    SyntaxNode base;

    Variable* lvar;
    Variable* rvar;
    // Variable* offsetVar; // for arrays and maybe something else

    VariableAssignment();
    VariableAssignment(Span* span);
};

struct Variable {
    SyntaxNode base;

    VariableDefinition* def;

    Value cvalue; // c as compiler
    Value ivalue; // i as interpreter

    DArrayValue istack;

    int unrollExpression; // DEPRECATED: should be removed so code should rly fully on Value interface!!!!! LOOk AT : maybe get rid of Variable itself and use Variable instead as before? or have two types of operand constant and dynamic?
    Expression* expression;

    QualifiedName name;
};

struct FunctionPrototype {
    // TODO : is it realy needed
    int inArgsCnt; // number of arguments form perspective of the language
    DArrayVariableDefinition inArgs;
    VariableDefinition* outArg;
};

struct Function {
    SyntaxNode base;
    FunctionPrototype prototype;
    INamedEx name;

    DArrayReturnStatement returns;

    Scope* bodyScope;

    int internalIdx; // if it is > 0, then its internal function, and value represents unique id, otherwise should be ignored ***** TODO : for now value: -1 is used as identifer to not render function, fix it later *****

    int icnt = 0; // counter of function usage by interpreter
    int istackIdx = 0; // 0 is neutral value, no additional stack is used, so indexing is from 1

    QualifiedName* errorSetName;
    ErrorSet* errorSet;

    Interpreter::ExeBlock* exe;
};

struct ForeignFunction {
    Function fcn;
    IForeignCode code;
};

struct FunctionCall {
    Expression base;
    QualifiedName name;

    Function* fcn;
    Variable* fptr; // for function pointers, meh...
    int inArgsCnt; // same as Function::inArgsCnt
    DArrayVariable inArgs;
    Variable* outArg;
};

struct Branch {
    SyntaxNode base;

    DArrayScope scopes;
    DArrayVariable expressions;
};

struct SwitchCase {
    SyntaxNode base;

    Variable* switchExp;
    DArrayVariable casesExp;

    DArrayScope cases;
    Scope* elseCase;
};

struct WhileLoop {
    SyntaxNode base;

    Scope* bodyScope;
    Variable* expression;
};

// TODO : Deprecated
struct ForLoop {
    SyntaxNode base;

    Scope* bodyScope;

    Variable* initEx;
    Variable* conditionEx;
    Variable* actionEx;
};

struct Loop {
    SyntaxNode base;

    Scope* bodyScope;

    Variable* array;
    Variable* idx;
    VariableDefinition* idxDef;
    Variable* to;
};

struct ReturnStatement {
    SyntaxNode base;

    Function* fcn;
    Variable* var;
    Variable* err;

    int idx; // indexes itself in Function.returns
};

struct ContinueStatement {
    SyntaxNode base;
};

struct BreakStatement {
    SyntaxNode base;
};

struct GotoStatement {
    SyntaxNode base;
    INamed name;

    Span* span;
    Label* label;
};

struct Label {
    SyntaxNode base;
    INamedEx name;
    Span* span;
};

struct OperationExpression {
    Expression base;
    OperatorEnum opType;
};

struct UnaryExpression {
    OperationExpression base;
    Variable* operand;
};

struct BinaryExpression {
    OperationExpression base;
    Variable* left;
    Variable* right;
};

struct TernaryExpression {
    OperationExpression base;
    Variable* condition;
    Variable* trueExp;
    Variable* falseExp;
};

struct TypeDefinition {
    SyntaxNode base;
    DataType dtype;
    INamedEx name;

    DArrayVariable vars;

    Function* unaryPlus;
    Function* unaryMinus;
    Function* addition;
    Function* subtraction;
    Function* multiplication;
    Function* division;
    Function* modulo;
    Function* address;
    Function* subscript;
};

// TODO : unite under TypeDefinition?
struct Union {
    TypeDefinition base;
};

struct ErrorSet {
    SyntaxNode base;
    INamedEx name;

    uint64_t value;
    DArrayVariable vars;
};

struct Pointer {
    Pointer* parentPointer; // so we can walk back
    void* pointsTo;
    DataTypeEnum pointsToEnum;
};

struct Array {
    Pointer base;

    Variable* length;
    int flags;
};

struct Slice {
    Expression base;

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
    Keyword* dtypeMap;
    int dtypeMapLen;
};

struct ImportStatement {
    SyntaxNode base;

    // name of the importing file
    String fname;

    // defines how the file file content is wrapped
    // for now only KW_NAMESPACE, -1 is used for no wrapp
    Keyword keyword;

    // additional parrameter for ketWord
    // for KW_NAMESPACE its the name of the namespace
    String param;

    // root scope of the imports file
    // Scope* root;
};



// ======================================
// NODE REGISTRY
// ===

struct Foo {

    static struct {
    union {
        int asdasd;
        int dsadsa;
    };
    };

};

// declared as struct as we may need to provide possibility
// to the 'user' to sotre the registers after compilation
// and start the new one
struct _RegMemory {

    static void init();

    static constexpr size_t dataSize = 26;

    // contain references to nodes in linear way
    // so validator dont have to traverse tree that much
    union {

        // TODO : think of better name
        DArray::Container          data[dataSize];

        struct {
            DArrayLangDef              langDefs;
            DArrayCodeBlock            codeBlocks;
            DArrayForeignFunction      foreignFunctions;
            DArrayVariable             variables;
            DArrayVariable             fcnCalls;
            DArrayFunction             fcns;
            DArrayVariableDefinition   customDataTypesReferences;
            DArrayVariableAssignment   variableAssignments;
            DArrayVariable             cmpTimeVars;
            DArrayVariable             arrays;
            DArrayLoop                 loops;
            DArrayLabel                labels;
            DArrayVariable             branchExpressions;
            DArrayStatement            statements;
            DArrayVariableDefinition   initializations;
            DArrayReturnStatement      returnStatements;
            DArraySwitchCase           switchCases;
            DArrayVariableDefinition   variableDefinitions;
            DArrayErrorSet             customErrors;
            DArrayUnion                unions;
            DArraySlice                slices;
            DArrayVariableAssignment   arraysAllocations;
            DArrayImportStatement      imports;
            DArrayTypeDefinition       customDataTypes;
            DArrayEnumerator           enumerators;
            DArrayGotoStatement        gotos;
        };

    };

    struct Node {

        static void init(Scope* node);
        static void init(Namespace* node);
        static void init(Using* node);
        static void init(CodeBlock* node);
        static void init(Enumerator* node);
        static void init(Statement* node);
        static void init(VariableDefinition* node);
        static void init(VariableAssignment* node);
        static void init(Variable* node);
        static void init(Function* node);
        static void init(ForeignFunction* node);
        static void init(Branch* node);
        static void init(SwitchCase* node);
        static void init(WhileLoop* node);
        static void init(ForLoop* node);
        static void init(Loop* node);
        static void init(ReturnStatement* node);
        static void init(ContinueStatement* node);
        static void init(BreakStatement* node);
        static void init(GotoStatement* node);
        static void init(Label* node);
        static void init(TypeDefinition* node);
        static void init(Union* node);
        static void init(ErrorSet* node);
        static void init(ImportStatement* node);

        static void init(FunctionCall* node);
        static void init(OperationExpression* node);
        static void init(UnaryExpression* node);
        static void init(BinaryExpression* node);
        static void init(TernaryExpression* node);
        static void init(Catch* node);
        static void init(Cast* node);
        static void init(Slice* node);

        static void init(Pointer* node);
        static void init(Array* node);
        static void init(FunctionPrototype* node);
        static void init(ArrayInitialization* node);
        static void init(StringInitialization* node);
        static void init(TypeInitialization* node);
        static void init(QualifiedName* node);

        static Scope*              initScope();
        static Namespace*          initNamespace();
        static Using*              initUsing();
        static CodeBlock*          initCodeBlock();
        static Enumerator*         initEnumerator();
        static Statement*          initStatement();
        static VariableDefinition* initVariableDefinition();
        static VariableAssignment* initVariableAssignment();
        static Variable*           initVariable();
        static Function*           initFunction();
        static ForeignFunction*    initForeignFunction();
        static Branch*             initBranch();
        static SwitchCase*         initSwitchCase();
        static WhileLoop*          initWhileLoop();
        static ForLoop*            initForLoop();
        static Loop*               initLoop();
        static ReturnStatement*    initReturnStatement();
        static ContinueStatement*  initContinueStatement();
        static BreakStatement*     initBreakStatement();
        static GotoStatement*      initGotoStatement();
        static Label*              initLabel();
        static TypeDefinition*     initTypeDefinition();
        static Union*              initUnion();
        static ErrorSet*           initErrorSet();
        static ImportStatement*    initImportStatement();

        static FunctionCall*        initFunctionCall();
        static OperationExpression* initOperationExpression();
        static UnaryExpression*     initUnaryExpression();
        static BinaryExpression*    initBinaryExpression();
        static TernaryExpression*   initTernaryExpression();
        static Catch*               initCatch();
        static Cast*                initCast();
        static Slice*               initSlice();

        static Pointer*              initPointer();
        static Array*                initArray();
        static FunctionPrototype*    initFunctionPrototype();
        static ArrayInitialization*  initArrayInitialization();
        static StringInitialization* initStringInitialization();
        static TypeInitialization*   initTypeInitialization();
        static QualifiedName*        initQualifiedName();

        static Scope*              copy(Scope* node);
        static Namespace*          copy(Namespace* node);
        static Using*              copy(Using* node);
        static CodeBlock*          copy(CodeBlock* node);
        static Enumerator*         copy(Enumerator* node);
        static Statement*          copy(Statement* node);
        static VariableDefinition* copy(VariableDefinition* node);
        static VariableAssignment* copy(VariableAssignment* node);
        static Variable*           copy(Variable* node);
        static Function*           copy(Function* node);
        static ForeignFunction*    copy(ForeignFunction* node);
        static Branch*             copy(Branch* node);
        static SwitchCase*         copy(SwitchCase* node);
        static WhileLoop*          copy(WhileLoop* node);
        static ForLoop*            copy(ForLoop* node);
        static Loop*               copy(Loop* node);
        static ReturnStatement*    copy(ReturnStatement* node);
        static ContinueStatement*  copy(ContinueStatement* node);
        static BreakStatement*     copy(BreakStatement* node);
        static GotoStatement*      copy(GotoStatement* node);
        static Label*              copy(Label* node);
        static TypeDefinition*     copy(TypeDefinition* node);
        static Union*              copy(Union* node);
        static ErrorSet*           copy(ErrorSet* node);
        static ImportStatement*    copy(ImportStatement* node);

        static FunctionCall*           copy(FunctionCall* node);
        static OperationExpression*    copy(OperationExpression* node);
        static UnaryExpression*        copy(UnaryExpression* node);
        static BinaryExpression*       copy(BinaryExpression* node);
        static TernaryExpression*      copy(TernaryExpression* node);

        static Variable* copy(Variable* dest, Variable* src);
        static Variable* copyRef(Variable* dest, Variable* src);

    };
    Node Node;

    struct Find {

        static void* generic(DArray::Container* arr, String* name, MemberOffset mName);
        static void* generic(Scope* scope, String* name, MemberOffset mName, MemberOffset mArray);


        // Non-dynamic arrays usually don't occur, as the tree has to be ready
        // to be modified at any moment, so I won't predefine them.
        static Variable* inArray(Variable* arr, int arrLen, String* name);

        // Explicitly declared DArray find functions to reduce a bit of verbosity
        // and add some type safety
        static Variable*           inArray(DArrayVariable* arr, String* name);
        static Function*           inArray(DArrayFunction* arr, String* name);
        static Enumerator*         inArray(DArrayEnumerator* arr, String* name);
        static Namespace*          inArray(DArrayNamespace* arr, String* name);
        static Union*              inArray(DArrayUnion* arr, String* name);
        static ErrorSet*           inArray(DArrayErrorSet* arr, String* name);
        static TypeDefinition*     inArray(DArrayTypeDefinition* arr, String* name);
        static Label*              inArray(DArrayLabel* arr, String* name);
        static GotoStatement*      inArray(DArrayGotoStatement* arr, String* name);
        static Using*              inArray(DArrayUsing* arr, String* name);
        static ImportStatement*    inArray(DArrayImportStatement* arr, String* name);
        static CodeBlock*          inArray(DArrayCodeBlock* arr, String* name);
        static ForeignFunction*    inArray(DArrayForeignFunction* arr, String* name);
        static VariableDefinition* inArray(DArrayVariableDefinition* arr, String* name);

        // And 'full' functions that can be used to get the index. In fact, these alone
        // are sufficient, but I feel having both versions is more convenient.
        // There's something about going down the rabbit hole...
        static int inArray(DArrayVariable* arr, String* name, Variable** out);
        static int inArray(DArrayFunction* arr, String* name, Function** out);
        static int inArray(DArrayEnumerator* arr, String* name, Enumerator** out);
        static int inArray(DArrayNamespace* arr, String* name, Namespace** out);
        static int inArray(DArrayUnion* arr, String* name, Union** out);
        static int inArray(DArrayErrorSet* arr, String* name, ErrorSet** out);
        static int inArray(DArrayTypeDefinition* arr, String* name, TypeDefinition** out);
        static int inArray(DArrayLabel* arr, String* name, Label** out);
        static int inArray(DArrayGotoStatement* arr, String* name, GotoStatement** out);
        static int inArray(DArrayUsing* arr, String* name, Using** out);
        static int inArray(DArrayImportStatement* arr, String* name, ImportStatement** out);
        static int inArray(DArrayCodeBlock* arr, String* name, CodeBlock** out);
        static int inArray(DArrayForeignFunction* arr, String* name, ForeignFunction** out);
        static int inArray(DArrayVariableDefinition* arr, String* name, VariableDefinition** out);

        static Variable*       inScopeVariable(Scope* scope, String* name);
        static Function*       inScopeFunction(Scope* scope, String* name);
        static Union*          inScopeUnion(Scope* scope, String* name);
        static Label*          inScopeLabel(Scope* scope, String* name);
        static ErrorSet*       inScopeErrorSet(Scope* scope, String* name);
        static TypeDefinition* inScopeTypeDefinition(Scope* scope, String* name);
        static Enumerator*     inScopeEnumerator(Scope* scope, String* name);
        static Namespace*      inScopeNamespace(Scope* scope, String* name);
        static GotoStatement*  inScopeGotoStatement(Scope* scope, String* name);

    };
    Find Find;

};

extern _RegMemory Reg;



// ======================================
// INTERNAL PRE-DEFINED NODES
// ===

namespace Internal {

    const char IFS_PRINTF[] = "printf";
    const char IFS_ALLOC[]  = "malloc";
    const char IFS_FREE[]   = "free";

    const char IVS_NULL[]    = "null";
    const char IVS_TRUE[]    = "true";
    const char IVS_FALSE[]   = "false";

    enum VariableType {
      IV_NULL,
      IV_TRUE,
      IV_FALSE,
      IV_COUNT
    };

    enum FunctionType : uint32_t {
        IF_PRINTF = 1,
        IF_ALLOC  = 2,
        IF_FREE   = 3,
        IF_COUNT  = 4,
    };

    extern Variable variables[IV_COUNT];
    extern Function functions[IF_COUNT];

    extern Variable* zero;

    // each bit corresponds with the InternaFunction enum
    // indicates if function was used at least once in code
    extern uint64_t functionUsed;

    void init();

};



// ======================================
// NODE ALLOCATOR
// ===

constexpr int nodeTypeSize[AT_COUNT] = {
    sizeof(Scope),
    sizeof(VariableDefinition),
    sizeof(VariableAssignment),
    sizeof(TypeDefinition),
    sizeof(TypeInitialization),
    sizeof(Enumerator),
    sizeof(Variable),
    sizeof(Function),
    sizeof(Branch),
    sizeof(SwitchCase),
    sizeof(WhileLoop),
    sizeof(ForLoop),
    sizeof(Loop),
    sizeof(ReturnStatement),
    sizeof(ContinueStatement),
    sizeof(BreakStatement),
    sizeof(GotoStatement),
    sizeof(Label),
    sizeof(Namespace),
    sizeof(Statement),
    sizeof(CodeBlock),
    sizeof(ErrorSet),
    sizeof(Union),
    sizeof(Using),
    sizeof(ImportStatement),

    sizeof(uint8_t),

    sizeof(FunctionCall),
    sizeof(OperationExpression),
    sizeof(UnaryExpression),
    sizeof(BinaryExpression),
    sizeof(TernaryExpression),
    sizeof(TypeInitialization),
    sizeof(StringInitialization),
    sizeof(ArrayInitialization),
    sizeof(Slice),
    sizeof(Catch),
    sizeof(Cast),
    sizeof(GetLength),
    sizeof(GetSize),

    sizeof(INamed),
    sizeof(Pointer),
    sizeof(Array),
    sizeof(QualifiedName),
    sizeof(FunctionPrototype),
    sizeof(ForeignFunction),
};

// This is more specific version of the allocator to handle allocations
// of nodes defined here, so there is an abstraction how each node type
// is handled.
// Look at allocator.h to for the 'actual' allocator that this is based on
#if !defined(_CUSTOM_ALLOCATOR_)

    inline Arena::Container* nalc;

    inline void initNAlloc(Arena::Container* allocator) {
        nalc = alc;
    }

    inline void releaseNAlloc(Arena::Container* allocator) {
    }

    inline void* nalloc(Arena::Container* allocator, AllocType type) {
        return alloc(allocator, nodeTypeSize[type]);
    }

    inline void* nalloc(Arena::Container* allocator, AllocType type, size_t count) {
        return alloc(allocator, nodeTypeSize[type] * count);
    }

    inline void ndealloc(Arena::Container* allocator, void* ptr) {

    }

#endif
