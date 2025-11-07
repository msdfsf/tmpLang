#include "translator.h"
#include "../../src/syntax.h"
#include "lsp.h"

#include <string>

// Here we dont expect a syntax representation
// to be valid. So just try to render as much
// as possible, and avoid crashes.

// We dont reuse the translator interface here
// as its will not be any beneficial.

// As we need to specify length of a message in
// advance, we cannot just write directly to stdout.
// So this is our global buffer for messages.
// Idea is to avoid allocations and just reuse it
// hoping that std::string will do a good job as
// i am lazy ass to implement it myself.
std::string buffer;

String LspRenderer::getBuffer() {
    return { buffer.data(), buffer.size() };
}

void LspRenderer::clearBuffer() {
    buffer.clear();
}


void printScope(Scope* const node, Variable* lvalue = NULL);
void printVariableDefinition(VariableDefinition* const node, Variable* lvalue = NULL);
void printVariableAssignment(VariableAssignment* const node, Variable* lvalue = NULL);
void printTypeDefinition(TypeDefinition* const node, Variable* lvalue = NULL);
void printTypeInitialization(TypeInitialization* const node, Variable* lvalue = NULL);
void printUnion(Union* const node, Variable* lvalue = NULL);
void printErrorSet(ErrorSet* const node, Variable* lvalue = NULL);
void printEnumerator(Enumerator* const node, Variable* lvalue = NULL);
void printVariable(Variable* const node, Variable* lvalue = NULL);
void printFunction(Function* const node, Variable* lvalue = NULL);
void printBranch(Branch* const node, Variable* lvalue = NULL);
void printSwitchCase(SwitchCase* const node, Variable* lvalue = NULL);
void printWhileLoop(WhileLoop* const node, Variable* lvalue = NULL);
void printForLoop(ForLoop* const node, Variable* lvalue = NULL);
void printLoop(Loop* const node, Variable* lvalue = NULL);
void printReturnStatement(ReturnStatement* const node, Variable* lvalue = NULL);
void printContinueStatement(ContinueStatement* const node, Variable* lvalue = NULL);
void printBreakStatement(BreakStatement* const node, Variable* lvalue = NULL);
void printGotoStatement(GotoStatement* const node, Variable* lvalue = NULL);
void printLabel(Label* const node, Variable* lvalue = NULL);
void printNamespace(Namespace* const node, Variable* lvalue = NULL);
void printStatement(Statement* const node, Variable* lvalue = NULL);
void printFunctionCall(FunctionCall* const node, Variable* lvalue = NULL, Variable* err = NULL);
void printOperand(Variable* const node, Variable* lvalue = NULL);
void printUnaryOperator(UnaryOperator* const node, Variable* lvalue = NULL);
void printBinaryOperator(BinaryOperator* const node, Variable* lvalue = NULL);
void printTernaryOperator(TernaryOperator* const node, Variable* lvalue = NULL);

void printWrapperExpression(WrapperExpression* const node, Variable* lvalue = NULL);
void printConstExpression(ConstExpression* const node, Variable* lvalue = NULL);
void printOperatorExpression(OperatorExpression* const node, Variable* lvalue = NULL);
void printCatchExpression(Catch* const node, Variable* lvalue = NULL, const int isGlobal = 0);

inline void printKeyValue(const char* key, INamed* value) {
    buffer += "\"" + std::string(key) + "\":\"" + std::string(value->name, value->nameLen) + "\"";
}

inline void printSymbolKind(Lsp::SymbolKind kind) {
    buffer += "\"kind\":\"" + std::to_string(kind) + "\"";
}

inline void printRange(const Span* const span) {
    
    Lsp::FileInfo* info = Lsp::getFileInfo(span->file->fpId);

    const int absLineStart = info->lineOffsets[span->start.ln - 1];
    const int absLineEnd = info->lineOffsets[span->end.ln - 1];

    const int startLine = span->start.ln - 1;
    const int startChar = span->start.idx - absLineStart;

    const int endLine = span->end.ln - 1;
    const int endChar = span->end.idx - absLineEnd;

    buffer += "\"range\":{";
    buffer += "\"start\":{\"line\":";
    buffer += std::to_string(startLine);
    buffer += ",\"character\":";
    buffer += std::to_string(startChar);
    buffer += "},\"end\":{\"line\":";
    buffer += std::to_string(endLine);
    buffer += ",\"character\":";
    buffer += std::to_string(endChar);

}



void clearBuffer() {
    buffer.clear();
};

void LspRenderer::render(SyntaxNode* const node, Variable* lvalue) {

    switch (node->type) {
        
        case NT_SCOPE :
            printScope((Scope*) node, lvalue);
            break;
        case NT_VARIABLE_DEFINITION :
            printVariableDefinition((VariableDefinition*) node, lvalue);
            break;
        case NT_VARIABLE_ASSIGNMENT :
            printVariableAssignment((VariableAssignment*) node, lvalue);
            break;
        case NT_TYPE_DEFINITION :
            printTypeDefinition((TypeDefinition*) node, lvalue);
            break;
        case NT_TYPE_INITIALIZATION :
            printTypeInitialization((TypeInitialization*) node, lvalue);
            break;
        case NT_UNION :
            printUnion((Union*) node, lvalue);
            break;
        case NT_ERROR :
            printErrorSet((ErrorSet*) node, lvalue);
            break;
        case NT_ENUMERATOR :
            printEnumerator((Enumerator*) node, lvalue);
            break;
        case NT_VARIABLE :
            printVariable((Variable*) node, lvalue);
            break;
        case NT_FUNCTION :
            printFunction((Function*) node, lvalue);
            break;
        case NT_BRANCH :
            printBranch((Branch*) node, lvalue);
            break;
        case NT_SWITCH_CASE :
            printSwitchCase((SwitchCase*) node, lvalue);
            break;
        case NT_WHILE_LOOP :
            printWhileLoop((WhileLoop*) node, lvalue);
            break;
        case NT_FOR_LOOP :
            printForLoop((ForLoop*) node, lvalue);
            break;
        case NT_LOOP :
            printLoop((Loop*) node, lvalue);
            break;
        case NT_RETURN_STATEMENT :
            printReturnStatement((ReturnStatement*) node, lvalue);
            break;
        case NT_CONTINUE_STATEMENT :
            printContinueStatement((ContinueStatement*) node, lvalue);
            break;
        case NT_BREAK_STATEMENT :
            printBreakStatement((BreakStatement*) node, lvalue);
            break;
        case NT_GOTO_STATEMENT :
            printGotoStatement((GotoStatement*) node, lvalue);
            break;
        case NT_LABEL :
            printLabel((Label*) node, lvalue);
            break;
        case NT_NAMESPACE :
            printNamespace((Namespace*) node, lvalue);
            break;
        case NT_STATEMENT :
            printStatement((Statement*) node, lvalue);
            break;
        case NT_FUNCTION_CALL :
            printFunctionCall((FunctionCall*) node, lvalue);
            break;
        case NT_OPERAND :
            printOperand((Variable*) node, lvalue);
            break;
        case NT_UNARY_OPERATOR :
            printUnaryOperator((UnaryOperator*) node, lvalue);
            break;
        case NT_BINARY_OPERATOR :
            printBinaryOperator((BinaryOperator*) node, lvalue);
            break;
        case NT_TERNARY_OPERATOR :
            printTernaryOperator((TernaryOperator*) node, lvalue);
            break;
    
        default:
            break;
    
    }
    
}

void printScope(Scope* const node, Variable* lvalue) {
    
    for (int i = 0; i < node->children.size(); i++) {
        LspRenderer::render(node->children[i], lvalue);
    }

}

void printVariableDefinition(VariableDefinition* const node, Variable* lvalue) {

    buffer += "{";

    printKeyValue("name", node->var);
    buffer += ",";

    printSymbolKind(node->var->cvalue.dtypeEnum == DT_CUSTOM ? Lsp::SK_STRUCT : Lsp::SK_VARIABLE);
    buffer += ",";

    printRange(node->span);

    buffer += "}";

}



void printVariableAssignment(VariableAssignment* const node, Variable* lvalue) {

}

void printTypeDefinition(TypeDefinition* const node, Variable* lvalue) {

}

void printTypeInitialization(TypeInitialization* const node, Variable* lvalue) {

}

void printUnion(Union* const node, Variable* lvalue) {

}

void printErrorSet(ErrorSet* const node, Variable* lvalue) {

}

void printEnumerator(Enumerator* const node, Variable* lvalue) {

}

void printVariable(Variable* const node, Variable* lvalue) {

}

void printFunction(Function* const node, Variable* lvalue) {

}

void printBranch(Branch* const node, Variable* lvalue) {

}

void printSwitchCase(SwitchCase* const node, Variable* lvalue) {

}

void printWhileLoop(WhileLoop* const node, Variable* lvalue) {

}

void printForLoop(ForLoop* const node, Variable* lvalue) {

}

void printLoop(Loop* const node, Variable* lvalue) {

}

void printReturnStatement(ReturnStatement* const node, Variable* lvalue) {

}

void printContinueStatement(ContinueStatement* const node, Variable* lvalue) {

}

void printBreakStatement(BreakStatement* const node, Variable* lvalue) {

}

void printGotoStatement(GotoStatement* const node, Variable* lvalue) {

}

void printLabel(Label* const node, Variable* lvalue) {

}

void printNamespace(Namespace* const node, Variable* lvalue) {

}

void printStatement(Statement* const node, Variable* lvalue) {

}

void printFunctionCall(FunctionCall* const node, Variable* lvalue, Variable* err) {

}

void printOperand(Variable* const node, Variable* lvalue) {

}

void printUnaryOperator(UnaryOperator* const node, Variable* lvalue) {

}

void printBinaryOperator(BinaryOperator* const node, Variable* lvalue) {

}

void printTernaryOperator(TernaryOperator* const node, Variable* lvalue) {

}

void printWrapperExpression(WrapperExpression* const node, Variable* lvalue) {

}

void printConstExpression(ConstExpression* const node, Variable* lvalue) {

}

void printOperatorExpression(OperatorExpression* const node, Variable* lvalue) {

}

void printCatchExpression(Catch* const node, Variable* lvalue, const int isGlobal) {

}