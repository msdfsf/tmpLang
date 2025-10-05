#pragma once

#include "globals.h"
#include <array>

namespace Lex {

    enum TokenKind : int32_t;
    enum TokenDetail : int32_t;
    
    union Token {
        struct {
            int32_t kind;
            int32_t detail;
        };
        int64_t encoded;
    };

    union TokenValue {
        uint64_t ival;
        double  fval;
        String* str;
        void* any;
    };

    static inline uint64_t packTokens(TokenKind a, TokenKind b) {
        return (b | (a << 8));
    }

    static inline uint16_t packChars(unsigned char a, unsigned char b) {
        return (b | (a << 8));
    }

    static const char EOS = '\0';
    static const char EOL = '\n';

    // As there are a lot of long names
    // groupless literals will be defined without
    // prefixes and other identification stuff...
    static const char POINTER = '^';
    static const char ADDRESS = '&';
    static const char STATEMENT_END = ';';
    static const char STRING_LITERAL = '"';
    static const char RAW_POSTFIX = 'b';
    static const char CHAR_LITERAL = '\'';
    static const char ESCAPE_CHAR = '\\';
    static const char ARRAY_BEGIN = '[';
    static const char ARRAY_END = ']';
    static const char SCOPE_BEGIN = '{';
    static const char SCOPE_END = '}';
    static const char LABEL_END = ':';
    static const char EQUAL = '=';
    static const char SKIP = '_';
    static const char LIST_SEPARATOR = ',';
    static const uint16_t SCOPE_RESOLUTION = packChars(':', ':');

    static const char* KWS_ARRAY_LENGTH = "length";
    static const char* KWS_ARRAY_SIZE = "size";

    static const char* KWS_VOID = "void";
    static const char* KWS_INT = "int";
    static const char* KWS_I8 = "i8";
    static const char* KWS_I16 = "i16";
    static const char* KWS_I32 = "i32";
    static const char* KWS_I64 = "i64";
    static const char* KWS_U8 = "u8";
    static const char* KWS_U16 = "u16";
    static const char* KWS_U32 = "u32";
    static const char* KWS_U64 = "u64";
    static const char* KWS_F32 = "f32";
    static const char* KWS_F64 = "f64";
    static const char* KWS_CONST = "const";
    static const char* KWS_EMBED = "embed";
    static const char* KWS_MUTON = "muton";
    static const char* KWS_AUTON = "auton";
    static const char* KWS_FCN = "fcn";
    static const char* KWS_DEF = "def";
    static const char* KWS_STRUCT = "struct";
    static const char* KWS_UNION = "union";
    static const char* KWS_IF = "if";
    static const char* KWS_ELSE = "else";
    static const char* KWS_FOR = "for";
    static const char* KWS_WHILE = "while";
    static const char* KWS_LOOP = "loop";
    static const char* KWS_WHEN = "when";
    static const char* KWS_CASE = "case";
    static const char* KWS_GOTO = "goto";
    static const char* KWS_ENUM = "enum";
    static const char* KWS_RETURN = "return";
    static const char* KWS_CONTINUE = "continue";
    static const char* KWS_BREAK = "break";
    static const char* KWS_USING = "using";
    static const char* KWS_SCOPE = "scope";
    static const char* KWS_NAMESPACE = "namespace";
    static const char* KWS_ALLOC = "alloc";
    static const char* KWS_FREE = "free";
    static const char* KWS_ERROR = "error";
    static const char* KWS_CATCH = "catch";
    static const char* KWS_IMPORT = "import";
    static const char* KWS_FROM = "from";
    static const char* KWS_TRUE = "true";
    static const char* KWS_FALSE = "false";
    static const char* KWS_AS = "as";
    static const char* KWS_NULL = "null";

    static const char* CDS_NONE = "none";
    static const char* CDS_TEST = "test";

    enum Keyword {
        KW_VOID,
        KW_INT,
        KW_I8,
        KW_I16,
        KW_I32,
        KW_I64,
        KW_U8,
        KW_U16,
        KW_U32,
        KW_U64,
        KW_F32,
        KW_F64,
        KW_CONST,
        KW_EMBED,
        KW_MUTON,
        KW_AUTON,
        KW_FCN,
        KW_DEF,
        KW_STRUCT,
        KW_UNION,
        KW_IF,
        KW_ELSE,
        KW_FOR,
        KW_WHILE,
        KW_LOOP,
        KW_WHEN,
        KW_CASE,
        KW_GOTO,
        KW_ENUM,
        KW_RETURN,
        KW_CONTINUE,
        KW_BREAK,
        KW_USING,
        KW_SCOPE,
        KW_NAMESPACE,
        KW_ALLOC,
        KW_FREE,
        KW_ERROR,
        KW_CATCH,
        KW_IMPORT,
        KW_FROM,
        KW_TRUE,
        KW_FALSE,
        KW_AS,
        KW_NULL,
        KW_COUNT
    };

    enum Directive {
        CD_NONE,
        CD_TEST,
        CD_COUNT
    };

    static const char* keywordStringTable[KW_COUNT] = {
        KWS_VOID,
        KWS_INT,
        KWS_I8,
        KWS_I16,
        KWS_I32,
        KWS_I64,
        KWS_U8,
        KWS_U16,
        KWS_U32,
        KWS_U64,
        KWS_F32,
        KWS_F64,
        KWS_CONST,
        KWS_EMBED,
        KWS_MUTON,
        KWS_AUTON,
        KWS_FCN,
        KWS_DEF,
        KWS_STRUCT,
        KWS_UNION,
        KWS_IF,
        KWS_ELSE,
        KWS_FOR,
        KWS_WHILE,
        KWS_LOOP,
        KWS_WHEN,
        KWS_CASE,
        KWS_GOTO,
        KWS_ENUM,
        KWS_RETURN,
        KWS_CONTINUE,
        KWS_BREAK,
        KWS_USING,
        KWS_SCOPE,
        KWS_NAMESPACE,
        KWS_ALLOC,
        KWS_FREE,
        KWS_ERROR,
        KWS_CATCH,
        KWS_IMPORT,
        KWS_FROM,
        KWS_TRUE,
        KWS_FALSE,
        KWS_AS,
        KWS_NULL,
    };

    static const char* directivesStringTable[CD_COUNT] = {
        CDS_NONE,
        CDS_TEST,
    };

    constexpr int KW_TABLE_SIZE = 158;
    constexpr std::array<int, KW_TABLE_SIZE> makeKeywordTable() {

        std::array<int, KW_TABLE_SIZE> table = {};

        table[6] = KW_I32;
        table[9] = KW_FOR;
        table[11] = KW_CATCH;
        table[13] = KW_STRUCT;
        table[14] = KW_FREE;
        table[15] = KW_NAMESPACE;
        table[19] = KW_CONTINUE;
        table[21] = KW_INT;
        table[23] = KW_IF;
        table[30] = KW_VOID;
        table[32] = KW_WHEN;
        table[36] = KW_LOOP;
        table[38] = KW_ERROR;
        table[40] = KW_F64;
        table[43] = KW_ELSE;
        table[49] = KW_IMPORT;
        table[52] = KW_RETURN;
        table[59] = KW_BREAK;
        table[61] = KW_FCN;
        table[67] = KW_U8;
        table[68] = KW_TRUE;
        table[72] = KW_I16;
        table[82] = KW_SCOPE;
        table[83] = KW_U64;
        table[85] = KW_MUTON;
        table[86] = KW_U32;
        table[99] = KW_ALLOC;
        table[101] = KW_I64;
        table[105] = KW_F32;
        table[108] = KW_USING;
        table[109] = KW_AUTON;
        table[113] = KW_GOTO;
        table[117] = KW_CONST;
        table[122] = KW_CASE;
        table[123] = KW_UNION;
        table[125] = KW_EMBED;
        table[127] = KW_FALSE;
        table[129] = KW_ENUM;
        table[133] = KW_NULL;
        table[136] = KW_FROM;
        table[139] = KW_I8;
        table[140] = KW_AS;
        table[144] = KW_U16;
        table[145] = KW_WHILE;
        table[149] = KW_DEF;
        
        return table;

    };
    constexpr auto keywordTable = makeKeywordTable();

    constexpr int CD_TABLE_SIZE = 1;
    static const int directivesTable[CD_TABLE_SIZE] = {
        CD_TEST,
    };

    enum TokenKind : int32_t {
        TK_NONE,
        TK_STATEMENT_BEGIN,
        TK_STATEMENT_END,
        TK_KEYWORD,
        TK_DIRECTIVE,
        TK_IDENTIFIER,
        TK_NUMBER,
        TK_STRING,
        TK_CHAR,
        TK_EQUAL,
        TK_SCOPE_RESOLUTION,
        TK_SCOPE_BEGIN,
        TK_SCOPE_END,
        TK_ARRAY_END,
        TK_LIST_SEPARATOR,
        TK_RAW,
        TK_PARENTHESIS_BEGIN,
        TK_PARENTHESIS_END,
        TK_ARROW,
        TK_SKIP,
        TK_FILE,
        
        // If used, detail should be valid OperatorEnum
        TK_BINARY_OPERATOR,
        // Not used as TokenDetail, since some operators may 
        // represent other tokens. Such tokens are more 
        // convenient to access via TokenKind, so storing 
        // operators in this field is preferred.
        // Colliding tokens will be represented by constants 
        // with the same value, defined outside of this enum.
        TK_OP_BEGIN,

        TK_OP_PLUS,
        TK_OP_MINUS,
        TK_OP_INCREMENT,
        TK_OP_DECREMENT,
        TK_OP_MULTIPLICATION,
        TK_OP_DIVISION,
        TK_OP_MODULO,
        TK_OP_AND,
        TK_OP_OR,
        TK_OP_XOR,
        TK_OP_NEGATION,
        TK_OP_SHIFT_RIGHT,
        TK_OP_SHIFT_LEFT,
        TK_OP_BOOL_AND,
        TK_OP_BOOL_OR,
        TK_OP_BOOL_NEGATION,
        TK_OP_BOOL_EQUAL,
        TK_OP_BOOL_NOT_EQUAL,
        TK_OP_LESS_THAN,
        TK_OP_LESS_THAN_OR_EQUAL,
        TK_OP_GREATER_THAN,
        TK_OP_GREATER_THAN_OR_EQUAL,
        TK_OP_CONCATENATION,
        TK_OP_MEMBER_SELECTION,
        TK_OP_SUBSCRIPT,

        TK_OP_END,

        TK_END,
    };

    constexpr TokenKind TK_LABEL_END = TK_STATEMENT_BEGIN;
    constexpr TokenKind TK_POINTER = TK_OP_XOR;
    constexpr TokenKind TK_ADDRESS = TK_OP_AND;
    constexpr TokenKind TK_ARRAY_BEGIN = TK_OP_SUBSCRIPT;
    constexpr TokenKind TK_THE_REST = TK_OP_CONCATENATION;
    constexpr TokenKind TK_SLICE = TK_STATEMENT_BEGIN;

    // CAUTION: Keyword enum must be aligned with 
    //          corresponding token representations
    enum TokenDetail : int32_t {
        TD_NONE = -1,

        TD_KW_VOID,
        TD_KW_INT,
        TD_KW_I8,
        TD_KW_I16,
        TD_KW_I32,
        TD_KW_I64,
        TD_KW_U8,
        TD_KW_U16,
        TD_KW_U32,
        TD_KW_U64,
        TD_KW_F32,
        TD_KW_F64, // TODO : add last data type identifier
        TD_KW_CONST,
        TD_KW_EMBED,
        TD_KW_MUTON,
        TD_KW_AUTON,
        TD_KW_FCN,
        TD_KW_DEF,
        TD_KW_STRUCT,
        TD_KW_UNION,
        TD_KW_IF,
        TD_KW_ELSE,
        TD_KW_FOR,
        TD_KW_WHILE,
        TD_KW_LOOP,
        TD_KW_WHEN,
        TD_KW_CASE,
        TD_KW_GOTO,
        TD_KW_ENUM,
        TD_KW_RETURN,
        TD_KW_CONTINUE,
        TD_KW_BREAK,
        TD_KW_USING,
        TD_KW_SCOPE,
        TD_KW_NAMESPACE,
        TD_KW_ALLOC,
        TD_KW_FREE,
        TD_KW_ERROR,
        TD_KW_CATCH,
        TD_KW_IMPORT,
        TD_KW_FROM,
        TD_KW_TRUE,
        TD_KW_FALSE,
        TD_KW_AS,
        TD_KW_NULL,

        TD_CD_BEGIN,
        TD_CD_TEST,
        TD_CD_END,
        
        TD_DT_VOID,
        TD_DT_I64,
        TD_DT_U64,
        TD_DT_F32,
        TD_DT_F64,

    };



    const char* toStr(TokenKind token);



    // 'next' functions will commit changes to span
    Token nextToken(Span* const span, TokenValue* val = NULL);
    Token nextFileName(Span* const span, TokenValue* val = NULL);

    // 'try' functions will not commit changes to span if token is not found
    Token tryToken(Span* const span, Token token, TokenValue* val = NULL);
    Token tryKeyword(Span* const span, Keyword keyword);

    // 'peek' functions will not commit changes to span
    Token peekToken(Span* const span, TokenValue* val = NULL);
    Token peekNthToken(Span* const span, TokenValue* val, unsigned int n);
    Token peekTokenSkipDecorators(Span* const span, TokenValue* val = NULL);

    unsigned int hash(const char* str);

    int findBlockEnd(Span* const span, const char bCh, const char eCh);



    // Keep them in the header to prevent compiler yelling
    static inline Token toToken(int64_t val) {
        return Token { .encoded = val };
    }

    static inline Token toToken(TokenKind val) {
        return Token { .kind = val };
    }

    static inline Token toToken(TokenDetail val) {
        return Token { .detail = val };
    }

    static inline Token toTokenAsBinaryOperator(OperatorEnum val) {
        return Token{ .kind = TK_BINARY_OPERATOR,.detail = val };
    }

    static inline DataTypeEnum toDtype(Token val) {

        switch (val.detail) {
            case TD_DT_F32: return DT_F32;
            case TD_DT_F64: return DT_F64;
            case TD_DT_I64: return DT_I64;
            case TD_DT_U64: return DT_U64;
            case TD_KW_FCN: return DT_FUNCTION;
            default: return DT_VOID;
        }
    
    }

    static inline DataTypeEnum toDtype(Keyword val) {
        return (DataTypeEnum) (val == KW_FCN ? DT_FUNCTION : (val - KW_VOID));
    }

    static inline OperatorEnum toOperator(TokenValue val) {
        return (OperatorEnum) val.ival;
    }

    static inline OperatorEnum toOperator(Token val) {
        return (OperatorEnum) (val.kind - TK_OP_BEGIN + 1);
    }

    // Converts a Token to its corresponding unary OperatorEnum.
    // Returns OP_NONE if the token cannot represent a unary operator.
    static inline OperatorEnum toUnaryOperator(Token val) {

        switch (val.kind) {

            case TK_OP_PLUS             : return OP_UNARY_PLUS;
            case TK_OP_MINUS            : return OP_UNARY_MINUS;
            case TK_POINTER             : return OP_GET_VALUE;
            case TK_ADDRESS             : return OP_GET_ADDRESS;
            case TK_OP_INCREMENT        : return OP_INCREMENT;
            case TK_OP_DECREMENT        : return OP_DECREMENT;
            case TK_OP_NEGATION         : return OP_BITWISE_NEGATION;
            case TK_OP_BOOL_NEGATION    : return OP_NEGATION;
            default                     : return OP_NONE;
        
        }

    }

    static inline OperatorEnum toPostfixOperator(Token val) {

        switch (val.kind) {

            case TK_OP_INCREMENT        : return OP_INCREMENT;
            case TK_OP_DECREMENT        : return OP_DECREMENT;
            default                     : return OP_NONE;
        
        }

    }

    static inline OperatorEnum toBinaryOperator(Token val) {

        if (val.kind == TK_BINARY_OPERATOR) return (OperatorEnum) val.detail;

        switch (val.kind) {

            case TK_OP_PLUS                     : return OP_ADDITION;
            case TK_OP_MINUS                    : return OP_SUBTRACTION;
            case TK_OP_MULTIPLICATION           : return OP_MULTIPLICATION;
            case TK_OP_DIVISION                 : return OP_DIVISION;
            case TK_OP_MODULO                   : return OP_MODULO;
            case TK_OP_LESS_THAN                : return OP_LESS_THAN;
            case TK_OP_GREATER_THAN             : return OP_GREATER_THAN;
            case TK_OP_LESS_THAN_OR_EQUAL       : return OP_LESS_THAN_OR_EQUAL;
            case TK_OP_GREATER_THAN_OR_EQUAL    : return OP_GREATER_THAN_OR_EQUAL;
            case TK_OP_BOOL_EQUAL               : return OP_EQUAL;
            case TK_OP_BOOL_NOT_EQUAL           : return OP_NOT_EQUAL;
            case TK_OP_BOOL_AND                 : return OP_BOOL_AND;
            case TK_OP_BOOL_OR                  : return OP_BOOL_OR;
            case TK_OP_CONCATENATION            : return OP_CONCATENATION;
            case TK_OP_MEMBER_SELECTION         : return OP_MEMBER_SELECTION;
            case TK_OP_AND                      : return OP_BITWISE_AND;
            case TK_OP_OR                       : return OP_BITWISE_OR;
            case TK_OP_XOR                      : return OP_BITWISE_XOR;
            case TK_OP_NEGATION                 : return OP_BITWISE_NEGATION;
            case TK_OP_SHIFT_RIGHT              : return OP_SHIFT_RIGHT;
            case TK_OP_SHIFT_LEFT               : return OP_SHIFT_LEFT;
            case TK_ARRAY_BEGIN                 : return OP_SUBSCRIPT;
            default                             : return OP_NONE;
        
        }

    }

    static inline int isKeyword(Token token, Keyword keyword) {
        return (token.kind == TK_KEYWORD && token.detail == keyword);
    }

    static inline int isDtype(TokenDetail val) {
        return ((val >= TD_KW_VOID && val <= TD_KW_F64) || val == Lex::TD_KW_FCN);
    }

    static inline int isDtype(Token val) {
        return ((val.kind == Lex::TK_KEYWORD) && isDtype((TokenDetail) val.detail));
    }

    static inline int isInt(Keyword val) {
        return (val >= KW_INT && val <= KW_U64);
    }

    static inline int isOperator(Token val) {
        return (val.kind > TK_OP_BEGIN && val.kind < TK_OP_END);
    }

    static inline int isPostfixOperator(Token val) {
        return val.kind == TK_OP_INCREMENT || val.kind == TK_OP_DECREMENT;    
    }
    
    static inline int compareChars(const char* str, uint16_t ch) {
        return (str[0] == (char)(ch & 0xFF)) && (str[1] == (char)(ch >> 8));
    }

    template <typename T>
    static inline uint64_t toIntStr(T ch) {
        uint64_t tmp = 0;
        memcpy(&tmp, &ch, sizeof(T));
        return tmp;
    }

}

