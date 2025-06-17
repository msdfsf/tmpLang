#pragma once

#include "globals.h"

namespace Lex {

    const char EOS = '\0';
    const char EOL = '\n';

    // As there are a lot of long names
    // groupless literals will be defined without
    // prefixes and other identification stuff...
    const char POINTER = '^';
    const char ADDRESS = '&';
    const char STRING_LITERAL = '"';
    const char RAW_POSTFIX = 'b';
    const char CHAR_LITERAL = '\'';
    const char ESCAPE_CHAR = '\\';
    const char ARRAY_BEGIN = '[';
    const char ARRAY_END = ']';
    const char SCOPE_BEGIN = '{';
    const char SCOPE_END = '}';

    const char* KWS_VOID = "void";
    const char* KWS_INT = "int";
    const char* KWS_I8 = "i8";
    const char* KWS_I16 = "i16";
    const char* KWS_I32 = "i32";
    const char* KWS_I64 = "i64";
    const char* KWS_U8 = "u8";
    const char* KWS_U16 = "u16";
    const char* KWS_U32 = "u32";
    const char* KWS_U64 = "u64";
    const char* KWS_F32 = "f32";
    const char* KWS_F64 = "f64";
    const char* KWS_CONST = "const";
    const char* KWS_EMBED = "embed";
    const char* KWS_MUTON = "muton";
    const char* KWS_AUTON = "auton";
    const char* KWS_FCN = "fcn";
    const char* KWS_DEF = "def";
    const char* KWS_STRUCT = "struct";
    const char* KWS_UNION = "union";
    const char* KWS_IF = "if";
    const char* KWS_ELSE = "else";
    const char* KWS_FOR = "for";
    const char* KWS_WHILE = "while";
    const char* KWS_LOOP = "loop";
    const char* KWS_WHEN = "when";
    const char* KWS_CASE = "case";
    const char* KWS_GOTO = "goto";
    const char* KWS_ENUM = "enum";
    const char* KWS_RETURN = "return";
    const char* KWS_CONTINUE = "continue";
    const char* KWS_BREAK = "break";
    const char* KWS_USING = "using";
    const char* KWS_SCOPE = "scope";
    const char* KWS_NAMESPACE = "namespace";
    const char* KWS_ALLOC = "alloc";
    const char* KWS_FREE = "free";
    const char* KWS_ERROR = "error";
    const char* KWS_CATCH = "catch";
    const char* KWS_IMPORT = "import";
    const char* KWS_FROM = "from";
    const char* KWS_TRUE = "true";
    const char* KWS_FALSE = "false";
    const char* KWS_NULL = "null";

    const char* CDS_NONE = "none";
    const char* CDS_TEST = "test";

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
        KW_NULL,
        KW_COUNT
    };

    enum Directive {
        CD_NONE,
        CD_TEST,
        CD_COUNT
    };

    static const const char* keywordStringTable[KW_COUNT] = {
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
        KWS_NULL,
    };

    static const char* directivesStringTable[CD_COUNT] = {
        CDS_NONE,
        CDS_TEST,
    };

    constexpr int KW_TABLE_SIZE = 203;
    constexpr std::array<int, KW_TABLE_SIZE> makeKeywordTable() {
        std::array<int, KW_TABLE_SIZE> table = { 0 };

        table[6] = KW_ELSE;
        table[7] = KW_U32;
        table[10] = KW_FROM;
        table[12] = KW_INT;
        table[17] = KW_I8;
        table[19] = KW_FREE;
        table[22] = KW_FALSE;
        table[25] = KW_MUTON;
        table[33] = KW_AUTON;
        table[40] = KW_U64;
        table[46] = KW_EMBED;
        table[57] = KW_LOOP;
        table[60] = KW_I16;
        table[61] = KW_UNION;
        table[70] = KW_ENUM;
        table[83] = KW_BREAK;
        table[84] = KW_IMPORT;
        table[87] = KW_ALLOC;
        table[90] = KW_WHEN;
        table[91] = KW_F64;
        table[99] = KW_CASE;
        table[106] = KW_I64;
        table[117] = KW_FOR;
        table[121] = KW_DEF;
        table[123] = KW_FCN;
        table[124] = KW_TRUE;
        table[126] = KW_I32;
        table[130] = KW_USING;
        table[131] = KW_NAMESPACE;
        table[133] = KW_U8;
        table[139] = KW_NULL;
        table[146] = KW_IF;
        table[150] = KW_ERROR;
        table[152] = KW_U16;
        table[156] = KW_CONST;
        table[159] = KW_WHILE;
        table[175] = KW_CONTINUE;
        table[176] = KW_CATCH;
        table[180] = KW_VOID;
        table[184] = KW_RETURN;
        table[190] = KW_SCOPE;
        table[198] = KW_GOTO;
        table[199] = KW_F32;
        table[200] = KW_STRUCT;

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
        TK_POINTER,
        TK_EQUAL,
        TK_ADDRESS,
        TK_THE_REST,
        TK_MEMBER_SELECTION,
        TK_SCOPE_RESOLUTION,
        TK_SCOPE_BEGIN,
        TK_SCOPE_END,
        TK_ARRAY_BEGIN,
        TK_ARRAY_END,
        TK_SKIP_VARIABLE,
        TK_LIST_SEPARATOR,
        TK_RAW,
        TK_PARENTHESIS_BEGIN,
        TK_PARENTHESIS_END,
        TK_ARROW,
        TK_END,
    };

    // CAUTION: Keyword enum must be aligned with 
    //          corresponding token representations
    enum TokenDetail : int32_t {
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
        TD_KW_F64,
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
        TD_KW_NULL,
        
        TD_DT_I64,
        TD_DT_U64,
        TD_DT_F32,
        TD_DT_F64,
        TD_DT_VOID,
    };

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

    Token nextToken(Location* const loc, TokenValue* val = NULL);
    Token nextTokenSkipDecorators(Location* const loc, TokenValue* val = NULL);

    Token tryToken(Location* const loc, TokenKind token);
    Token tryKeyword(Location* const loc, Keyword keyword);

    unsigned int hash(const char* str);

    int findBlockEnd(Location* const loc, const char bCh, const char eCh);



    // Keep them in the header to prevent compiler yelling
    inline Token toToken(int64_t val) {
        return (Token) { .encoded = val };
    }

    inline Token toToken(TokenKind val) {
        return (Token) { .kind = val };
    }

    inline Token toToken(TokenDetail val) {
        return (Token) { .detail = val };
    }

    inline DataTypeEnum toDtype(Token val) {
        return (DataTypeEnum) (val.detail - TD_DT_VOID);
    }

    inline DataTypeEnum toDtype(Keyword val) {
        return (DataTypeEnum) (val - KW_VOID);
    }

    inline int isDtype(Token val) {
        return (val.detail >= TD_DT_VOID && val.detail <= TD_DT_F64);
    }

    inline int isInt(Keyword val) {
        return (val >= KW_INT && val <= KW_U64);
    }

    inline uint64_t packTokens(TokenKind a, TokenKind b) {
        return (b | (a << 8));
    }

    inline uint16_t packChars(unsigned char a, unsigned char b) {
        return (b | (a << 8));
    }
    
    inline int compareChars(const char* str, uint16_t ch) {
        return (str[0] == (char)(ch & 0xFF)) && (str[1] == (char)(ch >> 8));
    }

}

