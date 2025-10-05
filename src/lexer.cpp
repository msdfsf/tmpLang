#include "lexer.h"
#include "error.h"
#include "syntax.h"
#include "logger.h"
#include "utils.h"

// Expects null terminated buffer
namespace Lex {

    const char* toStr(TokenKind token) {

        switch (token) {
            case TK_NONE: return "none";
            case TK_IDENTIFIER: return "identifier";
            case TK_KEYWORD: return "keyword";
            case TK_CHAR: return "char";
            case TK_STRING: return "string";
            case TK_NUMBER: return "number";
            case TK_FILE: return "file";

            case TK_OP_INCREMENT: return "++";
            case TK_OP_PLUS: return "+";
            case TK_OP_DECREMENT: return "--";
            case TK_ARROW: return "->";
            case TK_OP_MINUS: return "-";
            case TK_OP_MULTIPLICATION: return "*";
            case TK_OP_DIVISION: return "/";
            case TK_OP_MODULO: return "%";

            case TK_OP_BOOL_NOT_EQUAL: return "!=";
            case TK_OP_BOOL_NEGATION: return "!";
            case TK_OP_BOOL_EQUAL: return "==";
            case TK_EQUAL: return "=";

            case TK_OP_LESS_THAN: return "<";
            case TK_OP_LESS_THAN_OR_EQUAL: return "<=";
            case TK_OP_GREATER_THAN: return ">";
            case TK_OP_GREATER_THAN_OR_EQUAL: return ">=";

            case TK_OP_SHIFT_LEFT: return "<<";
            case TK_OP_SHIFT_RIGHT: return ">>";

            case TK_OP_AND: return "&";
            case TK_OP_BOOL_AND: return "&&";
            case TK_OP_OR: return "|";
            case TK_OP_BOOL_OR: return "||";
            case TK_OP_XOR: return "^";
            case TK_OP_NEGATION: return "~";

            case TK_DIRECTIVE: return "#";

            case TK_OP_CONCATENATION: return "..";
            case TK_OP_MEMBER_SELECTION: return ".";

            case TK_SCOPE_RESOLUTION: return "::";
            case TK_STATEMENT_BEGIN: return ":";
            case TK_STATEMENT_END: return ";";

            case TK_SCOPE_BEGIN: return "{";
            case TK_SCOPE_END: return "}";

            case TK_ARRAY_BEGIN: return "[";
            case TK_ARRAY_END: return "]";

            case TK_SKIP: return "_";
            case TK_LIST_SEPARATOR: return ",";

            case TK_PARENTHESIS_BEGIN: return "(";
            case TK_PARENTHESIS_END: return ")";

            case TK_END: return "EOF";

            default: return "unknown";
        }
    
    }

    unsigned int hash(const char* str, int len) {
        unsigned int hash = 5381 ^ 0x9c8d7e6f;
        int idx = 0;
        while (idx < len) {
            hash = ((hash << 5) + hash) ^ (uint8_t) str[idx++];
        }
        return hash % KW_TABLE_SIZE;
    }

    inline Keyword keywordLookup(const char* str, const int len) {

        const unsigned int h = hash(str, len);
        if (h >= KW_TABLE_SIZE) return KW_VOID;
        
        Keyword keyword = (Keyword) keywordTable[h];
        
        const int ans = cstrcmp(keywordStringTable[keyword], String { (char*) str, (uint64_t) len });
        return ans ? keyword : KW_VOID;

    }
    
    inline Directive directiveLookup(const char* str, const int len) {

        const unsigned int h = hash(str, len) + 1;
        if (h >= KW_TABLE_SIZE) return CD_NONE;
        
        Directive directive = (Directive) directivesTable[h];

        const int ans = cstrcmp(directivesStringTable[directive], String { (char*) str, (uint64_t) len });
        return ans ? directive : CD_NONE;

    }

    static inline bool isWhitespace(char ch) {
        return (
            ch == ' '  || ch == '\t' ||
            ch == '\v' || ch == '\f' ||
            ch == '\r' || ch == '\n'
        );
    }

    inline int isWhitespaceButNewLine(char ch) {
        return (
            ch == ' '  || ch == '\t' ||
            ch == '\v' || ch == '\f' ||
            ch == '\r'
        );
    }

    inline Pos skipWhitespaces(const char* str, Pos pos) {
        while (str[pos.idx] != '\0' && isWhitespace(str[pos.idx])) {
            if (str[pos.idx] == '\n') {
                pos.ln++;
            }
            pos.idx++;
        }
        return pos;
    }

    // if unterminated comment found returns its start position and marks *err as 1 
    // (should be premarked to other value), NULL err ignores and just skips to EOF
    static inline Pos skipWhitespacesAndComments(const char* str, Pos pos, int* err) {

        while (1) {
        
            const char ch = str[pos.idx];

            if (ch == '\n') {
                pos.ln++;
                pos.idx++;
                continue;
            }
            
            if (isWhitespaceButNewLine(ch)) {
                pos.idx++;
                continue;
            }
            
            if (ch == '/' && str[pos.idx + 1] == '/') {

                pos.idx += 2;

                while (1) {

                    if (str[pos.idx] == EOL) {
                        pos.ln++;
                        pos.idx++;
                        break;
                    } else if (str[pos.idx] == EOS) {
                        break;
                    }

                    pos.idx++;

                }

                continue;
                
            }
            
            if (ch == '/' && str[pos.idx + 1] == '{') {

                pos.idx += 2;

                Pos startPos = pos;
                int nestedComments = 1;
                while (nestedComments > 0) {

                    if (str[pos.idx] == EOS) {
                        if (err) *err = 1;
                        return startPos;
                    } else if (str[pos.idx] == EOL) {
                        pos.ln++;
                        pos.idx++;
                    } else if (str[pos.idx] == '/' && str[pos.idx + 1] == '{') {
                        nestedComments++;
                        pos.idx += 2;
                    } else if (str[pos.idx] == '/' && str[pos.idx + 1] == '}') {
                        nestedComments--;
                        pos.idx += 2;
                    } else {
                        pos.idx++;
                    }
                    
                }

                continue;

            }

            return pos;
        
        }

    }

    inline int cmpTwoChars(uint16_t packed, const char* str) {
        return (str[0] == (packed & 0xFF)) && (str[1] == ((packed >> 8) & 0xFF));
    }

    inline int isIdentifierStart(const char ch) {
        return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_';
    }

    inline int isIdentifierChar(const char ch) {
        return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '_';
    }

    inline int isNumberStart(const char ch) {
        return (ch >= '0' && ch <= '9') || ch == '.' || ch == '-';
    }

    inline int parseIdentifier(char* const str) {
        

        if (!isIdentifierStart(str[0])) return 0;

        int idx = 1;
        while (isIdentifierChar(str[idx])) {
            idx++;
        }

        return idx;

    }

    inline TokenDetail parseNumber(char* const str, int* len, uint64_t* out) {

        int idx = 0;

        const int16_t tmp = ((uint8_t)str[0]) | ((uint8_t)str[1] << 8); //((int16_t *) str)[0];
        if (tmp == (('x' << 8) + '0')) {
            // int-hex

            uint64_t num = 0;
            for (int i = 2;; i++) {

                const char ch = str[i];

                if (ch >= '0' && ch <= '9') num = num * 16 + (ch - '0');
                else if (ch >= 'A' && ch <= 'F') num = num * 16 + (10 + ch - 'A');
                else if (ch >= 'a' && ch <= 'f') num = num * 16 + (10 + ch - 'a');
                else if (ch == '_') {
                    if (str[i + 1] == '_') return TD_DT_VOID;
                } else {
                    *len = i;
                    *out = num;
                    return TD_DT_I64;
                }
            
            }
        
        } else if (tmp == (('b' << 8) + '0')) {
            // int-bin

            uint64_t num = 0;
            for (int i = 2;; i++) {

                const char ch = str[i];
                if (ch == '0' || ch == '1') num = num * 2 + (ch - '0');
                else if (ch == '_') {
                    if (str[i + 1] == '_') return TD_DT_VOID;
                } else {
                    *len += i;
                    *out = num;
                    return TD_DT_I64;
                }
            
            }
        
        } else {

            uint64_t integer = 0;
            for (int i = 0;; i++) {

                const char ch = str[i];

                if (ch >= '0' && ch <= '9') integer = integer * 10 + (ch - '0');
                else if (ch == '.') {
                    // float
                    // TODO : precision for 'small' floats

                    double value = (double) integer;
                    double frac = 0.0;
                    double scale = 0.1;

                    while (1) {
                        
                        i++;

                        const char ch = str[i];
                        if (ch >= '0' && ch <= '9') {
                            frac += (ch - '0') * scale;
                            scale *= 0.1;
                        } else if (ch == '_') {
                            if (str[i + 1] == '_') return TD_DT_VOID;
                        } else {
                            break;
                        }

                    }

                    if (ch == 'f') {
                        *len = i + 1;
                        *((float_t *)out) = (float_t)(value + frac);
                        return TD_DT_F32;
                    }

                    *len = i;
                    *((double_t *)out) = value + frac;
                    return TD_DT_F64;

                } else if (ch == 'f') {

                    *len = i + 1;
                    *((float_t *) out) = (float_t) integer;
                    return TD_DT_F32;
                    
                } else if (ch == '_') {
                    
                    if (str[i + 1] == '_') return TD_DT_VOID;
                    
                } else {

                    *len = i;
                    *out = integer;
                    return TD_DT_I64;
                
                }
            
            }
        
        }

        return TD_DT_VOID;

    }

    int parseEscapeChar(char* str, int* idx) {

        const char ch = str[*idx];
        switch(ch) {

            case 'a':
                return 0x07;
            case 'b':
                return 0x08;
            case 'f':
                return 0x0C;
            case 'n':
                return 0x0A;
            case 'r':
                return 0x0D;
            case 't':
                return 0x09;
            case 'v':
                return 0x0B;
            case '\\':
                return 0x5C;
            case '\'':
                return 0x27;
            case '\"':
                return 0x22;
            case '\?':
                return 0x3F;
            case '\0':
                return 0;
            default:
                return -1;

        }

    };

    uint64_t parseHexInt(char* const str, int* idx) {

        uint64_t num = 0;
        for (int i = 0;; i++) {

            const char ch = str[i];

            if (ch >= '0' && ch <= '9') num = num * 16 + (ch - '0');
            else if (ch >= 'A' && ch <= 'F') num = num * 16 + (10 + ch - 'A');
            else if (ch >= 'a' && ch <= 'f') num = num * 16 + (10 + ch - 'a');
            else {
                *idx = i;
                return num;
            }

        }

    }

    inline int parseChar(char* str, int* idx) {

        char ch = str[*idx];
            
        if (ch == ESCAPE_CHAR) {
            
            if (str[*idx + 1] == 'x') {

                int hexLen;
                ch = parseHexInt(str + *idx + 2, &hexLen);
                if (hexLen == 0) {
                    // Logger::log(Logger::ERROR, "At least one hex digit required!", span, 1);
                    return Err::UNEXPECTED_SYMBOL;
                }
                idx += hexLen;
            
            } else {
                
                ch = parseEscapeChar(str, idx);
                if (ch == -1) {
                    // parseHexInt(str, &idx);
                    // Logger::log(Logger::ERROR, ERR_STR(Err::UNSUPPORTED_ESCAPE_SEQUENCE), span, 1);
                    return Err::UNSUPPORTED_ESCAPE_SEQUENCE;
                }
            
            }
        
        }

        return ch;

    }

    int parseCharLiteral(char* const str, int* len, uint64_t* out) {
        
        int idx = 0;
        uint64_t tmpOut = 0;

        int size = 0;
        while (1) {

            char ch = parseChar(str, &idx);
            
            if (ch == CHAR_LITERAL) break;
            if (ch == EOS) {
                // Logger::log(Logger::ERROR, ERR_STR(Err::UNEXPECTED_END_OF_FILE));
                return Err::UNEXPECTED_END_OF_FILE;
            }

            uint64_t tmp = ch;
            tmp <<= 56;

            tmpOut >>= 8;
            tmpOut |= tmp;

            size++;

            idx++;

        }

        if (size > 8) {
            //Logger::log(Logger::ERROR, ERR_STR(Err::DATA_TYPE_SIZE_EXCEEDED), span, len);
            return Err::DATA_TYPE_SIZE_EXCEEDED;
        }

        tmpOut >>= (8 - size) * 8;

        *len = idx;
        *out = tmpOut;

        return size;

    }

    int findStringEnd(char* const str) {

        int idx = 0;

        while (1) {

            const char ch = parseChar(str, &idx);
            if (ch == STRING_LITERAL) break;
            if (ch == EOS) return Err::UNEXPECTED_END_OF_FILE;

            idx++;

        }

        idx += (str[idx + 1] == RAW_POSTFIX);

    }

    int parseStringLiteral(char* const str, int* len, StringInitialization** initOut) {

        int idx = 0;

        while (1) {

            const char ch = parseChar(str, &idx);
            if (ch == STRING_LITERAL) break;
            if (ch == EOS) return Err::UNEXPECTED_END_OF_FILE;

            idx++;
        
        }

        const int strLen = idx;
        const int rawStringRequired = (str[idx + 1] == RAW_POSTFIX) ? 1 : 0;

        StringInitialization* init = new StringInitialization;
        init->rawStr = std::string(str, strLen);
        init->rawPtr = str;
        init->rawPtrLen = strLen;

        // meh but whatever
        if (rawStringRequired) {
            init->wideStr = NULL;
            init->wideDtype = DT_U8;
            idx++;
        } else {

            int utf8Len;
            int utf8BytesPerChar;
            char* utf8Str = Utils::encodeUtf8(init->rawStr.c_str(), strLen, &utf8Len, &utf8BytesPerChar);
            
            if (utf8BytesPerChar != 1) {
                init->wideStr = utf8Str;
                init->wideDtype = (DataTypeEnum) (DT_U8 + utf8BytesPerChar - 1);
                init->wideLen = utf8Len;
            } else {
                init->wideStr = NULL;
                init->wideDtype = DT_U8;
            }
        
        }

        *len = idx;
        *initOut = init;

        return Err::OK;

    }

    // first char already consumed
    Token parseQualifiedName(Span* const span, char* const str, QualifiedName* const qname, int* outLen) {

        int idx = 0;
        int len = 1;
        int accLen = 0;
        Token token = { .kind = TK_IDENTIFIER, .detail = 0 };

        qname->name = NULL;

        while (1) {
            
            while (isIdentifierChar(str[idx + 1])) {
                idx++;
                len++;
            }

            Keyword keyword = keywordLookup(str, len);

            if (keyword > 0) {
                token.kind = TK_KEYWORD;
                token.detail = keyword;
            }

            if (qname->name) {
                INamedLoc* name = new INamedLoc;
                name->span = NULL;
                name->name = qname->name;
                name->nameLen = qname->nameLen;
                qname->path.push_back(name);
            }

            qname->name = str + idx - len + 1;
            qname->nameLen = len;

            if (!cmpTwoChars(SCOPE_RESOLUTION, str + idx + 1)) {
                break;
            }

            if (token.kind == TK_KEYWORD) {
                // ERROR
                *outLen = accLen + len;
                return toToken(Err::UNEXPECTED_SYMBOL);
            }

            accLen += len + 2;
            len = 0;
            idx +=2;

        }

        *outLen = accLen + len;
        return token;

    }



    Token tryToken(Span* const span, Token inToken, TokenValue* outVal) {
        
        Pos posStart = span->start;
        Pos posEnd = span->end;

        Token token = nextToken(span, outVal);
        if (token.encoded == inToken.encoded) return token;

        span->start = posStart;
        span->end = posEnd;

        return { .kind = TK_NONE };
    
    }

    Token tryKeyword(Span* const span, Keyword keyword) {

        Pos posStart = span->start;
        Pos posEnd = span->end;

        Token token = nextToken(span, NULL);
        if (isKeyword(token, keyword)) return token;

        span->start = posStart;
        span->end = posEnd;

        return { .kind = TK_NONE };

    }



    Token peekToken(Span* const span, TokenValue* val) {

        Pos posStart = span->start;
        Pos posEnd = span->end;

        Token token = nextToken(span, val);

        span->start = posStart;
        span->end = posEnd;

        return token;

    }

    // n should be >0, calling with 1 is equivalent of calling peekToken
    Token peekNthToken(Span* const span, TokenValue* val, unsigned int n) {
    
        Token token;

        Pos posStart = span->start;
        Pos posEnd = span->end;

        int i = 0;
        while (i < n) {
            token = nextToken(span, val);
            i++;
        }
    
        span->start = posStart;
        span->end = posEnd;

        return token;

    }



    // span start and end will be set to token boundaries
    // uses span->end as current position, so span->end.idx + 1 is the 
    // functions first char to parse
    Token nextToken(Span* const span, TokenValue* val) {

        Token token;
        
        char* const str = span->str;

        int err = 0;
        span->end.idx++;
        Pos startPos = skipWhitespacesAndComments(str, span->end, &err);
        if (err) {
            span->start = startPos;
            span->end = startPos;
            Logger::log(Logger::ERROR, "Unterminated comment!", span, 1);
            return toToken(Err::UNTERMINATED_COMMENT);
        }

        int ln = 0;
        int len = 1;

        const char ch = str[startPos.idx];
        switch (ch) {

            case CHAR_LITERAL: {

                uint64_t ch = 0;
                const int size = parseCharLiteral(str + startPos.idx + 1, &len, &ch);
                if (size < 0) {
                    Logger::log(Logger::ERROR, "Wrong char literal format!", span, 1);
                    return toToken(size);
                }

                if (val) val->ival = ch;
                token.kind = TK_CHAR;
                token.detail = size;

                len += 2;
                break;
                
            }
            
            case STRING_LITERAL: {

                if (val) {
                    StringInitialization* init;
                    const int err = parseStringLiteral(str + startPos.idx + 1, &len, &init);
                    val->any = init;
                } else {
                    len = findStringEnd(str + startPos.idx + 1);
                }

                if (err < 0) {
                    Logger::log(Logger::ERROR, "Wrong string literal format!", span, 1);
                    return toToken(err);
                }

                token.kind = TK_STRING;
                
                len += 2;
                break;
                
            }

            case '+': {
                    
                if (str[startPos.idx + 1] == '+') {
                    len = 2;
                    token = { .kind = TK_OP_INCREMENT };
                } else {
                    token = { .kind = TK_OP_PLUS };
                }

                break;
                
            }

            case '-': {
                    
                if (str[startPos.idx + 1] == '-') {
                    len = 2;
                    token = { .kind = TK_OP_DECREMENT };
                } else if (str[startPos.idx + 1] == '>') {
                    len = 2;
                    token = { .kind = TK_ARROW };
                } else {
                    token = { .kind = TK_OP_MINUS };
                }

                break;
                
            }

            case '*': {
                    
                token = { .kind = TK_OP_MULTIPLICATION };
                break;
                
            }
                
            case '!': {
                    
                if (str[startPos.idx + 1] == '=') {
                    len = 2;
                    token = { .kind = TK_OP_BOOL_NOT_EQUAL };
                } else {
                    token = { .kind = TK_OP_BOOL_NEGATION };
                }

                break;
                
            }

            case '/': {

                token = { .kind = TK_OP_DIVISION };
                break;
                
            }

            case '%': {
                    
                token = { .kind = TK_OP_MODULO };
                break;
                
            }
                
            case '<': {
                    
                if (str[startPos.idx + 1] == '=') {
                    len = 2;
                    token = { .kind = TK_OP_LESS_THAN_OR_EQUAL };
                } else if (str[startPos.idx + 1] == '<') {
                    len = 2;
                    token = { .kind = TK_OP_SHIFT_LEFT };
                } else {
                    token = { .kind = TK_OP_LESS_THAN };
                }

                break;
                
            }
                
            case '>': {
                    
                if (str[startPos.idx + 1] == '=') {
                    len = 2;
                    token = { .kind = TK_OP_GREATER_THAN_OR_EQUAL };
                } else if (str[startPos.idx + 1] == '>') {
                    len = 2;
                    token = { .kind = TK_OP_SHIFT_RIGHT };
                } else {
                    token = { .kind = TK_OP_GREATER_THAN };
                }

                break;
                
            }

            case '=': {
                    
                if (str[startPos.idx + 1] == '=') {
                    len = 2;
                    token = { .kind = TK_OP_BOOL_EQUAL };
                } else {
                    token = { .kind = TK_EQUAL };
                }

                break;
                
            }

            case '&': {
                    
                if (str[startPos.idx + 1] == '&') {
                    len = 2;
                    token = { .kind = TK_OP_BOOL_AND };
                } else {
                    token = { .kind = TK_OP_AND };
                }

                break;
                
            }
                
            case '|': {
                    
                if (str[startPos.idx + 1] == '|') {
                    len = 2;
                    token = { .kind = TK_OP_BOOL_OR };
                } else {
                    token = { .kind = TK_OP_OR };
                }

                break;
                
            }

            case '^': {
                    
                token = { .kind = TK_OP_XOR };
                break;
                
            }

            case '~': {
                    
                token = { .kind = TK_OP_NEGATION };
                break;
                
            }
                
            case '#': {
                    
                const int len = parseIdentifier(str + startPos.idx + 1);
                Directive directive = directiveLookup(str, len);
                    
                if (directive > 0) {
                    token.detail = TD_CD_BEGIN + directive + 1;
                } else {
                    token.detail = TD_NONE;
                }

                token.kind = TK_DIRECTIVE;

                break;

            }
                
            case '.': {

                if (str[startPos.idx + 1] == '.') {
                    len = 2;
                    token = { .kind = TK_OP_CONCATENATION };
                } else {
                    token = { .kind = TK_OP_MEMBER_SELECTION };
                }

                break;
                
            }
                
            case ':': {
                    
                if (str[startPos.idx + 1] == ':') {
                    len = 2;
                    token = { .kind = TK_SCOPE_RESOLUTION };
                } else {
                    token = { .kind = TK_STATEMENT_BEGIN };
                }

                break;

            }
                
            case ';': {
                    
                token = { .kind = TK_STATEMENT_END };
                break;
                
            }
                
            case '{': {
                    
                token = { .kind = TK_SCOPE_BEGIN };
                break;
                
            }
                
            case '}': {
                    
                token = { .kind = TK_SCOPE_END };
                break;
                
            }
                
            case '[': {
                    
                token = { .kind = TK_ARRAY_BEGIN };
                break;
                
            }
                
            case ']': {
                    
                token = { .kind = TK_ARRAY_END };
                break;
                
            }
                
            case '_': {
                    
                token = { .kind = TK_SKIP };
                break;
                
            }
                
            case ',': {
                    
                token = { .kind = TK_LIST_SEPARATOR };
                break;
                
            }

            case '(': {
                    
                token = { .kind = TK_PARENTHESIS_BEGIN };
                break;
                
            }

            case ')': {
                    
                token = { .kind = TK_PARENTHESIS_END };
                break;
                
            }

            case EOS: {
                    
                token = { .kind = TK_END };
                break;
                
            }
                
            default: { 
                    
                if (isIdentifierStart(ch)) {

                    QualifiedName stackName;
                    QualifiedName* name = val ? new QualifiedName() : &stackName;
                    token = parseQualifiedName(span, str + startPos.idx, name, &len);

                    if (val) val->any = (void*) name;

                } else if (isNumberStart(ch)) {
                    
                    len = 0;
                    uint64_t num = 0;
                    TokenDetail dtype = parseNumber(str + startPos.idx, &len, &num);

                    if (val) val->ival = num;
                    token.kind = TK_NUMBER;
                    token.detail = dtype;

                }

                break;

            }
            
        }

        span->start = startPos;
        span->end.ln = startPos.ln + ln;
        span->end.idx = startPos.idx + len - 1;

        return token;

    }

    Token nextFileName(Span* const span, TokenValue* val) {

        char* const str = span->str;

        int err = 0;
        span->end.idx++;
        Pos startPos = skipWhitespacesAndComments(str, span->end, &err);
        if (err) {
            span->start = startPos;
            Logger::log(Logger::ERROR, "Unterminated comment!", span, 1);
            return toToken(Err::UNTERMINATED_COMMENT);
        }

        int idx = span->end.idx;
        
        while (1) {

            const char ch = str[idx];
            if (isIdentifierChar(ch) || ch == '.' || ch == '\\' || ch == '/') {
                idx++;
                continue;
            } else {
                break;
            }

        }

        val->str = new String(str, idx - span->end.idx);
        
        span->start = startPos;
        span->end.ln = startPos.ln;
        span->end.idx = startPos.idx + idx - 1;

        return { .kind = TK_FILE };

    }

    Token peekTokenSkipDecorators(Span* const span, TokenValue* val) {

        char* const str = span->str;
        int idx = span->end.idx + 1;

        Token token;
        while (1) {

            const char ch = str[idx];
            idx++;

            if (ch == POINTER) continue;
            if (ch == ARRAY_BEGIN) {

                // TODO : To a inline function
                int cnt = 1;
                while (1) {

                    const char ch = str[idx];
                    idx++;

                    if (ch == ARRAY_END && cnt <= 1) break;
                    if (ch == ARRAY_BEGIN) cnt++;
                
                }

                continue;

            }

            if (!isWhitespace(ch)) {

                span->end.idx = idx - 2;
                return peekToken(span, val);

            }

        }

        return toToken(TokenKind::TK_NONE);

    }

    int findBlockEnd(Span* const span, const char bCh, const char eCh) {
        
        int ln = 0;
        int idx = span->end.idx + 1;
        Pos startPos = { .idx = idx, .ln = span->start.ln };
        char* const str = span->str;

        int toClose = 1;

        while (1) {

            const char ch = str[idx];

            if (ch == '\n') {
                ln++;
            } else if (ch == bCh) {
                toClose++;
            } else if (ch == eCh) {
                toClose--;
                if (toClose <= 0) break;            
            } else if (ch == '\0') {
                span->end.ln += ln;
                span->end.idx = idx;
                return Err::UNEXPECTED_END_OF_FILE;
            }

            idx++;
        
        }

        const int len = idx - span->end.idx - 1;

        span->start = startPos;
        span->end.ln += ln;
        span->end.idx = idx;

        return len;

    }

}
