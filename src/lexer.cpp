#include "lexer.h"
#include "error.h"
#include "syntax.h"
#include "logger.h"
#include "utils.h"

// Expects null terminated buffer
namespace Lex {

    inline int isIdentifierStart(const char ch) {
        return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_';
    }

    inline int isIdentifierChar(const char ch) {
        return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
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

    int parseCharLiteral(char* const str, int* len, uint64_t* out) {
        
        int idx = 0;
        uint64_t tmpOut = 0;

        int size = 0;
        while (1) {

            idx++;
            
            char ch = str[idx - 1];
            if (ch == CHAR_LITERAL) break;
            
            if (ch == ESCAPE_CHAR) {
                
                if (str[idx] == 'x') {

                    int hexLen;
                    ch = parseHexInt(str + idx + 1, &hexLen);
                    if (hexLen == 0) {
                        // Logger::log(Logger::ERROR, "At least one hex digit required!", loc, 1);
                        return Err::UNEXPECTED_SYMBOL;
                    }
                    idx += hexLen;
                
                } else {
                    
                    ch = parseEscapeChar(str, &idx);
                    if (ch == -1) {
                        // parseHexInt(str, &idx);
                        // Logger::log(Logger::ERROR, ERR_STR(Err::UNSUPPORTED_ESCAPE_SEQUENCE), loc, 1);
                        return Err::UNSUPPORTED_ESCAPE_SEQUENCE;
                    }
                
                }
            
            }
            
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
            //Logger::log(Logger::ERROR, ERR_STR(Err::DATA_TYPE_SIZE_EXCEEDED), loc, len);
            return Err::DATA_TYPE_SIZE_EXCEEDED;
        }

        tmpOut >>= (8 - size) * 8;

        *len = idx;
        *out = tmpOut;

        return size;

    }

    int parseStringLiteral(char* const str, int* len, StringInitialization** initOut) {

        int idx = 0;

        while (1) {

            const char ch = str[idx];
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

    /*
    Lex::Token parseScopeNames(INamedVar* var, char* const str, Location* const loc) {

        Location startLocation = *loc;

        char* word = str + loc->idx;
        int wordLen = Utils::findVarEnd(word);

        loc->idx += wordLen;

        if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        while (CMP_TWO_CHARS(str + loc->idx, SCOPE_RESOLUTION_SYMBOL)) {

            INamedLoc* scName = new INamedLoc(word, wordLen, getLocationStamp(&startLocation));
            var->scopeNames.push_back(scName);

            loc->idx += 2;
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            word = str + loc->idx;
            wordLen = Utils::findVarEnd(word);

            startLocation = *loc;
            loc->idx += wordLen;

        }

        var->name = word;
        var->nameLen = wordLen;

        return Err::OK;

    }
    */

    // expects that str starts at word
    // TODO: better name, as it skips also variable name
    int skipScopeNames(char* const str, Location* const loc) {

        while (1) {

            char* const word = str + loc->idx;
            const int wordLen = Utils::findVarEnd(word);

            loc->idx += wordLen;
            
            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

            if (CMP_TWO_CHARS(str + loc->idx, SCOPE_RESOLUTION_SYMBOL)) {
                loc->idx += 2;
            } else {
                loc->idx = (word - str) + wordLen;
                return Err::OK;
            }

            if (Utils::skipWhiteSpacesAndComments(str, loc) < 0) return Err::UNEXPECTED_END_OF_FILE;

        }

    }




    unsigned int hash(const char* str, int len) {
        unsigned int hash = 5381 ^ 0x12345678;
        while (len > 0) {
            hash = ((hash << 5) + hash) ^ (unsigned char) str[len--];
        }
        return hash;
    }

    inline Keyword keywordLookup(const char* str, const int len) {

        const unsigned int h = hash(str, len);
        if (h >= KW_TABLE_SIZE) return KW_VOID;
        
        return (Keyword) keywordTable[h];

    }
    
    inline Directive directiveLookup(const char* str, const int len) {

        const unsigned int h = hash(str, len) + 1;
        if (h >= KW_TABLE_SIZE) return CD_NONE;
        
        return (Directive) directivesTable[h];

    }




    Token nextToken(Location* const loc, TokenValue* val) {

        int ln = 0;
        int idx = loc->eidx;
        char* const str = loc->str;

        Token token;
        while (1) {

            const char ch = str[idx];
            idx++;

            if (ch == CHAR_LITERAL) {

                int len = 0;
                uint64_t ch = 0;
                const int size = parseCharLiteral(str + idx, &len, &ch);
                if (size < 0) {
                    Logger::log(Logger::ERROR, "Wrong char literal format!", loc, 1);
                    return toToken(size);
                }

                if (val) val->ival = ch;
                token.kind = TK_CHAR;
                token.detail = size;

                idx += len;

            } else if (ch == STRING_LITERAL) {

                int len = 0;
                StringInitialization* init;
                const int err = parseStringLiteral(str + idx, &len, &init);
                if (err < 0) {
                    Logger::log(Logger::ERROR, "Wrong string literal format!", loc, 1);
                    return toToken(err);
                }

                if (val) val->any = init;
                token.kind = TK_STRING;

                idx += len;

            } else if (isIdentifierStart(ch)) {

                int len = 1;
                while (isIdentifierChar(str[idx + len])) {
                    len++;
                }

                Keyword keyword = keywordLookup(str, len);

                if (keyword > 0) {
                    token.kind = TK_KEYWORD;
                    token.detail = keyword;
                } else {
                    token.encoded = TK_IDENTIFIER;
                }

            } else if (isNumberStart(ch)) {
                
                int len = 0;
                uint64_t num = 0;
                TokenDetail dtype = parseNumber(str + idx, &len, &num);

                if (val) val->ival = num;
                token.kind = TK_NUMBER;
                token.detail = dtype;
                
                idx += len;

            } else if (ch == '#') {

                const int len = parseIdentifier(str + idx);
                Directive directive = directiveLookup(str, len);
                if (directive > 0) {
                    token.kind = TK_KEYWORD;
                    token.detail = KW_COUNT + directive;
                } else {
                    token = toToken(Err::UNKNOWN_DIRECTIVE);
                }

            } else if (ch == POINTER) {

                token.encoded = TK_POINTER;
            
            } else if (ch == '=') {

                token.encoded = TK_EQUAL;

            } else if (ch == ADDRESS) {

                token.encoded = TK_ADDRESS;

            } else if (ch == '.') {

                if (str[idx + 1] == '.' && str[idx + 2] == '.') {
                    idx += 2;
                    token.encoded = TK_THE_REST;
                }

                token.encoded = TK_MEMBER_SELECTION;

            } else if (ch == ':') {

                if (str[idx + 1] == ':') {
                    idx++;
                    token.encoded = TK_SCOPE_RESOLUTION;
                }

                token.encoded = TK_STATEMENT_BEGIN;
                
            } else if (ch == ';') {

                token.encoded = TK_STATEMENT_END;

            } else if (ch == '{') {

                token.encoded = TK_SCOPE_BEGIN;
            
            } else if (ch == '}') {

                token.encoded = TK_SCOPE_END;

            } else if (ch == '[') {

                token.encoded = TK_ARRAY_BEGIN;
            
            } else if (ch == ']') {

                token.encoded = TK_ARRAY_END;

            } else if (ch == '_') {

                token.encoded = TK_SKIP_VARIABLE;

            } else if (ch == ',') {

                token.encoded = TK_LIST_SEPARATOR;

            } else if (ch == 'b') {

                token.encoded = TK_RAW;

            } else if (ch == EOL) {
                
                ln++;
                continue;
            
            } else if (ch == EOF) {

                token.encoded = TK_END;
            }

            break;

        }

        loc->eln += ln;
        loc->eidx = idx - 1;

        return token;

    }

    Token nextTokenSkipDecorators(Location* const loc, TokenValue* val = NULL) {
        
        int ln = 0;
        int idx = loc->eidx;
        char* const str = loc->str;

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

            } else {

                loc->eln = ln;
                loc->eidx = idx - 1;
                return nextToken(loc, val);
            
            }

        }

        return toToken(TokenKind::TK_NONE);

    }

    int findBlockEnd(Location* const loc, const char bCh, const char eCh) {
        
        int ln = 0;
        int idx = loc->eidx;
        char* const str = loc->str;

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
                loc->eln += ln;
                loc->eidx = idx;
                return Err::UNEXPECTED_END_OF_FILE;
            }

            idx++;
        
        }

        const int len = idx - loc->eidx;

        loc->eln += ln;
        loc->eidx = idx + 1;

        return len;

    }

}
