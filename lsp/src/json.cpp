#include "json.h"
#include "../../src/error.h"

// #include <cctype>
#include <cstdlib>
#include <cstring>
#include <cmath>

namespace Json {

    std::vector<std::string> buffers(1);

    Value::Value() : type(JS_NULL) {}
    Value::Value(JsonType t) : type(t) {}
    Value::Value(bool b) : type(JS_BOOL), boolean(b) {}
    Value::Value(int n) : type(JS_NUMBER), number(static_cast<double>(n)) {}
    Value::Value(double n) : type(JS_NUMBER), number(n) {}
    Value::Value(const char* s) : type(JS_STRING), string(s) {}
    Value::Value(const String& s) : type(JS_STRING), string(s) {}
    Value::Value(const Array& a) : type(JS_ARRAY), array(new Array(a)) {}
    Value::Value(const Object& o) : type(JS_OBJECT), object(new Object(o)) {}

    void setThreadsCount(const unsigned int count) {
        buffers.resize(count);
    }

    void skipWhitespace(String str, int* idx) {
        while (*idx < str.len && std::isspace(str.buff[*idx])) {
            (*idx)++;
        }
    }

    int parseString(String str, Value* out, int* idx) {
   
        if (str.buff[*idx] != '"') return Err::UNEXPECTED_SYMBOL;
        (*idx)++;

        int start = *idx;
        while (*idx < str.len && str[*idx] != '"') {
            if (str[*idx] == '\\' && *idx + 1 < str.len) {
                (*idx)++;
            }
            (*idx)++;
        }
        if (*idx >= str.len) return Err::UNEXPECTED_END_OF_FILE;

        int len = *idx - start;
        char* val = (char*) malloc(len + 1);
        memcpy(val, str.buff + start, len);
        val[len] = '\0';

        out->type = JS_STRING;
        out->string.buff = val;
        out->string.len = len;

        (*idx)++;
        return Err::OK;

    }

    int parseNull(const String& str, Value* out, int* idx) {
        
        if (strncmp(str.buff + *idx, "null", 4) == 0) {
            out->type = JS_NULL;
            (*idx) += 4;
            return 0;
        }

        return Err::UNEXPECTED_SYMBOL;
    
    }

    int parseBool(const String& str, Value* out, int* idx) {
        
        if (strncmp(str.buff + *idx, "true", 4) == 0) {
            out->type = JS_BOOL;
            out->boolean = 1;
            (*idx) += 4;
            return 0;
        }
        
        if (strncmp(str.buff + *idx, "false", 5) == 0) {
            out->type = JS_BOOL;
            out->boolean = 0;
            (*idx) += 5;
            return 0;
        }
        
        return Err::UNEXPECTED_SYMBOL;
    
    }

    int parseNumber(String str, Value* out, int* idx) {

        const char* begin = str.buff + *idx;
        char* end;
        double num = strtod(begin, &end);
        if (end == begin) return Err::UNEXPECTED_SYMBOL;

        out->type = JS_NUMBER;
        out->number = num;

        *idx += (end - begin);
        return Err::OK;
    
    }

    int parseValue(String str, Value* out, int* idx);

    int parseArray(String str, Value* out, int* idx) {
        
        if (str.buff[*idx] != '[') return Err::UNEXPECTED_SYMBOL;
        (*idx)++;

        out->type = JS_ARRAY;
        out->array = new Array();

        skipWhitespace(str, idx);
        if (str[*idx] == ']') {
            (*idx)++;
            return 0;
        }

        while (1) {

            Value* elem = new Value();
            int err = parseValue(str, elem, idx);
            if (err < 0) return err;

            out->array->items.push_back(elem);

            skipWhitespace(str, idx);
            if (str[*idx] == ']') {
                (*idx)++;
                break;
            }
            if (str[*idx] != ',') return Err::UNEXPECTED_SYMBOL;
            (*idx)++;

        }

        return Err::OK;

    }

    int parseObject(const String& str, Value* out, int* idx) {
        
        if (str.buff[*idx] != '{') return Err::UNEXPECTED_SYMBOL;
        (*idx)++;

        out->type = JS_OBJECT;
        out->object = new Object();

        skipWhitespace(str, idx);
        if (str.buff[*idx] == '}') {
            (*idx)++;
            return 0;
        }

        while (1) {

            Value keyVal;
            int err = parseString(str, &keyVal, idx);
            if (err < 0) return err;

            skipWhitespace(str, idx);
            if (str.buff[*idx] != ':') return Err::UNEXPECTED_SYMBOL;
            (*idx)++;

            Value* val = new Value();
            err = parseValue(str, val, idx);
            if (err < 0) return err;

            Pair pair;
            pair.key = keyVal.string;
            pair.value = *val;
            out->object->pairs.push_back(pair);

            skipWhitespace(str, idx);
            if (str.buff[*idx] == '}') {
                (*idx)++;
                break;
            }
            if (str.buff[*idx] != ',') return Err::UNEXPECTED_SYMBOL;
            (*idx)++;
        }

        return 0;
    }

    int parseValue(String str, Value* out, int* idx) {

        char ch = str.buff[*idx];

        if (ch == '{') return parseObject(str, out, idx);
        if (ch == '[') return parseArray(str, out, idx);
        if (ch == '"') return parseString(str, out, idx);
        if (ch == 'n') return parseNull(str, out, idx);
        if (ch == 't' || ch == 'f') return parseBool(str, out, idx);
        if (ch == '-' || isdigit((unsigned char)ch)) return parseNumber(str, out, idx);

        return Err::UNEXPECTED_SYMBOL;
    }



    int parse(String str, Value* root) {
        int idx = 0;
        skipWhitespace(str, &idx);
        return parseValue(str, root, &idx);
    }

    void dumpValue(Value* js, std::string& buffer) {

        switch (js->type) {
            
            case JS_ARRAY: {
                
                buffer += '[';

                const int size = js->array->items.size();
                for (int i = 0; i < size; i++) {
                    dumpValue(js->array->items[i], buffer);
                    if (i + 1 != size) buffer += ',';
                }

                buffer += ']';

                break;
            
            }

            case JS_OBJECT: {

                buffer += '{';

                const int size = js->object->pairs.size();
                for (int i = 0; i < size; i++) {
                    
                    Pair val = js->object->pairs[i];

                    buffer += '"' + std::string(val.key.buff, val.key.len) + '"';
                    buffer += ':';
                    dumpValue(&val.value, buffer);
                    
                    if (i + 1 != size) buffer += ',';
                
                }

                buffer += '}';
                
                break;

            }

            case JS_BOOL: {

                buffer += js->boolean ? "true" : "false";
                break;
            
            }

            case JS_NULL: {

                buffer += "null";
                break;
            
            }

            case JS_NUMBER: {
                
                double num = js->number;
                if (std::floor(num) == num) {
                    buffer += std::to_string(static_cast<int64_t>(num));
                } else {
                    buffer += std::to_string(num);
                }
                break;
            
            }

            case JS_STRING: {
                
                // TODO: escape string
                buffer += '"';
                buffer += { js->string.buff, js->string.len };
                buffer += '"';
                break;

            }
        
            default: {
                break;
            }

        }

    }

    String dump(Value* js) {

        std::string& buffer = buffers[0];
        buffer.clear();

        dumpValue(js, buffer);

        return { buffer.data(), buffer.size() };

    }

    Json::Value value(Json::Object obj, const char* key) {

        for (int i = 0; i < obj.pairs.size(); i++) {
            Pair pair = obj.pairs[i];
            if (!memcmp(pair.key.buff, key, pair.key.len) && key[pair.key.len] == '\0') {
                return pair.value;
            }
        }

        return { JS_INVALID };

    }

    void freeParsedValue(Value val) {
        
        switch (val.type) {
            
            case JS_STRING: {
                free(val.string.buff);
                break;
            }
            
            case JS_ARRAY: {
                for (auto item : val.array->items) {
                    freeParsedValue(item);
                    delete item;
                }
                delete val.array;
                break;
            }
            
            case JS_OBJECT: {
                for (auto& pair : val.object->pairs) {
                    freeParsedValue(pair.value);
                    free(pair.key.buff);
                }
                delete val.object;
                break;
            }
            
            default: {
                break;
            }
        
        }
        
        val.type = JS_NULL;
    
    }

    void freeSyntheticValue(Value val) {
        
        switch (val.type) {
            
            case JS_STRING: {
                break;
            }
            
            case JS_ARRAY: {
                for (auto item : val.array->items) {
                    freeSyntheticValue(item);
                    delete item;
                }
                delete val.array;
                break;
            }
            
            case JS_OBJECT: {
                for (auto& pair : val.object->pairs) {
                    freeSyntheticValue(pair.value);
                }
                delete val.object;
                break;
            }
            
            default: {
                break;
            }
        
        }
        
        val.type = JS_NULL;
    
    }

}