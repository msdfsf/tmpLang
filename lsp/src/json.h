#pragma once
#include "../../src/globals.h"
#include <vector>

namespace Json {

    enum JsonType {

        JS_INVALID = -1,
        JS_NULL,
        JS_BOOL,
        JS_NUMBER,
        JS_STRING,
        JS_ARRAY,
        JS_OBJECT
    
    };

    struct Array;
    struct Object;

    struct Value {
        
        JsonType type;
        union {
            int boolean;
            double number;
            String string;
            Array* array;
            Object* object;
        };

        Value();
        Value(JsonType t);
        Value(bool b);
        Value(int n);
        Value(double n);
        Value(const char* s);
        Value(const String& s);
        Value(const Array& a);
        Value(const Object& o);
    
    };

    struct Pair {
        
        String key;
        Value value;

        Pair() {};
        Pair(const char* k, const Value& v) : key(k), value(v) {}
        Pair(const String& k, const Value& v) : key(k), value(v) {}
    
    };

    struct Array {
        std::vector<Value*> items;
    };

    struct Object {
        
        std::vector<Pair> pairs;

        Object() {}
        Object(std::initializer_list<Pair> init) : pairs(init) {}
    
    };

    int parse(String str, Json::Value* root);
    String dump(Json::Value* val);
    Json::Value value(Json::Object obj, const char* str);
    
    void freeParsedValue(Value val);
    void freeSyntheticValue(Value val);

}