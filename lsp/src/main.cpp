#include "../../src/globals.h"
#include "../../src/error.h"

#include "json.h"
#include "lsp.h"
#include "comm_provider.h"



enum LogType {
    LOG_ERROR = 1,
    LOG_WARNING = 2,
    LOG_INFO = 3,
    LOG_LOG = 4
};



CommProvider::Info commInfo = { CommProvider::CT_STD };



static int hexVal(char c) {

    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
    if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
    return -1;

}

// Convert file:// URI -> filesystem path reusing the input buffer
// returns the new length of the path
int uriToPathInplace(String* uri) {

    const char* prefix = "file://";
    const int prefixLen = 7;
    int idx = 0;

    if (uri->len >= prefixLen && strncmp(uri->buff, prefix, prefixLen) == 0) {
        idx = prefixLen;
    }

    int outIdx = 0;
    while (idx < uri->len) {

        if (uri->buff[idx] == '%' && 
            idx + 2 < uri->len &&
            std::isxdigit((unsigned char) uri->buff[idx + 1]) &&
            std::isxdigit((unsigned char) uri->buff[idx + 2])
        ) {

            int high = hexVal(uri->buff[idx + 1]);
            int low = hexVal(uri->buff[idx + 2]);
            if (high >= 0 && low >= 0) {
                uri->buff[outIdx] = (char)((high << 4) | low);
                outIdx++;
                idx += 3;
                continue;
            }
        }

        char ch = uri->buff[idx++];
        #ifdef _WIN32
            if (ch == '/') ch = '\\';
        #endif
        uri->buff[outIdx] = ch;
        outIdx++;

    }

    uri->buff[outIdx] = '\0';
    uri->len = outIdx;

    return outIdx;

}



#include <fcntl.h>
#include <io.h>
int main() {

    if (_setmode(_fileno(stdin), _O_BINARY) == -1) {
        perror("Cannot set stdin to binary mode");
        return 1;
    }

    if (_setmode(_fileno(stdout), _O_BINARY) == -1) {
        perror("Cannot set stdout to binary mode");
        return 1;
    }

    int beOrNotToBe = false;
    while (1) {

        fprintf(stderr, "\nLOOP\n");

        CommProvider::Message msg;
        
        int err = CommProvider::read(&commInfo, &msg);
        fprintf(stderr, "msg-err: %s\n", ERR_STR(err));
        if (err == Err::UNEXPECTED_END_OF_FILE) continue;
        if (err < 0) break;

        Json::Object root = *msg.body.object;

        int id = Json::value(root, "id").number;
        Lsp::RequestMethod method = Lsp::toRequestMethod(Json::value(root, "method"));

        switch (method) {

            case Lsp::RM_INITIALIZE: {

                fprintf(stderr, "RM_INITIALIZE\n");

                Json::Value response = { Json::Object {
                    {"jsonrpc", "2.0"},
                    {"id", id},
                    {"result", Json::Object{
                        {"capabilities", Json::Object{
                            {"textDocumentSync", Json::Object{
                                {"openClose", true},
                                {"change", Lsp::TextDocument::SK_INCREMENTAL}
                            }},
                            {"documentSymbolProvider", true}
                        }},
                        {"serverInfo", Json::Object{
                            {"name", "vi-lsp"},
                            {"version", "1.0.0"}
                        }}
                    }}
                }};

                CommProvider::write(&commInfo, response);
                Json::freeSyntheticValue(response);

                break;
            
            }

            case Lsp::RM_TEXT_DOCUMENT_DID_OPEN: {

                fprintf(stderr, "RM_TEXT_DOCUMENT_DID_OPEN\n");

                Json::Value paramVal = Json::value(root, "params");
                if (paramVal.type != Json::JS_OBJECT) break;

                Json::Value docVal = Json::value(*paramVal.object, "textDocument");
                if (docVal.type != Json::JS_OBJECT) break;

                Json::Value uriVal = Json::value(*docVal.object, "uri");
                if (uriVal.type != Json::JS_STRING) break;

                Json::Value textVal = Json::value(*docVal.object, "text");
                if (textVal.type != Json::JS_STRING) break;

                Lsp::TextDocument::didOpen(uriVal.string, textVal.string);
                if (err < 0) break;

                break;

            }

            case Lsp::RM_TEXT_DOCUMENT_DID_CHANGE: {
                
                fprintf(stderr, "RM_TEXT_DOCUMENT_DID_CHANGE\n");

                Json::Value paramVal = Json::value(root, "params");
                if (paramVal.type != Json::JS_OBJECT) break;

                Json::Value docVal = Json::value(*paramVal.object, "textDocument");
                if (docVal.type != Json::JS_OBJECT) break;

                Json::Value uriVal = Json::value(*docVal.object, "uri");
                if (uriVal.type != Json::JS_STRING) break;

                Json::Value arrVal = Json::value(root, "contentChanges");
                if (arrVal.type != Json::JS_ARRAY) break;

                const int err = Lsp::TextDocument::didChange(uriVal.string, *arrVal.array);
                if (err < 0) break;

                break;

            }

            case Lsp::RM_TEXT_DOCUMENT_DID_CLOSE: {
                
                fprintf(stderr, "RM_TEXT_DOCUMENT_DID_CLOSE\n");

                Json::Value paramVal = Json::value(root, "params");
                if (paramVal.type != Json::JS_OBJECT) break;

                Json::Value docVal = Json::value(*paramVal.object, "textDocument");
                if (docVal.type != Json::JS_OBJECT) break;

                Json::Value uriVal = Json::value(*docVal.object, "uri");
                if (uriVal.type != Json::JS_STRING) break;

                const int err = Lsp::TextDocument::didClose(uriVal.string);
                if (err < 0) break;

                break;

            }

            case Lsp::RM_TEXT_DOCUMENT_DOCUMENT_SYMBOL: {

                fprintf(stderr, "RM_TEXT_DOCUMENT_DOCUMENT_SYMBOL\n");

                Json::Value paramVal = Json::value(root, "params");
                if (paramVal.type != Json::JS_OBJECT) break;

                Json::Value docVal = Json::value(*paramVal.object, "textDocument");
                if (docVal.type != Json::JS_OBJECT) break;

                Json::Value uriVal = Json::value(*docVal.object, "uri");
                if (uriVal.type != Json::JS_STRING) break;

                fprintf(stderr, "URI: %.*s\n", (int) uriVal.string.len, uriVal.string.buff);

                Lsp::TextDocument::documentSymbol(uriVal.string, &commInfo);

                break;

            }

            case Lsp::RM_INITIALIZED: {
                break;
            }

            case Lsp::RM_TEXT_DOCUMENT_HOVER: {
                fprintf(stderr, "RM_TEXT_DOCUMENT_HOVER\n");
                break;
            }

            case Lsp::RM_TEXT_DOCUMENT_DEFINITION: {
                fprintf(stderr, "RM_TEXT_DOCUMENT_DEFINITION\n");
                break;
            }

            case Lsp::RM_SHUTDOWN: {
                
                beOrNotToBe = 0;

                Json::Value response = {{
                    {"jsonrpc", "2.0"},
                    {"id", id},
                    {"result", Json::Value()}
                }};

                CommProvider::write(&commInfo, response);
                Json::freeSyntheticValue(response);

                break;
            
            }

            case Lsp::RM_EXIT: {
                return beOrNotToBe ? 0 : 1;    
            }
            
            default:
                break;
        }

    }

    return 0;

}