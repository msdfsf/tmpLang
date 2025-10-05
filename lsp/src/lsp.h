#pragma once
#include <string>
#include <map>
#include <unordered_map>

#include "../../src/globals.h"
#include "json.h"
#include "comm_provider.h"

namespace Lsp {

    enum SymbolKind {
        SK_FILE = 1,
        SK_MODULE = 2,
        SK_NAMESPACE = 3,
        SK_PACKAGE = 4,
        SK_CLASS = 5,
        SK_METHOD = 6,
        SK_PROPERTY = 7,
        SK_FIELD = 8,
        SK_CONSTRUCTOR = 9,
        SK_ENUM = 10,
        SK_INTERFACE = 11,
        SK_FUNCTION = 12,
        SK_VARIABLE = 13,
        SK_CONSTANT = 14,
        SK_STRING = 15,
        SK_NUMBER = 16,
        SK_BOOLEAN = 17,
        SK_ARRAY = 18,
        SK_OBJECT = 19,
        SK_KEY = 20,
        SK_NULL = 21,
        SK_ENUM_MEMBER = 22,
        SK_STRUCT = 23,
        SK_EVENT = 24,
        SK_OPERATOR = 25,
        SK_TYPE_PARAMETER = 26
    };

    struct FileInfo {
        
        String id;
        File file;

        // indexed by line number - 1
        // has to be updated on every change via computeLineOffsets
        std::vector<uint64_t> lineOffsets;

    };

    // they have to be kept in sync, use related functions
    // one is for string lookup from client
    // other for fast lookups while rendering or so
    inline std::map<String, FileInfo*> openedFiles;
    inline std::vector<FileInfo*> openedFilesLookup;

    int computeLineLengths(FileInfo* const info, const char* const text);

    FileInfo* getFileInfo(uint64_t id);



    enum RequestMethod {
        
        // lifecycle
        RM_INITIALIZE,
        RM_INITIALIZED,
        RM_SHUTDOWN,
        RM_EXIT,

        // window / client interaction
        RM_WINDOW_SHOW_MESSAGE,
        RM_WINDOW_SHOW_MESSAGE_REQUEST,
        RM_WINDOW_LOG_MESSAGE,
        RM_TELEMETRY_EVENT,
        RM_CLIENT_REGISTER_CAPABILITY,
        RM_CLIENT_UNREGISTER_CAPABILITY,
        RM_WORKSPACE_WORKSPACE_FOLDERS,
        RM_WORKSPACE_CONFIGURATION,

        // text document sync
        RM_TEXT_DOCUMENT_DID_OPEN,
        RM_TEXT_DOCUMENT_DID_CHANGE,
        RM_TEXT_DOCUMENT_DID_SAVE,
        RM_TEXT_DOCUMENT_DID_CLOSE,

        // diagnostics
        RM_TEXT_DOCUMENT_PUBLISH_DIAGNOSTICS,
        RM_WORKSPACE_DIAGNOSTIC,
        RM_WORKSPACE_DIAGNOSTIC_REFRESH,

        // language features
        RM_TEXT_DOCUMENT_HOVER,
        RM_TEXT_DOCUMENT_DEFINITION,
        RM_TEXT_DOCUMENT_DECLARATION,
        RM_TEXT_DOCUMENT_TYPE_DEFINITION,
        RM_TEXT_DOCUMENT_IMPLEMENTATION,
        RM_TEXT_DOCUMENT_REFERENCES,
        RM_TEXT_DOCUMENT_DOCUMENT_HIGHLIGHT,
        RM_TEXT_DOCUMENT_DOCUMENT_SYMBOL,
        RM_WORKSPACE_SYMBOL,
        RM_TEXT_DOCUMENT_CODE_ACTION,
        RM_TEXT_DOCUMENT_CODE_LENS,
        RM_CODE_LENS_RESOLVE,
        RM_TEXT_DOCUMENT_DOCUMENT_LINK,
        RM_DOCUMENT_LINK_RESOLVE,
        RM_TEXT_DOCUMENT_DOCUMENT_COLOR,
        RM_TEXT_DOCUMENT_COLOR_PRESENTATION,
        RM_TEXT_DOCUMENT_FORMATTING,
        RM_TEXT_DOCUMENT_RANGE_FORMATTING,
        RM_TEXT_DOCUMENT_ON_TYPE_FORMATTING,
        RM_TEXT_DOCUMENT_RENAME,
        RM_WORKSPACE_EXECUTE_COMMAND,
        RM_WORKSPACE_APPLY_EDIT,

        // file operations
        RM_WORKSPACE_DID_CREATE_FILES,
        RM_WORKSPACE_DID_RENAME_FILES,
        RM_WORKSPACE_DID_DELETE_FILES,
        RM_WORKSPACE_WILL_CREATE_FILES,
        RM_WORKSPACE_WILL_RENAME_FILES,
        RM_WORKSPACE_WILL_DELETE_FILES,

        // newer features
        RM_WORKSPACE_INLAY_HINT,
        RM_INLAY_HINT_RESOLVE,
        RM_TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL,
        RM_TEXT_DOCUMENT_SEMANTIC_TOKENS_RANGE,
        RM_TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL_DELTA,
        RM_WORKSPACE_SEMANTIC_TOKENS_REFRESH,
        RM_WORKSPACE_INLAY_HINT_REFRESH,
        RM_WORKSPACE_INLINE_VALUE_REFRESH,
        RM_WORKSPACE_CODE_LENS_REFRESH,
        RM_WORKSPACE_MONIKER,
        RM_TEXT_DOCUMENT_PREPARE_CALL_HIERARCHY,
        RM_CALL_HIERARCHY_INCOMING_CALLS,
        RM_CALL_HIERARCHY_OUTGOING_CALLS,
        RM_TEXT_DOCUMENT_PREPARE_TYPE_HIERARCHY,
        RM_TYPE_HIERARCHY_SUPERTYPES,
        RM_TYPE_HIERARCHY_SUBTYPES,

        // unknown / not implemented
        RM_UNKNOWN

    };

    static const std::unordered_map<std::string, RequestMethod> methodMap = {
        
        {"initialize", RM_INITIALIZE},
        {"initialized", RM_INITIALIZED},
        {"shutdown", RM_SHUTDOWN},
        {"exit", RM_EXIT},

        {"window/showMessage", RM_WINDOW_SHOW_MESSAGE},
        {"window/showMessageRequest", RM_WINDOW_SHOW_MESSAGE_REQUEST},
        {"window/logMessage", RM_WINDOW_LOG_MESSAGE},

        {"telemetry/event", RM_TELEMETRY_EVENT},

        {"client/registerCapability", RM_CLIENT_REGISTER_CAPABILITY},
        {"client/unregisterCapability", RM_CLIENT_UNREGISTER_CAPABILITY},

        {"workspace/workspaceFolders", RM_WORKSPACE_WORKSPACE_FOLDERS},
        {"workspace/configuration", RM_WORKSPACE_CONFIGURATION},
        {"workspace/symbol", RM_WORKSPACE_SYMBOL},
        {"workspace/diagnostic", RM_WORKSPACE_DIAGNOSTIC},
        {"workspace/diagnostic/refresh", RM_WORKSPACE_DIAGNOSTIC_REFRESH},
        {"workspace/executeCommand", RM_WORKSPACE_EXECUTE_COMMAND},
        {"workspace/applyEdit", RM_WORKSPACE_APPLY_EDIT},
        {"workspace/didCreateFiles", RM_WORKSPACE_DID_CREATE_FILES},
        {"workspace/didRenameFiles", RM_WORKSPACE_DID_RENAME_FILES},
        {"workspace/didDeleteFiles", RM_WORKSPACE_DID_DELETE_FILES},
        {"workspace/willCreateFiles", RM_WORKSPACE_WILL_CREATE_FILES},
        {"workspace/willRenameFiles", RM_WORKSPACE_WILL_RENAME_FILES},
        {"workspace/willDeleteFiles", RM_WORKSPACE_WILL_DELETE_FILES},
        {"workspace/inlayHint", RM_WORKSPACE_INLAY_HINT},
        {"workspace/semanticTokens/refresh", RM_WORKSPACE_SEMANTIC_TOKENS_REFRESH},
        {"workspace/inlayHint/refresh", RM_WORKSPACE_INLAY_HINT_REFRESH},
        {"workspace/inlineValue/refresh", RM_WORKSPACE_INLINE_VALUE_REFRESH},
        {"workspace/codeLens/refresh", RM_WORKSPACE_CODE_LENS_REFRESH},
        {"workspace/moniker", RM_WORKSPACE_MONIKER},

        {"textDocument/didOpen", RM_TEXT_DOCUMENT_DID_OPEN},
        {"textDocument/didChange", RM_TEXT_DOCUMENT_DID_CHANGE},
        {"textDocument/didSave", RM_TEXT_DOCUMENT_DID_SAVE},
        {"textDocument/didClose", RM_TEXT_DOCUMENT_DID_CLOSE},
        {"textDocument/publishDiagnostics", RM_TEXT_DOCUMENT_PUBLISH_DIAGNOSTICS},
        {"textDocument/hover", RM_TEXT_DOCUMENT_HOVER},
        {"textDocument/definition", RM_TEXT_DOCUMENT_DEFINITION},
        {"textDocument/declaration", RM_TEXT_DOCUMENT_DECLARATION},
        {"textDocument/typeDefinition", RM_TEXT_DOCUMENT_TYPE_DEFINITION},
        {"textDocument/implementation", RM_TEXT_DOCUMENT_IMPLEMENTATION},
        {"textDocument/references", RM_TEXT_DOCUMENT_REFERENCES},
        {"textDocument/documentHighlight", RM_TEXT_DOCUMENT_DOCUMENT_HIGHLIGHT},
        {"textDocument/documentSymbol", RM_TEXT_DOCUMENT_DOCUMENT_SYMBOL},
        {"textDocument/codeAction", RM_TEXT_DOCUMENT_CODE_ACTION},
        {"textDocument/codeLens", RM_TEXT_DOCUMENT_CODE_LENS},
        {"textDocument/documentLink", RM_TEXT_DOCUMENT_DOCUMENT_LINK},
        {"textDocument/documentColor", RM_TEXT_DOCUMENT_DOCUMENT_COLOR},
        {"textDocument/colorPresentation", RM_TEXT_DOCUMENT_COLOR_PRESENTATION},
        {"textDocument/formatting", RM_TEXT_DOCUMENT_FORMATTING},
        {"textDocument/rangeFormatting", RM_TEXT_DOCUMENT_RANGE_FORMATTING},
        {"textDocument/onTypeFormatting", RM_TEXT_DOCUMENT_ON_TYPE_FORMATTING},
        {"textDocument/rename", RM_TEXT_DOCUMENT_RENAME},
        {"textDocument/semanticTokens/full", RM_TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL},
        {"textDocument/semanticTokens/range", RM_TEXT_DOCUMENT_SEMANTIC_TOKENS_RANGE},
        {"textDocument/semanticTokens/full/delta", RM_TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL_DELTA},
        {"textDocument/prepareCallHierarchy", RM_TEXT_DOCUMENT_PREPARE_CALL_HIERARCHY},
        {"textDocument/prepareTypeHierarchy", RM_TEXT_DOCUMENT_PREPARE_TYPE_HIERARCHY},

        {"codeLens/resolve", RM_CODE_LENS_RESOLVE},
        {"documentLink/resolve", RM_DOCUMENT_LINK_RESOLVE},
        {"inlayHint/resolve", RM_INLAY_HINT_RESOLVE},

        {"callHierarchy/incomingCalls", RM_CALL_HIERARCHY_INCOMING_CALLS},
        {"callHierarchy/outgoingCalls", RM_CALL_HIERARCHY_OUTGOING_CALLS},

        {"typeHierarchy/supertypes", RM_TYPE_HIERARCHY_SUPERTYPES},
        {"typeHierarchy/subtypes", RM_TYPE_HIERARCHY_SUBTYPES}

    };

    RequestMethod toRequestMethod(Json::Value val);


    namespace TextDocument {

        enum SyncKind {

            SK_NONE = 0,
            SK_FULL = 1,
            SK_INCREMENTAL = 2

        };

        int didOpen(String uri, String text);
        int didChange(String uri, Json::Array changes);
        int didClose(String uri);

        int documentSymbol(const char* uri, CommProvider::Info* commInfo);

    }

}