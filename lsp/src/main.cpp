#include "json.h"
#include "lsp.h"
#include "comm_provider.h"
#include "../../src/dynamic_arena.h"

#include <cstdio>



CommProvider::Info commInfo = { CommProvider::CT_STD };



constexpr JsonString operator ""_js(const char* s, size_t l) {
    return {(char*) s, l};
}

static bool match(JsonString str, const char* lit) {
    return (str.len == (int) strlen(lit) && strncmp(str.data, lit, str.len) == 0);
}

template <typename T>
static JsonString generateResponse(JsonWriter* js, T* result, int id) {

    jsonWriteObjectStart(js);
    jsonWriteKey(js, "jsonrpc"_js);
    jsonWriteStr(js, "2.0"_js);

    jsonWriteKey(js, "id"_js);
    jsonWriteInt(js, id);

    jsonWriteKey(js, "result"_js);
    jsonWriteObjectStart(js);

    Lsp::serialize(js, result);

    jsonWriteObjectEnd(js);
    jsonWriteObjectEnd(js);

    return jsonWriterCommit(js);

}

Arena::Container lspAllocatorArena;

#include <fcntl.h>
#include <io.h>
int main() {

    #ifdef _DEBUG
    while (!IsDebuggerPresent()) Sleep(100);
    #endif

    if (_setmode(_fileno(stdin), _O_BINARY) == -1) {
        perror("Cannot set stdin to binary mode");
        return 1;
    }

    if (_setmode(_fileno(stdout), _O_BINARY) == -1) {
        perror("Cannot set stdout to binary mode");
        return 1;
    }

    CommProvider::Err err;
    CommProvider::Info comm;
    comm.type = CommProvider::CT_STD;

    Arena::Container requestArena;
    Arena::init(&requestArena, 1024 * 1024 * 32);

    Arena::init(&lspAllocatorArena, 1024 * 1024 * 32);
    Lsp::Allocator lspAllocator = {
        .alloc   =(void* (*)(void*, size_t)) static_cast<void* (*) (Arena::Container*, size_t)>(&Arena::push),
        .context = &lspAllocatorArena
    };

    fprintf(stderr, "[LSP] Initialized\n");

    bool beOrNotToBe = true;
    while (beOrNotToBe) {
        JsonLex js = { 0 };
        JsonType token;

        CommProvider::Message msg;
        err = CommProvider::read(&comm, &msg);

        if (err == CommProvider::ERR_CLOSED) break;
        if (err != CommProvider::OK) continue;

        Arena::clear(&requestArena);
        jsonLexInit(&js, msg.body, "lsp_input"_js);



        // Extract:
        // "id": <number>
        // "method": <string>
        // "params": <array> -> as JsonString which
        token = jsonNext(&js);
        if (!jsonMatch(&js, token, JSON_OBJECT_OPEN)) {
            continue;
        }

        int id = -1;
        JsonString methodUri = { 0 };
        JsonString params;

        while (true) {

            token = jsonNext(&js);
            if (token == JSON_OBJECT_CLOSE) break;
            if (!jsonMatch(&js, token, JSON_KEY)) break;

            JsonString key = js.value.s;

            token = jsonNext(&js);
            if (match(key, "id")) {
                if (jsonMatch(&js, token, JSON_NUMBER)) {
                    id = (int) js.value.n;
                }
                continue;
            } else if (match(key, "method")) {
                if (jsonMatch(&js, token, JSON_STRING)) {
                    methodUri = js.value.s;
                }
                continue;
            }
            jsonSkipValue(&js, token);

        }


        if (js.errCode != 0) {
            // TODO
        }

        // Convert method uri to enum representation
        Lsp::T::RequestMethod method = Lsp::toRequestMethod(methodUri);

        // Dispatch requested method
        JsonWriter jsWr;
        char* buffer = (char*) requestArena.head->data;
        jsonWriterInit(&jsWr, buffer, requestArena.blockPayloadSize);

        JsonString resp;
        switch (method) {

            case Lsp::T::RM_INITIALIZE: {

                fprintf(stderr, "RM_INITIALIZE\n");

                Lsp::Initialize* params = Lsp::parse<Lsp::Initialize>(&js, &lspAllocator);
                Lsp::T::InitializeResult result = { 0 };

                // Sync Options
                auto* sync = Lsp::alloc<Lsp::T::TextDocumentSyncOptions>(&lspAllocator);
                sync->openClose = true;
                sync->change    = Lsp::T::TEXT_DOCUMENT_SYNC_KIND_INCREMENTAL;

                // Capabilities
                auto* caps = Lsp::alloc<Lsp::T::ServerCapabilities>(&lspAllocator);
                caps->textDocumentSync = sync;

                // Features
                caps->hoverProvider          = true;
                caps->definitionProvider     = true;
                caps->documentSymbolProvider = true;
                caps->declarationProvider    = true;
                caps->implementationProvider = true;

                // Server Info
                result.serverInfo = Lsp::alloc<Lsp::T::InitializeResult::_ServerInfo>(&lspAllocator);
                result.serverInfo->name    = "MyLspServer";
                result.serverInfo->version = "0.1.0";

                result.capabilities = caps;

                generateResponse(&jsWr, &result, id);
                break;

            }

            case Lsp::T::RM_TEXT_DOCUMENT_DID_OPEN: {

                fprintf(stderr, "RM_TEXT_DOCUMENT_DID_OPEN\n");

                using namespace Lsp::TextDocument;
                Lsp::handle(Lsp::parse<DidOpen>(&js, &lspAllocator));

                break;

            }

            case Lsp::T::RM_TEXT_DOCUMENT_DID_CHANGE: {

                fprintf(stderr, "RM_TEXT_DOCUMENT_DID_CHANGE\n");

                using namespace Lsp::TextDocument;
                Lsp::handle(Lsp::parse<DidChange>(&js, &lspAllocator));

                break;

            }

            case Lsp::T::RM_TEXT_DOCUMENT_DID_CLOSE: {

                fprintf(stderr, "RM_TEXT_DOCUMENT_DID_CLOSE\n");

                using namespace Lsp::TextDocument;
                Lsp::handle(Lsp::parse<DidClose>(&js, &lspAllocator));

                break;

            }

            case Lsp::T::RM_TEXT_DOCUMENT_DOCUMENT_SYMBOL: {

                fprintf(stderr, "RM_TEXT_DOCUMENT_DOCUMENT_SYMBOL\n");

                break;

            }

            case Lsp::T::RM_INITIALIZED: {
                Lsp::State::initialized = true;
                break;
            }

            case Lsp::T::RM_TEXT_DOCUMENT_HOVER: {
                fprintf(stderr, "RM_TEXT_DOCUMENT_HOVER\n");
                break;
            }

            case Lsp::T::RM_TEXT_DOCUMENT_DEFINITION: {
                fprintf(stderr, "RM_TEXT_DOCUMENT_DEFINITION\n");
                break;
            }

            case Lsp::T::RM_SHUTDOWN: {

                beOrNotToBe = 0;

                jsonWriteObjectStart(&jsWr);
                    jsonWriteKey(&jsWr, "jsonrpc"_js); jsonWriteStr(&jsWr, "2.0"_js);
                    jsonWriteKey(&jsWr, "id"_js);      jsonWriteInt(&jsWr, id);
                    jsonWriteKey(&jsWr, "result"_js);  jsonWriteNull(&jsWr);
                jsonWriteObjectEnd(&jsWr);

                JsonString resp = jsonWriterCommit(&jsWr);
                break;

            }

            case Lsp::T::RM_EXIT: {
                return beOrNotToBe ? 0 : 1;
            }

            default:
                break;
        }
        CommProvider::write(&commInfo, resp);

    }

    return 0;

}
