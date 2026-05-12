#include "../../src/registry.h"
#include "../../src/file_system.h"
#include "lsp.h"



namespace Reg {

    Unit* get(FileSystem::Handle fhnd) {
        FileSystem::FileInfo* info = FileSystem::getFileInfo(fhnd);
        if (!info->userData) return NULL;

        Lsp::FileData* data = (Lsp::FileData*) info->userData;
        return &data->unit[data->committedIdx];
    }

    void invalidate(FileSystem::Handle fhnd) {
        FileSystem::FileInfo* info = FileSystem::getFileInfo(fhnd);
        if (!info->userData) return;

        Lsp::FileData* data = (Lsp::FileData*) info->userData;
        Unit* unit = (Unit*) &data->unit[data->committedIdx];
        unit->ast = NULL;
        unit->reg = NULL;
    }

}
