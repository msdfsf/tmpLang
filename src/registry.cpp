#include "registry.h"
#include "file_system.h"



namespace Reg {

    Unit* get(FileSystem::Handle fhnd) {
        FileSystem::FileInfo* finfo = FileSystem::getFileInfo(fhnd);
        if (!finfo->userData) {
            Unit* unit = (Unit*) alloc(alc, sizeof(Unit));
            
            unit->ast = (AstContext*) alloc(alc, sizeof(AstContext));
            unit->reg = (AstRegistry*) alloc(alc, sizeof(AstRegistry));
            
            Ast::init(unit->ast);
            Ast::init(unit->reg);

            finfo->userData = unit;
        }

        return (Unit*) finfo->userData;
    }

    void invalidate(FileSystem::Handle fhnd) {
        FileSystem::FileInfo* finfo = FileSystem::getFileInfo(fhnd);
        if (!finfo->userData) return;

        Unit* unit = (Unit*) finfo->userData;
        // TODO : release
        unit->ast = NULL;
        unit->reg = NULL;
    }

}
