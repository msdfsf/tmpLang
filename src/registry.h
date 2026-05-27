#pragma once

#include "file_system.h"
#include "syntax.h"



namespace Extern { struct Library; }

namespace Reg {

    struct Unit {
        AstContext*  ast;
        AstRegistry* reg;

        // contains Extern::Library*
        DArray::Container* libs;
    };

    Unit* get(FileSystem::Handle file);
    void invalidate(FileSystem::Handle file);

    void addLibrary(Unit* unit, Extern::Library* lib);

}
