#pragma once

#include "file_system.h"
#include "syntax.h"



namespace Reg {

    struct Unit {
        AstContext*  ast;
        AstRegistry* reg;
    };

    Unit* get(FileSystem::Handle file);

    void invalidate(FileSystem::Handle file);

}
