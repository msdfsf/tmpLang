#pragma once

namespace Compiler {

    enum OutLangs {
        ITSELF_CONSOLE_LANG = 1 << 0,
        C_LANG              = 1 << 1,
    };

    enum BuildCommands {
        TRANSLATE,
        BUILD,
        RUN,
    };

    extern char* mainFile;
    extern char* outFile;
    extern char* outDir;

    extern int command;
    extern int outLangs;
    extern int debugInfo;
    
    int compile();

}
