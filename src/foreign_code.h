#pragma once

#include "syntax.h"
#include "validator.h"



namespace Reg { struct Unit; }

namespace Extern {

    enum LibraryType {
        LT_STATIC,
        LT_SHARED,
    };

    enum LibraryLoadLevel {
        // Library is ready to be examined but no code will be able to run
        LL_INSPECT,
        // Library is fully loaded to memory and ready for execution
        LL_EXECUTE,
    };

    struct Library {
        String name;
        String libPath;  // Path to .lib or .a
        String dllPath;  // Path to .dll or .so
        bool   isStatic; // true if libPath is a 'true' static
        
        LibraryLoadLevel loadLevel;

        void* osHandle;
    };

    typedef Library* LibraryHandle;



    // Inits globals, so has to be called only once
    void init();

    Err::Err loadLibrary(Validator::ValidationContext* ctx, String name, LibraryLoadLevel level, LibraryHandle* out);
    
    // Ensures that function exists from previously loadaed library
    Err::Err ensureFunctionExists(Validator::ValidationContext* ctx, LibraryHandle lib, Function* fcn);

    // Binds function to real address from previosly loaded library
    // Triggers 'Hot' load of library, if not already 'Hot' loaded
    Err::Err bindFunction(Validator::ValidationContext* ctx, Library* lib, Function* fcn);

    // Invokes function from previously binded shared library
    Err::Err invoke(AstContext* ast, Function* fcn, Value* args, uint32_t argCount, Value* out);



    namespace Abi {

        namespace Win64 {
            void call(void* addr, uint64_t* args, uint32_t count);
        }

        namespace SysV {
            void call(void* addr, uint64_t* args, uint32_t count);
        }

    }

}
