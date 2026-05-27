#include "array_list.h"
#include "data_types.h"
#include "diagnostic.h"
#include "dynamic_arena.h"
#include "file_driver.h"
#include "file_system.h"
#include "globals.h"
#include "registry.h"
#include "validator.h"
#include "foreign_code.h"

#include <cstdint>

#ifdef _WIN32
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
#endif



// TODO : linux version
namespace Extern {

    // TODO : be able to use allocator here instead of arena
    //        for now not universal...
    String utf8ToWChar(Arena::Container* arena, String str) {
        int wcharCount = MultiByteToWideChar(
            CP_UTF8,
            0,
            str.buff,
            str.len,
            NULL,
            0
        );

        if (wcharCount <= 0) {
            return String { NULL, 0 };
        }

        wchar_t* result = (wchar_t*) Arena::push(
            arena,
            wcharCount * sizeof(wchar_t),
            sizeof(wchar_t)
        );

        int converted = MultiByteToWideChar(
            CP_UTF8,
            0,
            str.buff,
            str.len,
            result,
            wcharCount
        );

        return converted == 0 ?
            String { NULL, 0 } :
            String { (char*) result, wcharCount * sizeof(wchar_t) };
    }

    String wcharToUtf8(Arena::Container* arena, String str) {
        int utf8Size = WideCharToMultiByte(
            CP_UTF8,
            0,
            (wchar_t*) str.buff,
            str.len,
            NULL,
            0,
            NULL,
            NULL
        );

        if (utf8Size <= 0) {
            return String { NULL, 0 };
        }

        char* result = (char*) Arena::push(
            arena,
            utf8Size,
            1
        );

        int converted = WideCharToMultiByte(
            CP_UTF8,
            0,
            (wchar_t*) str.buff,
            str.len,
            result,
            utf8Size,
            NULL,
            NULL
        );

        return converted == 0 ?
            String { NULL, 0 } :
            String { result, (uint64_t) utf8Size };
    }

    char* toCString(Arena::Container* arena, String str) {
        char* cstr = (char*) Arena::push(arena, str.len + 1);

        memcpy(cstr, str.buff, str.len);
        cstr[str.len] = '\0';

        return cstr;
    }




    std::atomic<TaskStatus> gLibSetLock;
    Set::Container gLibSet;

    // TODO : to a context or something
    thread_local char osErrBuff[512];

    struct LibraryId {
        uint32_t volume;
        uint32_t indexH;
        uint32_t indexL;
    };

    struct LibrarySlot {
        LibraryId id;
        Library lib;
    };
    
    void lockLibSet() {
        while (true) {
            TaskStatus expected = TS_PENDING;

            if (gLibSetLock.compare_exchange_strong(expected, TS_RUNNING, std::memory_order_acquire)) {
                return;
            }

            if (expected == TS_RUNNING) {
                gLibSetLock.wait(TS_RUNNING, std::memory_order_relaxed);
            }
        }
    }

    void unlockLibSet() {
        gLibSetLock.store(TS_PENDING, std::memory_order_release);
        gLibSetLock.notify_all();
    }



    void init() {
        gLibSet.hashMethod = Set::HM_12_FNV1A;
        gLibSet.keyOffset = getMemberOffset(LibrarySlot, id);
        Set::init(&gLibSet, 512);
    }



    // Windows
    //

    void formatOsError(int code, char* buff, const int buffSize) {
        FormatMessageA(
            FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
            NULL,
            code,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            buff,
            buffSize,
            NULL
        );
    }

    Err::Err loadLibrary(Validator::ValidationContext* ctx, String name, LibraryLoadLevel level, LibraryHandle* out) {
        HMODULE hnd = NULL;
        
        String wname = utf8ToWChar(&ctx->stringArena, name);

        if (level == LL_INSPECT) {
            hnd = LoadLibraryExW((wchar_t*) wname.buff, NULL,
                LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE | DONT_RESOLVE_DLL_REFERENCES);
        } else if (level == LL_EXECUTE) {
            hnd = LoadLibraryW((wchar_t*) wname.buff);
        } else {
            // TODO :
            return Err::UNEXPECTED_ERROR;
        }

        Arena::rollback(&ctx->stringArena, wname.buff);
        
        if (!hnd) {
            formatOsError(GetLastError(), osErrBuff, sizeof(osErrBuff));
            
            Diag::report(ctx->unit->ast, NULL, Err::LIBRARY_LOAD_FAILED,
                Diag::Format {
                    "Failed to map native library into compiler memory.\n"
                    "  Target: %*.s\n"
                    "  System Error: %s"
                },
                name.len, name.buff, osErrBuff
            );

            return Err::LIBRARY_LOAD_FAILED;
        }
        
        wchar_t* fullPath = (wchar_t*) Arena::push(&ctx->stringArena, FileSystem::MAX_FILE_PATH);
        DWORD resultSize = GetModuleFileNameW(hnd, fullPath, FileSystem::MAX_FILE_PATH);
        DWORD errCode = GetLastError();
        if (resultSize == 0 || errCode == ERROR_INSUFFICIENT_BUFFER) {
            FreeLibrary(hnd);
            
            formatOsError(errCode, osErrBuff, sizeof(osErrBuff));
            
            Diag::report(ctx->unit->ast, NULL, Err::LIBRARY_LOAD_FAILED,
                Diag::Format{
                    "Failed to resolve full library path.\n"
                    "  Target: %*.s\n"
                    "  System Error: %s"
                },
                name.len, name.buff, osErrBuff
            );

            Arena::rollback(&ctx->stringArena, fullPath);
            
            return Err::UNEXPECTED_ERROR;
        }

        HANDLE hFile = CreateFileW(
            fullPath,
            0,
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
            NULL,
            OPEN_EXISTING,
            0,
            NULL
        );

        if (hFile == INVALID_HANDLE_VALUE) {
            formatOsError(GetLastError(), osErrBuff, sizeof(osErrBuff));
            
            FreeLibrary(hnd);

            Diag::report(ctx->unit->ast, NULL, Err::UNEXPECTED_ERROR,
                Diag::Format{
                    "Internal Error: Unable to open native library for identity verification.\n"
                    "  Resolved Path: %ls\n"
                    "  System Error: %s"
                },
                fullPath, osErrBuff
            );

            Arena::rollback(&ctx->stringArena, fullPath);

            return Err::UNEXPECTED_ERROR;
        }
        
        LibraryId libId = { 0 };

        BY_HANDLE_FILE_INFORMATION info;
        if (!GetFileInformationByHandle(hFile, &info)) {
            formatOsError(GetLastError(), osErrBuff, sizeof(osErrBuff));

            CloseHandle(hFile);
            FreeLibrary((HMODULE) hnd);
            
            Diag::report(ctx->unit->ast, NULL, Err::UNEXPECTED_ERROR,
                Diag::Format{
                    "Internal Error: Failed to retrieve unique identity for native library.\n"
                    "  The compiler cannot verify if this file is already loaded.\n"
                    "  Path: %ls\n"
                    "  System Error: %s"
                },
                fullPath, osErrBuff
            );

            Arena::rollback(&ctx->stringArena, fullPath);

            return Err::UNEXPECTED_ERROR;
        }

        libId.volume = info.dwVolumeSerialNumber;
        libId.indexH = info.nFileIndexHigh;
        libId.indexL = info.nFileIndexLow;

        CloseHandle(hFile);

        // TODO : abstract to a function as we dont need to rly on os here I guess
        lockLibSet();

        LibrarySlot* libSlot = (LibrarySlot*) Set::find(&gLibSet, (uint64_t) &libId);
        
        if (!libSlot) {
            libSlot = (LibrarySlot*) alloc(alc, sizeof(LibrarySlot));
            libSlot->id = libId;
            libSlot->lib.dllPath = wcharToUtf8(alc, String { (char*) fullPath, resultSize });
            libSlot->lib.osHandle = (void*) hnd;
            libSlot->lib.loadLevel = level;

            Set::insert(&gLibSet, (uint8_t*) libSlot);
            *out = (LibraryHandle) &libSlot->lib;
        } else {
            if (libSlot->lib.loadLevel == LL_INSPECT && level == LL_EXECUTE) {
                // 'Upgrade' from INSPECT to EXECUTE
                FreeLibrary((HMODULE) libSlot->lib.osHandle);
                libSlot->lib.osHandle = LoadLibraryW(fullPath);
                libSlot->lib.loadLevel = LL_EXECUTE;

                if (!libSlot->lib.osHandle) {
                    unlockLibSet();
                    formatOsError(GetLastError(), osErrBuff, sizeof(osErrBuff));

                    Arena::rollback(&ctx->stringArena, wname.buff);

                    Diag::report(ctx->unit->ast, NULL, Err::LIBRARY_LOAD_FAILED,
                        Diag::Format{
                            "Failed to initialize native library for compile-time execution.\n"
                            "  The library was previously verified but could not be loaded as code.\n"
                            "  Path: %.*s\n"
                            "  System Error: %s\n"
                        },
                        libSlot->lib.dllPath.len, libSlot->lib.dllPath.buff, osErrBuff
                    );

                    return Err::LIBRARY_LOAD_FAILED;
                }
            }

            if (hnd != (HMODULE) libSlot->lib.osHandle) {
                FreeLibrary(hnd);
            }
            *out = (LibraryHandle) &libSlot->lib;
        }

        unlockLibSet();

        Arena::rollback(&ctx->stringArena, fullPath);

        return Err::OK;
    }

    Err::Err ensureFunctionExists(Validator::ValidationContext* ctx, LibraryHandle hLib, Function* fcn) {
        Library* lib = hLib;

        char* cstr = toCString(&ctx->stringArena, String { fcn->name.buff, fcn->name.len });
        FARPROC addr = GetProcAddress((HMODULE) lib->osHandle, cstr);
        if (!addr) {
            formatOsError(GetLastError(), osErrBuff, sizeof(osErrBuff));

            Diag::report(ctx->unit->ast, fcn->base.span, Err::SYMBOL_NOT_FOUND,
                Diag::Format{
                    "Failed to bind native symbol '%.*s'.\n"
                    "  The symbol '%s' was not found in the loaded library.\n"
                    "  Library: %.*s\n"
                    "  System Error: %s\n"
                },
                fcn->name.len, fcn->name.buff,
                cstr,
                lib->dllPath.len, lib->dllPath.buff,
                osErrBuff
            );

            return Err::SYMBOL_NOT_FOUND;
        }

        Arena::rollback(&ctx->stringArena, cstr);
    
        return Err::OK;
    }

    Err::Err bindFunction(Validator::ValidationContext* ctx, Library* lib, Function* fcn) {
        if (!lib->osHandle || lib->loadLevel != LL_EXECUTE) {
            LibraryHandle hDummy;
            Err::Err err = loadLibrary(ctx, lib->name, LL_EXECUTE, &hDummy);
            if (err != Err::OK) return err;
        }

        char stackBuffer[256];
        char* cstr = stackBuffer;
        if (fcn->name.len >= 256) {
            cstr = (char*) Arena::push(&ctx->stringArena, fcn->name.len + 1);
        }
        memcpy(cstr, fcn->name.buff, fcn->name.len);
        cstr[fcn->name.len] = '\0';

        void* addr = NULL;
        addr = (void*) GetProcAddress((HMODULE) lib->osHandle, cstr);

        if (!addr) {
            formatOsError(GetLastError(), osErrBuff, sizeof(osErrBuff));

            Diag::report(ctx->unit->ast, fcn->base.span, Err::SYMBOL_NOT_FOUND,
                Diag::Format{
                    "Failed to bind native function '%.*s'.\n"
                    "  The symbol '%s' was not found in the library.\n"
                    "  Library Path: %.*s\n"
                    "  System Error: %s\n"
                },
                fcn->name.len, fcn->name.buff, cstr,
                lib->dllPath.len, lib->dllPath.buff, osErrBuff
            );

            if (cstr != stackBuffer) free(cstr);
            return Err::SYMBOL_NOT_FOUND;
        }

        fcn->externAddress = addr;
        fcn->base.cmpStatus = TS_READY;

        if (cstr != stackBuffer) free(cstr);
        return Err::OK;
    }

    Err::Err invoke(AstContext* ast, Function* fcn, Value* args, uint32_t argCount, Value* out) {
        return Err::OK;
    }

}
