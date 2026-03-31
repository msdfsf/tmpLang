#pragma once

#include "string.h"
#include <cstdint>



// This serves as a compile-time interface for file handling,
// as different might need to represent files differently.
namespace FileSystem {

    constexpr size_t MAX_FILE_PATH = 4096;

    typedef void* Handle;
    constexpr Handle null = NULL;

    struct Timestamp {
        uint64_t low;
        uint64_t high;
    };

    enum PathFlag : uint8_t {
        PF_ABSOLUTE      = 1 << 0, // Path starts from a root (e.g., "/" or "C:\")
        PF_NORMALIZED    = 1 << 1, // Path has been normalized (no '.', '..', or double slashes)
        PF_HAS_EXT       = 1 << 2, // Path includes a file extension
        PF_HAS_NAME      = 1 << 3, // Path includes a valid name component
        PF_IS_DIR        = 1 << 4, // Path refers to a directory (known or forced)
        PF_IS_FILE       = 1 << 5, // Path refers to a regular file
    };

    struct Path {
        char buffer[MAX_FILE_PATH];
        uint16_t bufferLen;
        uint16_t nameOff;
        uint16_t extensionOff;
        uint8_t flags;
    };

    struct FileInfo {
        String name;
        Path* absPath;
        Path* relativePath; // has to be computed
        uint64_t sizeBytes;
        Timestamp modifiedTime;
        void* userData;
    };

    // We need capability to know form which source was
    // file loading requested.
    typedef uint32_t Origin;

    // Reserved ranges for the Origin ID
    namespace Origins {
        // Reserved for Compiler (0 - 10)
        constexpr Origin COMPILER_ARBITRARY = 0;
        constexpr Origin COMPILER_SOURCE    = 1;
        constexpr Origin COMPILER_ASSET     = 2;

        // Start of user specific IDs (11+)
        // The LSP or any other tool should define its own IDs
        // starting from here.
        constexpr Origin USER_START       = 11;
    }

    // init/release the FileSystem itself
    void init   ();
    void release();

    // load/unload file from the FileSystem
    Handle load(String fname, Origin origin);
    Handle load(String fpath, String fname, Origin origin);

    void   unload(Handle file, Origin origin);

    const char* getBuffer   (Handle file);
    FileInfo*   getFileInfo (Handle flhnd);
    String      getDirectory(Handle file);
    Handle      getHandle   (String absPath);

    void* getUserData(Handle flhnd);
    void  setUserData(Handle flhnd, void* dataPtr);

    // Path procedures
    Path* initPath      (String str);
    void  releasePath   (Path* path);
    void  annotatePath  (Path* path);
    bool  toAbsolutePath(Path* path);
    void  toParentPath  (Path* path);
    void  uriToPath     (String uri, Path* out);
    Path* computeRelativePath(Handle flhnd, String str);

    String catPaths(Path* base, const char* const appendix);

    Path* getExePath();

}
