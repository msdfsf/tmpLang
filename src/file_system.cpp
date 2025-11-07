#include "file_system.h"
#include "file_driver.h"
#include "array_list.h"
#include "allocator.h"
#include "logger.h"

#include <any>
#include <cstdint>
#include <cstring>
#include <filesystem>

#if _WIN32
#include <windows.h>
#undef ERROR
#endif



namespace FileSystem {

    constexpr int PATH_SEP = '\\';



    Logger::Type logErr = { .level = Logger::ERROR, .tag = stageTag };



    struct FileId {
        uint64_t size;
        std::filesystem::file_time_type time;
    };

    struct File {
        FileId id;
        String data;
    };

    struct Chunk {
        char empty;
        File file;
        FileInfo info;
    };

    DArray::Container files;


    inline int isPathDelimiter(char ch) {
        return (ch == '\\' || ch == '/');
    }

    void annotatePath(Path* path) {

        path->flags = 0;

        // Find name offset (after last '/')
        path->nameOff = 0;
        for (int i = path->bufferLen - 1; i >= 0; i--) {
            if (isPathDelimiter(path->buffer[i])) {
                path->nameOff = (uint16_t) (i + 1);
                if (i < path->bufferLen - 1) {
                    path->flags |= PF_HAS_NAME;
                }
                break;
            }
        }

        // Find extension offset (after last '.')
        path->extensionOff = path->bufferLen;
        for (int i = path->bufferLen - 1; i > path->nameOff; i--) {
            if (path->buffer[i] == '.') {
                path->extensionOff = (uint16_t) i;
                if (i < path->bufferLen - 1) {
                    path->flags |= PF_HAS_EXT;
                }
                break;
            }
        }

    }

    Path* initPath(String str) {

        if (str.len > MAX_FILE_PATH) {
            return NULL;
        }

        Path* path = (Path*) alloc(alc, sizeof(Path));

        path->bufferLen = str.len;
        memcpy(path->buffer, str.buff, str.len);
        path->buffer[path->bufferLen] = '\0';

        annotatePath(path);

        return path;

    }

    int validatePath(const Path* p) {

        // TODO
        return 0;

    }

    void toParentPath(Path* path) {

        for (int i = path->nameOff - 1; i >= 0; i--) {
            if (path->buffer[i] == '/') {
                path->bufferLen = i;
                path->nameOff = i;
                break;
            }
        }

        for (int i = path->bufferLen - 1; i >= 0; i--) {
            if (path->buffer[i] == '/') {
                path->nameOff = i + 1;
                break;
            }
        }

        path->extensionOff = path->bufferLen - 1;

    }

    int toAbsolutePath(Path* path) {

        std::filesystem::path tmp = std::filesystem::absolute(path->buffer);
        std::u8string str = tmp.u8string();

        std::string utf8(str.begin(), str.end());

        if (str.size() > MAX_FILE_PATH - 1) {
            return 0;
        }

        memcpy(path->buffer, utf8.c_str(), str.size() + 1);
        path->bufferLen = (uint16_t) str.size();

        annotatePath(path);

        return 1;

    }

    String nameFromPath(Path* path) {
        return String(
            path->buffer + path->nameOff,
            path->bufferLen - path->nameOff
        );
    }

    String catPaths(Path* base, const char* const appendix) {

        const int appendixLen = cstrlen(appendix);
        const int len = base->nameOff + appendixLen;
        char* const str = (char*) alloc(alc, len + 1);
        str[len] = '\0';

        memcpy(str, base->buffer, base->nameOff);
        memcpy(str + base->nameOff, appendix, appendixLen);

        return String(str, len);

    }

    String catPaths(Path* base, Path* appendix) {

        const int len = base->nameOff + appendix->bufferLen;
        char* const str = (char*) alloc(alc, len + 1);
        str[len] = '\0';

        memcpy(str, base->buffer, base->nameOff);
        memcpy(str + base->nameOff, appendix->buffer, appendix->bufferLen);

        return String(str, len);

    }

    Path* getExePath() {

        Path* path = (Path*) alloc(alc, sizeof(Path));
        path->bufferLen = MAX_FILE_PATH;

        #ifdef _WIN32
            DWORD result = GetModuleFileNameA(NULL, path->buffer, path->bufferLen);
            if (result > 0) {
                path->bufferLen = strlen(path->buffer);
                annotatePath(path);
                toParentPath(path);
            } else {
                Logger::log(logErr, "SYSTEM: Failed to get executable path! (GetModuleFileNameA failed)");
                return NULL;
            }
        #else
            ssize_t len = readlink("/proc/self/exe", path->buffer, path->bufferLen);
            if (len != -1) {
                path->bufferLen = strlen(path->buffer);
                annotatePath(path);
                toParentPath(path);
            } else {
                Logger::log(logErr, "Failed to get executable path! (readlink failed)");
                return NULL;
            }
        #endif

        return path;

    }



    // Expected to be called after loading the file
    // so no checks. We can check here, but it kinda has
    // no point as even after sucessful check file could
    // become invalid.
    FileId genId(const char* path) {

        FileId id;

        id.size = std::filesystem::file_size(path);
        id.time = std::filesystem::last_write_time(path);

        return id;

    }

    inline int cmpId(FileId idA, FileId idB) {
        return idA.size == idB.size && idA.time == idB.time;
    }

    inline Handle idx2hnd(int idx) {
        return (Handle) ((uint64_t) idx + 1);
    }

    inline int hnd2idx(Handle hnd) {
        return ((uint64_t) hnd) - 1;
    }

    Handle find(File* file, int* idx = NULL) {

        File* data = (File*) files.buffer;
        for (int i = 0; i < files.size; i++) {
            File* testFile = data + i;
            if (cmpId(testFile->id, file->id)) {
                if (idx) *idx = i;
                return file;
            }
        }

        if (idx) *idx = -1;
        return null;

    }

    FileInfo* getFileInfo(Handle flhnd) {

        Chunk* chunk = (Chunk*) DArray::get(&files, hnd2idx(flhnd));
        return &chunk->info;

    }

    void init() {
        DArray::init(&files, 32, sizeof(Chunk));
    }

    void release() {
        DArray::release(&files);
    }

    Handle load(Path* absPath) {

        char* buffer;
        char* fname = (char*) absPath->buffer;
        const int bufferLen = FileDriver::readFile(fname, &buffer);

        if (bufferLen < 0) {
            return null;
        }

        Chunk chunk;
        chunk.empty = 0;
        chunk.file.id = genId(fname);
        chunk.file.data.buff = buffer;
        chunk.file.data.len = bufferLen;
        chunk.info.modifiedTime = { 0, 0 };
        chunk.info.sizeBytes = 0;
        chunk.info.relativePath = NULL;
        chunk.info.userData = NULL;

        chunk.info.absPath = initPath(fname);
        annotatePath(chunk.info.absPath);

        toAbsolutePath(chunk.info.absPath);
        chunk.info.name = nameFromPath(chunk.info.absPath);

        int writeIdx = -1;

        Chunk* data = (Chunk*) files.buffer;
        for (int i = 0; i < files.size; i++) {

            Chunk* testChunk = data + i;
            if (testChunk->empty) {
                writeIdx = i;
                continue;
            }

            if (cmpId(testChunk->file.id, chunk.file.id)) {
                return idx2hnd(i);
            }

        }

        if (writeIdx > 0) {
            DArray::set(&files, writeIdx, &chunk);
            return idx2hnd(writeIdx);
        } else {
            DArray::push(&files, &chunk);
            return idx2hnd(files.size - 1);
        }

    }

    Handle load(String fname) {

        if (fname.len >= MAX_FILE_PATH) {
            return null;
        }

        Path* absPath = (Path*) alloc(alc, sizeof(Path));
        memcpy(absPath->buffer, fname.buff, fname.len);
        absPath->buffer[fname.len] = '\0';

        load(absPath);

    }

    Handle load(String fpath, String fname) {

        if (fname.len + fpath.len + 1 >= MAX_FILE_PATH) {
            return null;
        }

        Path* absPath = (Path*) alloc(alc, sizeof(Path));

        memcpy(absPath->buffer, fpath.buff, fpath.len);
        absPath->buffer[fpath.len] = PATH_SEP;

        memcpy(absPath->buffer + fpath.len + 1, fname.buff, fname.len);
        absPath->buffer[fpath.len + fname.len + 1] = '\0';

        load(absPath);

    }

    void unload(Handle flhnd) {

        int idx = hnd2idx(flhnd);
        if (idx >= files.size) return;

        Chunk* chunk = (Chunk*) files.buffer + idx;
        chunk->empty = 1;

    }

    const char* getBuffer(Handle flhnd) {

        int idx = hnd2idx(flhnd);
        if (idx >= files.size) return NULL;

        Chunk chunk;
        DArray::get(&files, idx, &chunk);
        if (chunk.empty) return NULL;

        return chunk.file.data.buff;

    }

    String getDirectory(Handle flhnd) {

        Chunk* chunk = ((Chunk*) files.buffer) + hnd2idx(flhnd);
        return { chunk->info.absPath->buffer, chunk->info.absPath->nameOff };

    }

    void* getUserData(Handle flhnd) {

        Chunk* chunk = ((Chunk*) files.buffer) + hnd2idx(flhnd);
        return chunk->info.userData;

    }

    void setUserData(Handle flhnd, void *dataPtr) {

        Chunk* chunk = ((Chunk*) files.buffer) + hnd2idx(flhnd);
        chunk->info.userData = dataPtr;

    }

    Path* computeRelativePath(Handle flhnd, String root) {

        std::filesystem::path tmpRoot(std::string(root.buff, root.len));
        std::filesystem::path tmpFile(std::string(root.buff, root.len));

        std::filesystem::path relative = std::filesystem::relative(tmpFile, tmpRoot);
        const size_t relativeSize = relative.string().size();
        if (relativeSize > MAX_FILE_PATH) {
            return NULL;
        }

        FileInfo* info = getFileInfo(flhnd);
        if (!info->relativePath) {
            info->relativePath = (Path*) alloc(alc, sizeof(Path));
        }
        memcpy(info->relativePath, relative.c_str(), relativeSize);
        annotatePath(info->relativePath);

        return info->relativePath;

    }

}
