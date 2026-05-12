#include "file_system.h"
#include "file_driver.h"
#include "array_list.h"
#include "allocator.h"
#include "logger.h"
#include "set.h"
#include "config.h"

#include <cstddef>
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



    struct File {
        String   data;
        FileInfo info;
        Origin   origin;
        int      idx; // idx in filesData
        bool     isOpened;
    };

    DArray::Container filesData;
    Set::Container    filesSet;



    Path* makePath(Path* src) {

        Path* path = (Path*) alloc(alc, sizeof(Path));

        path->bufferLen = src->bufferLen;
        memcpy(path->buffer, src->buffer, src->bufferLen);
        path->buffer[path->bufferLen] = '\0';

        annotatePath(path);

        return path;

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

    FileInfo* getFileInfo(Handle fhnd) {
        File* file = (File*) fhnd;
        return &file->info;
    }

    void init() {
        DArray::init(&filesData, Config::fileCount, sizeof(File));
        Set::init(&filesSet, Config::fileCount * 2);
        filesSet.hashMethod = Set::HM_STRING_FNV1A;
        filesSet.keyOffset = offsetof(File, info) +
                             offsetof(FileInfo, absPath) +
                             offsetof(String, buff);
    }

    void release() {
        DArray::release(&filesData);
        Set::release(&filesSet);
    }

    Handle load(Path* absPath, Origin origin) {

        {
            File* file = (File*) Set::find(&filesSet, (uint64_t) absPath->buffer);
            if (file) {
                file->isOpened = true;
                return file;
            }
        }

        char* buffer;
        const int bufferLen = FileDriver::readFile(absPath->buffer, &buffer);
        if (bufferLen < 0) return null;

        File file;
        file.isOpened = true;
        file.origin = origin;
        file.data.buff = buffer;
        file.data.len = bufferLen;
        file.info.modifiedTime = { 0, 0 };
        file.info.sizeBytes = 0;
        file.info.relativePath = NULL;
        file.info.userData = NULL;
        file.info.status = FileStatus::FS_DIRTY;

        file.info.absPath = makePath(file.info.absPath);
        annotatePath(file.info.absPath);

        toAbsolutePath(file.info.absPath);
        file.info.name = nameFromPath(file.info.absPath);

        file.idx = filesData.size;
        DArray::push(&filesData, &file);

        return DArray::get(&filesData, file.idx);

    }

    Handle load(String fname, Origin origin) {

        if (fname.len >= MAX_FILE_PATH) {
            return null;
        }

        Path* absPath = (Path*) alloc(alc, sizeof(Path));
        memcpy(absPath->buffer, fname.buff, fname.len);
        absPath->buffer[fname.len] = '\0';

        load(absPath, origin);

    }

    Handle load(String fpath, String fname, Origin origin) {

        if (fname.len + fpath.len + 1 >= MAX_FILE_PATH) {
            return null;
        }

        Path* absPath = (Path*) alloc(alc, sizeof(Path));

        memcpy(absPath->buffer, fpath.buff, fpath.len);
        absPath->buffer[fpath.len] = PATH_SEP;

        memcpy(absPath->buffer + fpath.len + 1, fname.buff, fname.len);
        absPath->buffer[fpath.len + fname.len + 1] = '\0';

        load(absPath, origin);

    }

    void unload(Handle fhnd, Origin origin) {
        File* file = (File*) fhnd;
        if (file->idx >= filesData.size) return;

        file->isOpened = false;
    }

    const char* getBuffer(Handle fhnd) {
        File* file = (File*) fhnd;
        if (!file->isOpened) return NULL;

        return file->data.buff;
    }

    String getDirectory(Handle fhnd) {
        File* file = (File*) fhnd;
        return { file->info.absPath->buffer, file->info.absPath->nameOff };
    }

    Handle getHandle(String absPath) {
        return (Handle) Set::find(&filesSet, (uint64_t) absPath.buff);
    }

    void* getUserData(Handle fhnd) {
        File* file = (File*) fhnd;
        return file->info.userData;
    }

    void setUserData(Handle fhnd, void *dataPtr) {
        File* file = (File*) fhnd;
        file->info.userData = dataPtr;
    }

    Path* computeRelativePath(Handle fhnd, String root) {

        std::filesystem::path tmpRoot(std::string(root.buff, root.len));
        std::filesystem::path tmpFile(std::string(root.buff, root.len));

        std::filesystem::path relative = std::filesystem::relative(tmpFile, tmpRoot);
        const size_t relativeSize = relative.string().size();
        if (relativeSize > MAX_FILE_PATH) {
            return NULL;
        }

        FileInfo* info = getFileInfo(fhnd);
        if (!info->relativePath) {
            info->relativePath = (Path*) alloc(alc, sizeof(Path));
        }
        memcpy(info->relativePath, relative.c_str(), relativeSize);
        annotatePath(info->relativePath);

        return info->relativePath;

    }

}
