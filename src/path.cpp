#include "file_system.h"
#include <filesystem>

namespace FileSystem {

    // NOTE : assuming normalization to '/'

    void annotatePath(Path* path) {
        path->nameOff = 0;
        path->extensionOff = path->bufferLen;

        for (int i = path->bufferLen - 1; i >= 0; i--) {
            if (path->buffer[i] == '/') {
                path->nameOff = (uint16_t) (i + 1);
                break;
            }
        }

        for (int i = path->bufferLen - 1; i >= path->nameOff; i--) {
            if (path->buffer[i] == '.') {
                path->extensionOff = (uint16_t) i;
                path->flags |= PF_HAS_EXT;
                break;
            }
        }

        if (path->nameOff < path->bufferLen) {
            path->flags |= PF_HAS_NAME;
        }
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

    bool toAbsolutePath(Path* path) {

        std::filesystem::path tmp = std::filesystem::absolute(path->buffer);
        std::u8string str = tmp.u8string();

        std::string utf8(str.begin(), str.end());

        if (str.size() > MAX_FILE_PATH - 1) {
            return false;
        }

        memcpy(path->buffer, utf8.c_str(), str.size() + 1);
        path->bufferLen = (uint16_t) str.size();

        return true;

    }

    static int hexVal(char c) {

        if (c >= '0' && c <= '9') return c - '0';
        if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
        if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
        return -1;

    }

    void uriToPath(String uri, Path* out) {
        const char prefix[] = "file:/";
        const size_t prefixLen = sizeof(prefix) - 1;

        size_t idx = 0;
        if (uri.len >= prefixLen &&
            strncmp(uri.buff, prefix, prefixLen) == 0
        ) {
            idx = prefixLen;
            for (; idx < prefixLen + 2; idx++) {
                if (uri.buff[idx] != '/') break;
            }
        }

        uint16_t outIdx = 0;
        while (idx < uri.len && outIdx < MAX_FILE_PATH - 1) {
            if (uri.buff[idx] == '%' && idx + 2 < uri.len) {
                int high = hexVal(uri.buff[idx + 1]);
                int low  = hexVal(uri.buff[idx + 2]);

                if (high >= 0 && low >= 0) {
                    out->buffer[outIdx] = (char) ((high << 4) | low);
                    idx += 3;
                    outIdx++;
                    continue;
                }
            }

            char ch = uri.buff[idx++];
            // Force consistent slashes for the set key
            if (ch == '\\') ch = '/';
            out->buffer[outIdx] = ch;
            outIdx++;
        }

        // Null-terminated so we are compatible with C functions
        // and can be used without any additional info to compute hash
        out->buffer[outIdx] = '\0';
        out->bufferLen = outIdx;
        out->flags = PF_NORMALIZED;

        // Drive letter normalization (Windows)
        #ifdef _WIN32
        if (out->bufferLen > 1 && out->buffer[1] == ':') {
            out->buffer[0] = (char) std::tolower(out->buffer[0]);
            out->flags |= PF_ABSOLUTE;
        }
        #endif

        annotatePath(out);
    }

    int computeRelativePath(Path* abs, String root, Path* out) {
        std::filesystem::path tmpRoot(std::string(root.buff, root.len));
        std::filesystem::path tmpFile(std::string(abs->buffer, abs->bufferLen));

        std::filesystem::path relative = std::filesystem::relative(tmpFile, tmpRoot);
        out->bufferLen = relative.string().size();
        if (out->bufferLen > MAX_FILE_PATH) {
            return -1;
        }

        memcpy(out->buffer, relative.c_str(), out->bufferLen);
        annotatePath(out);

        return out->bufferLen;
    }

}
