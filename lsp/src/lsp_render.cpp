#include "lsp_render.h"
#include "lsp.h"

#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstring>

constexpr int MAX_BUFFER_SIZE = 2048;

namespace Lsp::Render {

    struct Writer {
        String   buffer;
        uint32_t written;
    };

    inline void clamp(Writer* writer) {
        if (writer->written > writer->buffer.len) {
            writer->written = writer->buffer.len;
        }
    }

    void print(Writer* writer, String message) {
        writer->written += snprintf(
            writer->buffer.buff + writer->written,
            writer->buffer.len - writer->written,
            "%.*s", message.len, message.buff
        );
        clamp(writer);
    }

    void print(Writer* writer, const char* format, ...) {
        va_list args;
        va_start(args, format);
        
        writer->written += vsnprintf(
            writer->buffer.buff + writer->written,
            writer->buffer.len - writer->written,
            format, args
        );
        
        va_end(args);
        clamp(writer);
    }

    void printBold(Writer* writer, String text) {
        print(writer, "**%.*s**", text.len, text.buff);
    }

    void printSectionDelimiter(Writer* writer) {
        print(writer, "\n---\n");
    }

    String hover(SyntaxNode* node) {
        if (!node) return { nullptr, 0 };

        char buff[MAX_BUFFER_SIZE];
        Writer writer = { { buff, MAX_BUFFER_SIZE }, 0 };

        printBold(&writer, Ast::Node::str(node->type));
        printSectionDelimiter(&writer);

        switch (node->type) {
            case NT_FUNCTION: {
                print(&writer, "TODO");
                break;
            }

            case NT_VARIABLE_DEFINITION: {
                print(&writer, "TODO");
                break;
            }

            case NT_TYPE_DEFINITION: {
                print(&writer, "TODO");
                break;
            }

            case NT_ENUMERATOR: {
                print(&writer, "TODO");
                break;
            }

            case NT_NAMESPACE: {
                print(&writer, "TODO");
                break;

            }

            case NT_UNION: {
                print(&writer, "TODO");
                break;
            }

            default: {
                print(&writer, "TODO");
                break;
            }
        }

        String out;
        out.buff = alloc<char>(&State::allocator, writer.written);
        out.len = writer.written;

        memcpy(out.buff, writer.buffer.buff, out.len);

        return out;
    }

};
