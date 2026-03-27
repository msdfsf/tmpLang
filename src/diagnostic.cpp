#include "diagnostic.h"
#include "globals.h"
#include "logger.h"
#include "config.h"
#include "syntax.h"
#include <cstdarg>
#include <stdarg.h>
#include <stdio.h>



// #GiveMeBackDesignatedInitialization
const char* const Err::str(Err code) { switch (code) {

    case OK:
        return "OK";
    case MALLOC:
        return "Malloc doesn't feel good...";
    case UNEXPECTED_END_OF_FILE:
        return "Unexpected end of file!";
    case UNEXPECTED_END_OF_EXPRESSION:
        return "Unexpected end of expression!";
    case INVALID_OPERATOR:
        return "Invalid operator '%.*s'!";
    case VARIABLE_ALREADY_DEFINED:
        return "Variable '%.*s' already defined!";
    case FUNCTION_ALREADY_DEFINED:
        return "Function already defined!";
    case UNEXPECTED_SYMBOL:
        return "Unexpected symbol! %s expected.";
    case UNKNOWN_VARIABLE:
        return "Unknown variable '%.*s'!";
    case UNKNOWN_FUNCTION:
        return "Unknown function '%.*s'!";
    case INVALID_NUMBER_LITERAL:
        return "Invalid number literal!";
    case INVALID_TYPE_CONVERSION:
        return "Type conversion between '%s' and '%s' types is invalid!";
    case MISSING_VARIABLE_NAME:
        return "Variable name is missing!";
    case INVALID_TYPE_UNARY_OPERATOR:
        return "Invalid type '%s' to unary operator '%s'!";
    case INVALID_TYPE_BINARY_OPERATOR:
        return "Invalid type '%s' to binary operator '%s'!";
    case CANNOT_ASSIGN_TO_CONST:
        return "Cannot assign to constant lvalue!";
    case UNKNOWN_DATA_TYPE:
        return "Unknown data type!";
    case INVALID_DATA_TYPE:
        return "Invalid data type!";
    case COMPILE_TIME_KNOWN_EXPRESSION_REQUIRED:
        return "Compile time known expression required!";
    case TYPE_INIT_ATTRIBUTES_COUNT_MISMATCH:
        return "Attributes count mismatch! %i instead of %i given!";
    case INVALID_ATTRIBUTE_NAME:
        return "Invalid attribute name '%.*s'!";
    case OPERATOR_CANNOT_ACT_AS_UNARY:
        return "Operator '%.*s' cannot act as unary!";
    case OPERATOR_CANNOT_ACT_AS_BINARY:
        return "Operator '%.*s' cannot act as binary!";
    case CANNOT_GET_ADDRESS_OF_ADDRESS:
        return "Cannot get address of address!";
    case INVALID_USAGE_OF_OPERATOR:
        return "Invalid usage of operator '%.*s'!";
    case CANNOT_EVALUATE_EXPRESION_AT_CMP_TIME:
        return "Cannot evaluate expresion at compile time!";
    case INVALID_EMBED_ARRAY_SIZE:
        return "Embed array requires compile time size!";
    case UNKNOWN_QUALIFIER:
        return "Unknown qualifier! Valid are 'const' or 'embed'";
    case ARRAY_EXPECTED:
        return "Array expected!";
    case TOO_MANY_ARGUMENTS:
        return "Too many arguments!";
    case APPROPRIATE_TAG_DOES_NOT_EXISTS:
        return "Appropriate tag '%.*s' does not exists!";
    case BUFFER_OVERRUN:
        return "Buffer overrun!";
    case SYSTEM_COMMAND_EXECUTION_FAILED:
        return "System command execution failed!";
    case INVALID_LVALUE:
        return "Invalid lvalue!";
    case INVALID_VARIABLE_NAME:
        return "Invalid variable name! '%.*s'";
    case NOT_ENOUGH_ARGUMENTS:
        return "Not enough input arguments in function call!";
    case INVALID_DECLARATION_PLACE:
        return "%s is declared in invalid place!";
    case UNKNOWN_NAMESPACE:
        return "Unknown namespace!";
    case INVALID_NUMBER_OF_ATTRIBUTES:
        return "Invalid number of attributes.";
    case UNSUPPORTED_ESCAPE_SEQUENCE:
        return "Unsupported escape sequence!";
    case DATA_TYPE_SIZE_EXCEEDED:
        return "Data type size exceeded!";
    case UNEXPECTED_ERROR:
        return "Unexpected error!";
    case ARRAY_SIZE_MISMATCH:
        return "Array size mismatch!";
    case NO_MATCHING_FUNCTION_FOUND:
        return "No matching function found!";
    case MORE_THAN_ONE_OVERLOAD_MATCH:
        return "More than one overload variant match the function call!";
    case UNEXPECTED_RVALUE:
        return "Unexpected rvalue!";
    case UNKNOWN_ERROR_SET:
        return "Unknown error set!";
    case INVALID_RVALUE:
        return "Invalid rvalue!";
    case SYMBOL_ALREADY_DEFINED:
        return "Symbol already defined!";
    case TCC_ERROR:
        return "TCC error!";
    case UNTERMINATED_COMMENT:
        return "Unterminated comment!";
    case INVALID_ARRAY_LENGTH:
        return "Invalid array length!";
    case GLOBAL_SCOPE_REQUIRED:
        return "Declaration requires global scope!";
    case INVALID_DECLARATION_ORDER:
        return "Type definition '%.*s' includes type that is defined later! Move its definition before this one! Or may be you meant to use pointer?";
    case CIRCULAR_IMPORT:
        return "Circular import detected!";
    case FILE_DOES_NOT_EXISTS:
        return "File '%s' does not exists!";
    case UNKNOWN_DIRECTIVE:
        return "Unknown directive!";
    case INVALID_ARGUMENTS:
        return "Invalid arguments!";
    case NOT_YET_IMPLEMENTED:
        return "Not yet implemented!";
    case IO_ERROR:
        return "IO error!";
    case MAX_FILE_PATH_EXCEEDED:
        return "Max file path exceeded!";
    default:
        return "Unknown error!";

}}

const char* const Wrn::str(Wrn code) { switch (code) {

    case UNUSED_VARIABLE:
        return "Unused variable!";
    case SHADOWED_VARIABLE:
        return "Shadow variable";
    default:
        return "Unknown warning!";

}}

const char* const Inf::str(Inf code) { switch (code) {

    case TMP_INFO:
        return "tmp";
    default:
        return "Unknown info!";

}}

namespace Diag {

    Logger::Level toLoggerLevel(const Severity sev) {
        switch (sev) {
            SEV_NOTE:    return Logger::INFO;
            SEV_WARNING: return Logger::WARNING;
            SEV_FATAL:
            SEV_ERROR:   return Logger::ERROR;
        }
    }

    void report(AstContext* ctx, Span* span, Severity sev, uint32_t code, const char* const format, va_list args) {

        char buff[1024];

        const int len = vsnprintf(buff, sizeof(buff), format, args);
        if (len < 0) return;

        if constexpr (Config::LOGGING_ENABLED) {
            Logger::Level level = toLoggerLevel(sev);
            Logger::log({ .level = level, .tag = ctx->tag }, buff, span);
        }

        if constexpr (Config::ERROR_RECOVERY_ENABLED) {

            if (ctx->errorCount < Config::maxErrorCount) {

                AstError* err = &ctx->errors[ctx->errorCount];
                err->severity = sev;
                err->err = code;
                err->span = getSpanStamp(span);
                err->msg.buff = (char*) nalloc(nalc, AT_ERROR_STRING, len);
                err->msg.len = len;

                ctx->errorCount++;

            }

            ctx->totalErrorCount++;

        }

    }

    void report(AstContext* ctx, Span* span, Err::Err code, ...) {
        va_list args;
        va_start(args, code);
        report(ctx, span, SEV_ERROR, -code, va_arg(args, char*), args);
        va_end(args);
    }

    void report(AstContext* ctx, Span* span, Wrn::Wrn code, ...) {
        va_list args;
        va_start(args, code);
        report(ctx, span, SEV_WARNING, -code, va_arg(args, char*), args);
        va_end(args);
    }

    void report(AstContext* ctx, Span* span, Inf::Inf code, ...) {
        va_list args;
        va_start(args, code);
        report(ctx, span, SEV_NOTE, -code, va_arg(args, char*), args);
        va_end(args);
    }

    void report(AstContext* ctx, Span* span, Err::Err code, char* format, ...) {
        va_list args;
        va_start(args, code);
        report(ctx, span, SEV_ERROR, -code, args, args + 1);
        va_end(args);
    }

    void report(AstContext* ctx, Span* span, Wrn::Wrn code, char* format, ...) {
        va_list args;
        va_start(args, code);
        report(ctx, span, SEV_WARNING, -code, Wrn::str(code), args);
        va_end(args);
    }

    void report(AstContext* ctx, Span* span, Inf::Inf code, char* format, ...) {
        va_list args;
        va_start(args, code);
        report(ctx, span, SEV_NOTE, -code, Inf::str(code), args);
        va_end(args);
    }

}
