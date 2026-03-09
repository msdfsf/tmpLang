#pragma once

#define AC_RESET            "\x1b[0m"

#define AC_BLACK            "\x1b[30m"
#define AC_RED              "\x1b[31m"
#define AC_GREEN            "\x1b[32m"
#define AC_YELLOW           "\x1b[33m"
#define AC_BLUE             "\x1b[34m"
#define AC_MAGENTA          "\x1b[35m"
#define AC_CYAN             "\x1b[36m"
#define AC_WHITE            "\x1b[37m"

#define AC_BRIGHT_BLACK     "\x1b[90m"
#define AC_BRIGHT_RED       "\x1b[91m"
#define AC_BRIGHT_GREEN     "\x1b[92m"
#define AC_BRIGHT_YELLOW    "\x1b[93m"
#define AC_BRIGHT_BLUE      "\x1b[94m"
#define AC_BRIGHT_MAGENTA   "\x1b[95m"
#define AC_BRIGHT_CYAN      "\x1b[96m"
#define AC_BRIGHT_WHITE     "\x1b[97m"

#define AC_BOLD_BLACK       "\x1b[1;30m"
#define AC_BOLD_RED         "\x1b[1;31m"
#define AC_BOLD_GREEN       "\x1b[1;32m"
#define AC_BOLD_YELLOW      "\x1b[1;33m"
#define AC_BOLD_BLUE        "\x1b[1;34m"
#define AC_BOLD_MAGENTA     "\x1b[1;35m"
#define AC_BOLD_CYAN        "\x1b[1;36m"
#define AC_BOLD_WHITE       "\x1b[1;37m"

#define AC_BOLD             "\x1b[1m"
#define AC_DIM              "\x1b[2m"
#define AC_ITALIC           "\x1b[3m"
#define AC_UNDERLINE        "\x1b[4m"
#define AC_BLINK            "\x1b[5m"
#define AC_INVERSE          "\x1b[7m"
#define AC_HIDDEN           "\x1b[8m"
#define AC_STRIKETHROUGH    "\x1b[9m"

#define AC_SECTION          AC_BOLD AC_BRIGHT_CYAN
#define AC_VAR              AC_BOLD AC_BRIGHT_GREEN
#define AC_TYPE             AC_BRIGHT_YELLOW
#define AC_NUMBER           AC_BRIGHT_BLACK
#define AC_ADDRESS          AC_DIM AC_BRIGHT_BLACK
#define AC_SEPARATOR        AC_DIM

#define AC_ERROR    "\033[1;31m"
#define AC_WARNING  "\033[1;33m"
