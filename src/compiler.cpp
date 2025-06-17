// #pragma once
#include <cstdint>

#include "compiler.h"
#include "parser.h"
#include "validator.h"
#include "logger.h"
#include "syntax.h"
#include "error.h"
#include "utils.h"

#include "file_driver.h"

#include "itself_console_translator.h"
#include "c_translator.h"


#include "../lib/libtcc.h"

#include <map>
#include <string>
#include <filesystem>

#define NULL 0

inline void runTranslator(Translator* t) {
    t->debugInfo = Compiler::debugInfo;
    t->init(Compiler::outDir);
    t->printNode(t->mainFile, 0, SyntaxNode::root, NULL);
    t->printForeignCode();
    t->exit();
}



char* Compiler::mainFile = NULL;
char* Compiler::outFile = NULL;
char* Compiler::outDir = (char*) "./out";

int Compiler::command = TRANSLATE;
int Compiler::outLangs = 0;
int Compiler::debugInfo = 0;



struct ForeignLangsData {
    LangDef* data;
    FILE* file;    
};
std::map<std::string, ForeignLangsData*> foreignLangsMap;



template <typename T>
int printForeignCode(std::vector<T*> codeBlocks, std::vector<LangDef*>, char* outDir);
int printForeignFunction(std::vector<ForeignFunction*> foreignFcn, std::vector<LangDef*> langDefs, char* outDir);

int compileForeignCode(char* path);

int build();



int Compiler::compile() {

    Logger::log(Logger::INFO, "Compilation started...\n");

    int err;

    err = Parser::parse(mainFile);
    if (err < 0) return err;
    
    Logger::log(Logger::INFO, "Parsing completed...\n");

    err = Validator::validate();
    if (err < 0) return err;

    Logger::log(Logger::INFO, "Validating completed...\n");

    // if (printForeignCode(SyntaxNode::codeBlocks, SyntaxNode::langDefs, Compiler::outDir)) return -1;
    // if (printForeignFunction(SyntaxNode::foreignFunctions, SyntaxNode::langDefs, Compiler::outDir)) return -1;
    // if (compileForeignCode(Compiler::outDir)) return -1;
    
    // Logger::log(Logger::INFO, "Foreign block of codes assembled...\n");

    // TODO : make parallel
    if (outLangs & ITSELF_CONSOLE_LANG) runTranslator(&translatorItselfConsole);
    if (outLangs & C_LANG) runTranslator(&translatorC);
    
    Logger::log(Logger::INFO, "Translation completed...\n");
    if (Compiler::command == TRANSLATE) return 0;

    err = build();
    if (err < 0) return err;

    Logger::log(Logger::INFO, "Binary generation completed...\n");

    return Err::OK;

}

int build() {
    
    std::filesystem::path exePath = Utils::getExePath();
    
    std::string libPath = (exePath / "../tcc/lib").string();
    
    std::string tccIncPath[] = {
        #ifdef _WIN32
            (exePath / "../tcc/inc").string(),
            (exePath / "../tcc/inc/winapi").string()
        #else
            "/usr/include"
        #endif
    };
    const int tccIncPathLen = sizeof(tccIncPath) / sizeof(std::string);

    std::string resIncPath = (exePath / "../resources").string();

    TCCState *state = tcc_new();
    if (!state) {
        Logger::log(Logger::ERROR, "TCC: Failed to create TCC state.\n");
        return Err::TCC_ERROR;
    }

    #ifdef __unix__
        // tcc_set_options(state, "-nostdinc");
    #endif

    if (Compiler::debugInfo) {
        tcc_set_options(state, "-ggdb");
    }

    if (tcc_set_output_type(state, TCC_OUTPUT_EXE) < 0) {
        Logger::log(Logger::ERROR, "TCC: Failed to set output type.\n");
        tcc_delete(state);
        return Err::TCC_ERROR;
    }
    
    if (tcc_add_library_path(state, libPath.c_str()) < 0) {
        Logger::log(Logger::ERROR, "TCC: Failed to add library path.\n");
        tcc_delete(state);
        return Err::TCC_ERROR;
    }

    for (int i = 0; i < tccIncPathLen; i++) {
        if (tcc_add_include_path(state, tccIncPath[i].c_str()) < 0) {
            Logger::log(Logger::ERROR, "TCC: Failed to add include path.\n");
            tcc_delete(state);
            return Err::TCC_ERROR;
        }
    }
    
    if (
        tcc_add_include_path(state, resIncPath.c_str()) < 0 ||
        tcc_add_include_path(state, ".") < 0
    ) {
        Logger::log(Logger::ERROR, "TCC: Failed to add include path.\n");
        tcc_delete(state);
        return Err::TCC_ERROR;
    }

    #ifdef _WIN32
    if (
        tcc_add_library(state, "gdi32") < 0 ||
        tcc_add_library(state, "kernel32") < 0 ||
        tcc_add_library(state, "user32") < 0 ||
        tcc_add_library(state, "msvcrt") < 0 ||
        tcc_add_library(state, "tcc1-64") < 0
    ) {
        Logger::log(Logger::ERROR, "TCC: Failed to add required libraries.\n");
        tcc_delete(state);
        return Err::TCC_ERROR;
    }
    #else
    
    if (tcc_add_include_path(state, "/usr/lib/gcc/x86_64-linux-gnu/11/include") < 0) {
        Logger::log(Logger::ERROR, "TCC: Failed to add library path.\n");
        tcc_delete(state);
        return Err::TCC_ERROR;
    }

    if (tcc_add_library_path(state, "/usr/lib") < 0) {
        Logger::log(Logger::ERROR, "TCC: Failed to add library path.\n");
        tcc_delete(state);
        return Err::TCC_ERROR;
    }

    if (
        tcc_add_library(state, "m") < 0 ||        // Math library
        tcc_add_library(state, "c") < 0 ||        // Standard C library
        tcc_add_library(state, "dl") < 0 ||       // Dynamic linking library
        tcc_add_library(state, "pthread") < 0 || // POSIX threads library
        tcc_add_library(state, "tcc1-64") < 0
    ) {
        Logger::log(Logger::ERROR, "TCC: Failed to add required libraries.\n");
        tcc_delete(state);
        return Err::TCC_ERROR;
    }
    #endif

    if (
        tcc_add_file(state, "main.c") < 0
    ) {
        Logger::log(Logger::ERROR, "TCC: Failed to add .c files.\n");
        tcc_delete(state);
        return Err::TCC_ERROR;
    }

    if (tcc_output_file(state, Compiler::outFile) < 0) {
        Logger::log(Logger::ERROR, "TCC: Failed to generate output executable.\n");
        tcc_delete(state);
        return Err::TCC_ERROR;
    }
    
    tcc_delete(state);
    
    return Err::OK;

}
