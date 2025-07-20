#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "vm.h"

static void repl() {
    char line[1024];
    for(;;) {
        printf("> ");
        if(!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }

        interpret(line);
    }
}

static char* readFile(const char* path) {
    FILE* file = fopen(path, "rb");

    if(file == NULL) {
        fprintf(stderr, "Could not open the file \"%s\".\n", path);
        exit(74);
    }

    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);

    char* buffer = (char*)malloc(fileSize + 1);

    if(buffer == NULL) {
        fprintf(stderr, "Low memory. Cannot read \"%s\".\n", path);
        exit(74);
    }

    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);

    if(bytesRead < fileSize) {
        fprintf(stderr, "Could not read the file \"%s\".\n");
        exit(74);
    }

    buffer[bytesRead] = '\0';

    fclose(file);
    return buffer;
}

static void runFile(const char* path) {
    char* source = readFile(path);
    InterpretResult res = interpret(source);
    free(source);

    if(res == INTERPRET_COMPILE_ERR) exit(65);
    if(res == INTERPRET_RUNTIME_ERR) exit(70);
} 

int main(int argc, const char* argv[]) {
    initVM();

    if(argc == 1) {
        repl();
    } else if(argc == 2) {
        runFile(argv[1]);
    } else {
        fprintf(stderr, "Usage: cltl file_path \n");
        exit(64);
    }

    freeVM();
    return 0;
}