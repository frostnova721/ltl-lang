#ifndef cltl_vm_h
#define cltl_vm_h

#include "chunk.h"
#include "value.h"
#include "table.h"
#include "object.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    ObjClosure* closure;
    uint8_t* ip;
    Value* slots;
} CallFrame;

typedef struct
{
    CallFrame callFrames[FRAMES_MAX];
    int frameCount;
    
    Value stack[STACK_MAX];
    Value* stackTop;

    Table strings;
    Table globals;

    ObjUpvalue* openUpvalues;
    ObjString* initString;
    Obj* objects;

    int grayCount;
    int grayCapacity;
    Obj** grayStack;

    size_t allocatedBytes;
    size_t nextGc;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERR,
    INTERPRET_RUNTIME_ERR,
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();

InterpretResult interpret(const char* source);

// Stack operations
void push(Value value);
Value pop();

#endif