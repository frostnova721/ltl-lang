#include<stdio.h>
#include<string.h>

#include "value.h"
#include "vm.h"
#include "memory.h"
#include "table.h"
#include "object.h"

#define ALLOCATE_OBJ(type, objType) \
    (type*)allocateObject(sizeof(type), objType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* obj = (Obj*)reallocate(NULL, 0, size);
    obj->type = type;
    obj->next = vm.objects;
    vm.objects = obj;

    #ifdef DEBUG_LOG_GC
        printf("%p allocate %ld for %d\n", (void*)obj, size, type);
    #endif

    return obj;
}

static ObjString* allocateString(char* chars, int length, uint32_t hashCode) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STR);
    string->chars = chars;
    string->hashCode = hashCode;
    string->length = length;

    push(OBJ_VAL(string));
    tableSet(&vm.strings, string, NULL_VAL);
    pop();

    return string;
}

uint32_t hashString(const char* key, int length) {
    uint32_t hash =  2166136261u;

    for(int i=0; i<length; i++) {
        hash ^= key[i];
        hash *= 16777619;
    }

    return hash;
}

ObjString* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);

    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);

    if(interned != NULL) return interned;

    char* heapChars = ALLOCATE(char, length+1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

static void printFunction(ObjFun* fun) {
    if(fun->name == NULL) {
        printf("<script>");
        return;
    }
    printf("<fn %s>", fun->name->chars);
}

ObjString* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);

    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);

    if(interned != NULL) {
        FREE_ARRAY(char, chars, length +1);
        return interned;
    }

    return allocateString(chars, length, hash);
}

ObjFun* newFunction() {
    ObjFun* fun = ALLOCATE_OBJ(ObjFun, OBJ_FUN);
    fun->arity = 0;
    fun->name = NULL;
    fun->upvalueCount = 0;
    initChunk(&fun->chunk);
    return fun;
}

ObjNative* newNative(NativeFun fun) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->fun = fun;
    return native;
}

ObjClosure* newClosure(ObjFun* fun) {
    ObjUpvalue** upval = ALLOCATE(ObjUpvalue*, fun->upvalueCount);
    for(int i =0; i<fun->upvalueCount; i++) {
        upval[i] = NULL;
    }
    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->fun = fun;
    closure->upvalues = upval;
    closure->upvalueCount = fun->upvalueCount;
    return closure;
}

ObjUpvalue* newUpvalue(Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->next = NULL;
    upvalue->closed = NULL_VAL;
    return upvalue;
}

ObjClass* newClass(ObjString* name) {
    ObjClass* klazz = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    klazz->name = name;
    initTable(&klazz->methods);
    return klazz;
}

ObjInstance* newInstance(ObjClass* klass) {
    ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->klass = klass;
    initTable(&instance->fields);
    return instance;
}

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method) {
    ObjBoundMethod* newBound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
    newBound->method = method;
    newBound->reciever = receiver;
    return newBound;
}

void printObj(Value val) {
    switch (OBJ_TYPE(val))
    {
    case OBJ_STR:
        printf("%s", AS_CSTRING(val));
        break;
    case OBJ_FUN:
        printFunction(AS_FUNCTION(val));
        break;
    case OBJ_NATIVE: 
        printf("<native fn>"); break;
    case OBJ_CLOSURE:
        printFunction(AS_CLOSURE(val)->fun);
        break;
    case OBJ_UPVALUE:
        printf("upvalue");
        break;
    case OBJ_CLASS:
        printf("%s", AS_CLASS(val)->name->chars); 
        break;
    case OBJ_INSTANCE:
        printf("%s instance", AS_INSTANCE(val)->klass->name->chars);
        break;
    case OBJ_BOUND_METHOD:
        ObjClosure* method = (ObjClosure*)AS_BOUND_METHOD(val)->method;
        printFunction(method->fun);
        break;
    
    default:
        printf("Unknown object type");
        break;
    }
}