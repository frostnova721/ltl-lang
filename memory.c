#include <stdlib.h>

#include "memory.h"
#include "vm.h"
#include "compiler.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    vm.allocatedBytes += newSize - oldSize;
    if(newSize > oldSize) {
        #ifdef DEBUG_STRESS_GC
            collectGarbage();
        #endif

        if(vm.allocatedBytes > vm.nextGc) {
            collectGarbage();
        }
    }
    
    if(newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if(result == NULL) exit(1);
    return result;
}

void freeObjects() {
    Obj* object = vm.objects;
    while(object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }
    free(vm.grayStack);
}

void freeObject(Obj* object) {
    #ifdef DEBUG_LOG_GC
        printf("%p free type %d\n", (void*)object, object->type);
    #endif
    switch(object->type) {
        case OBJ_STR: {
            ObjString* string = (ObjString*)object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
        case OBJ_FUN: {
            ObjFun* fun = (ObjFun*)object;
            freeChunk(&fun->chunk);
            FREE(ObjFun, object);
            break;
        }
        case OBJ_NATIVE: {
            FREE(ObjNative, object);
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_UPVALUE: {
            FREE(ObjUpvalue, object);
            break;
        }
        case OBJ_CLASS: {
            ObjClass* klazz = (ObjClass*)object;
            freeTable(&klazz->methods);
            FREE(ObjClass, object);
            break;
        }
        case OBJ_INSTANCE: {
            ObjInstance* insta = (ObjInstance*)object;
            freeTable(&insta->fields);
            FREE(ObjInstance, object);
            break;
        }
        case OBJ_BOUND_METHOD: {
            FREE(ObjBoundMethod, object);
            break;
        }
    }
}

void markObject(Obj* object) {
    if(object == NULL) return;
    if(object->isMarked) return;
    
#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    object->isMarked = true;

    if(vm.grayCapacity < vm.grayCount+1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        vm.grayStack = realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);
    }
    vm.grayStack[vm.grayCount++] = object;
    if(vm.grayStack == NULL) exit(1);
}

void markValue(Value value) {
    if(!IS_OBJ(value)) return;
    markObject(AS_OBJ(value));
}

static void markArray(ValueArray* arr) {
    for(int i=0; i<arr->count; i++) {
        markValue(arr->values[i]);
    }
}

void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif
    switch(object->type) {
        case OBJ_NATIVE:
        case OBJ_STR:
            break;
        case OBJ_UPVALUE:
            markValue(((ObjUpvalue*)object)->closed);
            break;
        case OBJ_FUN: {
            ObjFun* fun = (ObjFun*)object;
            markObject((Obj*)fun->name);
            markArray(&fun->chunk.constants);
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            markObject((Obj*)closure->fun);
            for(int i=0; i<closure->upvalueCount; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }
        case OBJ_CLASS: {
            ObjClass* klazz = (ObjClass*)object;
            markObject((Obj*)klazz->name);
            markTable(&klazz->methods);
            break;
        }
        case OBJ_INSTANCE: {
            ObjInstance* insta = (ObjInstance*)object;
            markObject((Obj*)insta->klass);
            markTable(&insta->fields);
            break;
        }
        case OBJ_BOUND_METHOD: {
            ObjBoundMethod* bound = (ObjBoundMethod*)object;
            markValue(bound->reciever);
            markObject((Obj*)bound->method);
            break;
        }
    }
}

void markRoots() {
    for(Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }

    for(int i=0; i<vm.frameCount; i++) {
        markObject((Obj*)vm.callFrames[i].closure);
    }

    for(ObjUpvalue* uv = vm.openUpvalues; uv != NULL; uv = uv->next) {
        markObject((Obj*)uv);
    }

    markTable(&vm.globals);
    markCompilerRoots();
    markObject((Obj*)vm.initString);
}

void traceReferences() {
    while (vm.grayCount > 0) {
        Obj* obj = vm.grayStack[--vm.grayCount];
        blackenObject(obj);
    }
}

void sweep() {
    Obj* prev = NULL;
    Obj* obj = vm.objects;
    while(obj != NULL) {
        if(obj->isMarked) {
            obj->isMarked = false;
            prev = obj;
            obj = obj->next;
        } else {
            Obj* unreached = obj;

            // unlink from Linked list
            obj = obj->next;
            if(prev != NULL) {
                prev->next = obj;
            } else {
                vm.objects = obj;
            }
            freeObject(unreached);
        }
    }
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("--gc begin\n");
    size_t before = vm.allocatedBytes;
#endif

    markRoots();
    traceReferences();
    tableRemoveWhite(&vm.strings);
    sweep();

    vm.nextGc = vm.allocatedBytes * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("--gc end\n");
    printf(" collected %ld bytes (from %ld to %ld). next at %ld\n",
    before - vm.allocatedBytes, before, vm.allocatedBytes, vm.nextGc);
#endif
}