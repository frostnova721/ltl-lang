#ifndef cltl_object_h
#define cltl_object_h

#include "common.h"
#include "value.h"
#include "chunk.h"
#include "table.h"

#define OBJ_TYPE(val) (AS_OBJ(val)->type)

#define IS_STRING(val) isObjType(val, OBJ_STR)
#define IS_FUNCTION(val) isObjType(val, OBJ_FUN)
#define IS_NATIVE(val) isObjType(val, OBJ_NATIVE)
#define IS_CLOSURE(val) isObjType(val, OBJ_CLOSURE)
#define IS_CLASS(val) isObjType(val, OBJ_CLASS)
#define IS_INSTANCE(val) isObjType(val, OBJ_INSTANCE)
#define IS_BOUND_METHOD(val) isObjType(val, OBJ_BOUND_METHOD)

#define AS_STRING(val) ((ObjString*)AS_OBJ(val))
#define AS_CSTRING(val) (((ObjString*)AS_OBJ(val))->chars)
#define AS_FUNCTION(val) ((ObjFun*)AS_OBJ(val))
#define AS_NATIVE(val) \
    (((ObjNative*)AS_OBJ(val))->fun)
#define AS_CLOSURE(val) ((ObjClosure*)AS_OBJ(val))
#define AS_CLASS(val) ((ObjClass*)AS_OBJ(val))
#define AS_INSTANCE(val) ((ObjInstance*)AS_OBJ(val))
#define AS_BOUND_METHOD(val) ((ObjBoundMethod*)AS_OBJ(val))

typedef enum {
    OBJ_STR, //string
    OBJ_FUN,
    OBJ_NATIVE,
    OBJ_CLOSURE,
    OBJ_UPVALUE,
    OBJ_CLASS,
    OBJ_INSTANCE,
    OBJ_BOUND_METHOD,
} ObjType;

struct Obj
{
    ObjType type;
    bool isMarked;
    struct Obj* next;
};

typedef struct {
    Obj obj;
    int arity;
    int upvalueCount;
    Chunk chunk;
    ObjString* name;
} ObjFun;

typedef Value (*NativeFun) (int argCount, Value* args);

typedef struct {
    Obj obj;
    NativeFun fun;
} ObjNative;

struct ObjString {
    Obj obj;
    int length;
    char* chars;
    uint32_t hashCode;
};

typedef struct {
    Obj obj;
    ObjString* name;
    Table methods;
} ObjClass;

typedef struct {
    Obj obj;
    ObjClass* klass;
    Table fields;
} ObjInstance;

typedef struct ObjUpvalue {
    Obj obj;
    Value* location;
    struct ObjUpvalue* next;
    Value closed;
} ObjUpvalue;

typedef struct {
    Obj obj;
    ObjFun* fun;
    ObjUpvalue** upvalues;
    int upvalueCount;
} ObjClosure;

typedef struct {
    Obj obj;
    Value reciever;
    ObjClosure* method;
} ObjBoundMethod;

ObjString* copyString(const char* chars, int length);
ObjString* takeString(char* chars, int length);

ObjFun *newFunction();
ObjClosure *newClosure(ObjFun* fun);
ObjNative *newNative(NativeFun fun);
ObjUpvalue *newUpvalue(Value* slot);
ObjClass *newClass(ObjString* name);
ObjInstance *newInstance(ObjClass* klass);
ObjBoundMethod *newBoundMethod(Value receiver, ObjClosure* method);

void printObj(Value val);

static inline bool isObjType(Value val, ObjType type) {
    return IS_OBJ(val) && AS_OBJ(val)->type == type;
}


#endif