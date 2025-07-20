#ifndef cltl_value_h
#define cltl_value_h

#include<string.h>

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOX

#define QNAN ((uint64_t)0x7ffc000000000000)
#define SIGN_BIT ((uint64_t)0x8000000000000000)

#define TAG_NULL 1
#define TAG_FALSE 2
#define TAG_TRUE 3

typedef uint64_t Value;

#define NULL_VAL ((Value)(uint64_t)(QNAN | TAG_NULL))
#define IS_NULL(val) ((val) == NULL_VAL)

#define BOOL_VAL(val) ((val) ? TRUE_VAL : FALSE_VAL)
#define AS_BOOL(val) ((val) == TRUE_VAL)
#define IS_BOOL(val) (((val) | 1) == TRUE_VAL)

#define FALSE_VAL ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL ((Value)(uint64_t)(QNAN | TAG_TRUE))

#define OBJ_VAL(obj) \
    (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))
#define AS_OBJ(val) \
    ((Obj*)(uintptr_t)((val) & ~(SIGN_BIT | QNAN)))
#define IS_OBJ(val) \
    (((val) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define IS_NUM(val) (((val) & QNAN) != QNAN)
#define AS_NUM(val) valueToNum(val)
#define NUM_VAL(num) numToValue(num)

static inline Value numToValue(double num) {
    Value value;
    memcpy(&value, &num, sizeof(double));
    return value;
}

static inline double valueToNum(Value val) {
    double num;
    memcpy(&num, &val, sizeof(Value));
    return num;
}

#else

typedef enum {
    VAL_BOOL,
    VAL_NULL,
    VAL_NUM,
    VAL_OBJ,
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj* obj;
    } as;
} Value;

#define IS_BOOL(val) ((val).type == VAL_BOOL)
#define IS_NUM(val) ((val).type == VAL_NUM)
#define IS_NULL(val) ((val).type == VAL_NULL)
#define IS_OBJ(val) ((val).type == VAL_OBJ)

#define AS_BOOL(val) ((val).as.boolean)
#define AS_NUM(val) ((val).as.number)
#define AS_OBJ(val) ((val).as.obj)

#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define NULL_VAL        ((Value){VAL_NULL, {.number = 0}})
#define NUM_VAL(value) ((Value) {VAL_NUM, {.number = value}})
#define OBJ_VAL(object) ((Value) {VAL_OBJ, {.obj = (Obj*)object}})

#endif

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif