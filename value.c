#include <stdio.h>
#include<string.h>

#include "memory.h"
#include "object.h"
#include "value.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void writeValueArray(ValueArray* array, Value value) {
    if(array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(Value value) {
#ifdef NAN_BOX
    if(IS_BOOL(value)) {
        printf(AS_BOOL(value) ? "true" : "false");
    } else if (IS_NULL(value)) {
        printf("null");
    } else if (IS_NUM(value)) {
        printf("%g", AS_NUM(value));
    } else if (IS_OBJ(value)) {
        printObj(value);
    }
#else
    switch(value.type) {
        case VAL_BOOL: printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NULL: printf("null"); break;
        case VAL_NUM: printf("%g", AS_NUM(value)); break;
        case VAL_OBJ: printObj(value); break;
    }
#endif
}

bool valuesEqual(Value a, Value b) {
#ifdef NAN_BOX
    if(IS_NUM(a) && IS_NUM(b)) {
        return AS_NUM(a) == AS_NUM(b);
    }
    return a==b;
#else
    if(a.type != b.type) {
        return false;
    }

    switch(a.type) {
        case VAL_BOOL: return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NULL: return true;
        case VAL_NUM: return AS_NUM(a) == AS_NUM(b);

        case VAL_OBJ: return AS_OBJ(a) == AS_OBJ(b);

        default: return false;  
    }
#endif
}