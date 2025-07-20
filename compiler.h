#ifndef cltl_compiler_h
#define cltl_compiler_h

#include "chunk.h"
#include "object.h"

ObjFun* compile(const char* source);
void markCompilerRoots();

#endif