#ifndef cltl_debug_h
#define cltl_debug_h

#include "chunk.h"

void disassembleChunk(Chunk* chunk, const char* name);
int  disassembleInstruction(Chunk* chunk, int offset);

#endif