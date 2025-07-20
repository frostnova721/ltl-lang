#include<stdio.h>
#include<stdlib.h>

#include "common.h"
#include "scanner.h"
#include "object.h"
#include "string.h"
#include "compiler.h"
#include "memory.h"

#ifdef DEBUG_PRINT_CODE
    #include "debug.h"
#endif

typedef struct
{
    Token current;
    Token previous;
    bool errored;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGN,
    PREC_OR,
    PREC_AND,
    PREC_EQUALITY,
    PREC_COMPARE,
    PREC_TERM,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_CALL,
    PREC_PRIMARY,
} Precedence;

typedef void (*ParseFun)(bool canAssign);

typedef struct {
    ParseFun prefix;
    ParseFun infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;


typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
    TYPE_METHOD,
    TYPE_INITIALIZER,
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFun* fun;
    FunctionType type;
    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth;
    Upvalue upvalues[UINT8_COUNT];
} Compiler;

typedef struct ClassCompiler
{
    struct ClassCompiler* enclosing;
    Token name;
    bool hasSuperClass;
} ClassCompiler;


Parser parser;

Compiler *current = NULL;

ClassCompiler *currentClass = NULL;

Chunk* compilingChunk;

static Chunk* currentChunk() {
    return &current->fun->chunk;
}

static void errorAt(Token* token, const char* msg) {
    if(parser.panicMode) return;
    parser.panicMode = true;
    // printf(msg);
    fprintf(stderr, "[line %d] Error", token->line);

    if(token->type == TOKEN_EOF) {
        fprintf(stderr, " at the end.");
    } else if (token->type == TOKEN_ERROR) {

    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", msg);
    parser.errored = true;
}

static void error(const char* msg) {
    errorAt(&parser.previous, msg);
}

static void errorAtCurrent(const char* msg) {
    errorAt(&parser.current, msg);
}


static void advance() {
    parser.previous = parser.current;

    for(;;) {
        parser.current = scanToken();
        if(parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);   
    }
}

static void consume(TokenType type, const char* msg) {
    if(parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(msg);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if(!check(type)) return false;
    advance();
    return true;
}

static void writeByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void writeBytes(uint8_t b1, uint8_t b2) {
    writeByte(b1);
    writeByte(b2);
}

static void writeLoop(int start) {
    writeByte(OP_LOOP);

    int offset = currentChunk()->count - start + 2;
    if(offset > UINT16_MAX) error("Too large loop body");

    writeByte((offset >> 8) & 0xff);
    writeByte(offset & 0xff);
}

static int writeJump(uint8_t inst) {
    writeByte(inst);
    writeByte(0xff);
    writeByte(0xff);
    return currentChunk()->count - 2;
}

static void writeReturn() {
    if(current->type == TYPE_INITIALIZER) {
        writeBytes(OP_GET_LOCAL, 0);
    } else {
        writeByte(OP_NULL);
    }
    writeByte(OP_RETURN);
} 

static uint8_t makeConstant(Value val) {
    int constant = addConstant(currentChunk(), val);
    if(constant > UINT8_MAX) {
        error("Too many constants in the chunk..");
        return 0;
    }

    return (uint8_t)constant;
}

static void writeConstant(Value value) {
    writeBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
    int jump = currentChunk()->count - offset - 2; // -2 for jump offset

    if(jump > UINT16_MAX) {
        error("Jump wont be enough for this much distance! (Too high value for jumping)");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset+1] = jump & 0xff;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->fun = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->fun = newFunction();
    current = compiler;

    if(type != TYPE_SCRIPT) {
        current->fun->name = copyString(parser.previous.start, parser.previous.length);
    }

    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if(type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFun* endCompiler() {
    writeReturn();
    ObjFun* fun = current->fun;
    #ifdef DEBUG_PRINT_CODE
        if(!parser.errored){
            disassembleChunk(currentChunk(), fun->name != NULL ? fun->name->chars : "<script>");
        }
    #endif
    current = current->enclosing;
    return fun;
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    // Pop till end of scope
    while(current->localCount > 0 &&
    current->locals[current->localCount-1].depth > current->scopeDepth) {
        if(current->locals[current->localCount-1].isCaptured) {
            writeByte(OP_CLOSE_UPVALUE);
        } else { 
            writeByte(OP_POP);
        }
        current->localCount--;
    }
}

static void expression();
static void declaration();
static void statement();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
static void variable(bool canAssign);
static void namedVariable(Token tkn, bool canAssign);
static Token syntheticToken(const char* text);

static uint8_t identifierConstant(Token* token) {
    return makeConstant(OBJ_VAL(copyString(token->start, token->length)));
}

static bool identifiersEqual(Token* a, Token*b) {
    if(a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
    for(int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if(identifiersEqual(&local->name, name)) {
            if(local->depth == -1) {
                error("Cant read local variable in its own initialiser.");
            }
            return i;
        }
    }

    return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->fun->upvalueCount;

    for(int i=0; i<upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if(upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if(upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in the funct!");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->fun->upvalueCount++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
    if(compiler->enclosing == NULL) return -1;
    int local = resolveLocal(compiler->enclosing, name);
    if(local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if(upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

static void addLocal(Token name) {
    if(current->localCount == UINT8_COUNT) {
        error("Too many local variables in a function");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}

static void declareVar() {
    if(current->scopeDepth == 0) return;

    Token* name = &parser.previous;

    for(int i = current->localCount - 1; i>=0; i--) {
        Local* local = &current->locals[i];
        if(local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if(identifiersEqual(name, &local->name)) {
            error("A variable with this name is already present in the this scope.");
        }
    }

    addLocal(*name);
}

static uint8_t parseVar(const char* message) {
    consume(TOKEN_IDENTIFIER, message);

    declareVar();
    if(current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

static void markInitialised() {
    if(current->scopeDepth == 0) return;
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVar(uint8_t global) {
    if(current->scopeDepth > 0) {
        markInitialised();
        return;
    }
    writeBytes(OP_DEFINE_GLOBAL, global);
}

static int argList() {
    uint8_t argCount = 0;
    if(!check(TOKEN_PARAN_RIGHT)) {
        do {
            expression();

            if(argCount == 255) error("Cant have >255 arguments.");

            argCount++;
        } while(match(TOKEN_COMMA));
    }
    consume(TOKEN_PARAN_RIGHT, "Expected ')' after arguments");
    return argCount;    
}

static void and_(bool canAssign) {
    int endJmp = writeJump(OP_JUMP_IF_FALSE);
    writeByte(OP_POP);
    parsePrecedence(PREC_AND);
    patchJump(endJmp);
}

static void binary(bool canAssign) {
    TokenType op = parser.previous.type;

    ParseRule* rule = getRule(op);
    parsePrecedence((Precedence)rule->precedence + 1);

    switch(op) {
        case TOKEN_PLUS: writeByte(OP_ADD); break;
        case TOKEN_MINUS: writeByte(OP_SUBTRACT); break;
        case TOKEN_STAR: writeByte(OP_MULTIPLY); break;
        case TOKEN_SLASH: writeByte(OP_DIVIDE); break;

        case TOKEN_BANG_EQUAL: writeBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL: writeByte(OP_EQUAL); break;
        case TOKEN_GREATER: writeByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: writeBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS: writeByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL: writeBytes(OP_GREATER, OP_NOT); break;
        
        default:
            return; // stuff's unreachable atp.
    }
}

static void call(bool canAssign) {
    uint8_t argCount = argList();
    writeBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expected an identifier after '.'");
    uint8_t name = identifierConstant(&parser.previous);
    if(canAssign && match(TOKEN_EQUAL)) {
        expression();
        writeBytes(OP_SET_PROPERTY, name);
    } else if (match(TOKEN_PARAN_LEFT)) {
        uint8_t argCount = argList();
        writeBytes(OP_INVOKE, name);
        writeByte(argCount);
    } else {
        writeBytes(OP_GET_PROPERTY, name);
    }
}

static void literal(bool canAssign) {
    switch(parser.previous.type) {
        case TOKEN_FALSE: writeByte(OP_FALSE); break;
        case TOKEN_TRUE: writeByte(OP_TRUE); break;
        case TOKEN_NULL: writeByte(OP_NULL); break;
        default: return;
    }
}

static void expression() {
    parsePrecedence(PREC_ASSIGN);
}

static void block() {
    while(!check(TOKEN_BRACE_RIGHT) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_BRACE_RIGHT, "Expected '}' at the end of block.");
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();
    consume(TOKEN_PARAN_LEFT, "Expected '(' after the function name.");

    if(!check(TOKEN_PARAN_RIGHT)) {
        do {
            current->fun->arity++;
            if(current->fun->arity > 255) {
                errorAtCurrent("Max 255 parameters are supported.");
            }
            uint8_t paramConst = parseVar("Expected parameter name.");
            defineVar(paramConst);
        } while(match(TOKEN_COMMA));
    }

    consume(TOKEN_PARAN_RIGHT, "Expected ')' after the parameters.");

    consume(TOKEN_BRACE_LEFT, "Expected '{' before function body.");
    block();
    ObjFun* fun = endCompiler();

    writeBytes(OP_CLOSURE, makeConstant(OBJ_VAL(fun)));

    for(int i=0; i<fun->upvalueCount; i++) {
        writeByte(compiler.upvalues[i].isLocal ? 1 : 0);
        writeByte(compiler.upvalues[i].index);
    }
}

static void method() {
    consume(TOKEN_IDENTIFIER, "Expected method name.");
    uint8_t constant = identifierConstant(&parser.previous);
    FunctionType type = TYPE_METHOD;
    if(parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }
    function(type);
    writeBytes(OP_METHOD, constant);
}

static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expected class name.");
    uint8_t namedConstant = identifierConstant(&parser.previous);
    Token className = parser.previous;
    declareVar();
    writeBytes(OP_CLASS, namedConstant);
    defineVar(namedConstant);

    ClassCompiler classCompiler;
    classCompiler.name = parser.previous;
    classCompiler.hasSuperClass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    if(match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expected a superclass name.");
        variable(false);
        if(identifiersEqual(&className, &parser.previous)) {
            error("Classes cant inherit from itself");
        }

        beginScope();
        addLocal(syntheticToken("super"));
        defineVar(0);

        namedVariable(className, false);
        writeByte(OP_INHERIT);
        classCompiler.hasSuperClass = true;
    }

    namedVariable(className, false);
    consume(TOKEN_BRACE_LEFT, "Expected '{' before class body");

    while(!check(TOKEN_BRACE_RIGHT) && !check(TOKEN_EOF)) {
        method();
    }

    consume(TOKEN_BRACE_RIGHT, "Expected '}' after class body");
    writeByte(OP_POP);

    if(classCompiler.hasSuperClass) {
        endScope();
    }
    
    currentClass = currentClass->enclosing;
}

static void funDeclaration() {
    uint8_t global = parseVar("Expected function name.");
    markInitialised();
    function(TYPE_FUNCTION);
    defineVar(global);
}

static void varDeclaration() {
    uint8_t global = parseVar("Expected variable name.");

    if(match(TOKEN_EQUAL)) {
        expression();
    } else {
        writeByte(OP_NULL);
    }
    consume(TOKEN_SEMICOLON, "Expected ';' after variable declaration.");
    defineVar(global);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expected ';' after the expression.");
    writeByte(OP_POP);
}

static void ifStatement() {
    consume(TOKEN_PARAN_LEFT, "Expected '(' after 'if' keyword");
    expression();
    consume(TOKEN_PARAN_RIGHT, "Expected ')' after the condition");

    int then = writeJump(OP_JUMP_IF_FALSE);
    writeByte(OP_POP);
    statement();

    int elseJmp = writeJump(OP_JUMP);

    patchJump(then);
    writeByte(OP_POP);

    if(match(TOKEN_ELSE)) statement();
    patchJump(elseJmp);
}

static void printStatement() {
    expression();   
    consume(TOKEN_SEMICOLON, "Expected ';' after value.");
    writeByte(OP_PRINT);
}

static void returnStatement() {
    if(current->type == TYPE_SCRIPT) {
        error("return can't be performed in top level code.");
    }
    if(match(TOKEN_SEMICOLON)) {
        writeReturn();
    } else {
        if(current->type == TYPE_INITIALIZER) {
            error("Cannot return a value from initializer.");
        }
        expression();
        consume(TOKEN_SEMICOLON, "Expected ';' after return value.");
        writeByte(OP_RETURN);
    }
}

static void whileStatement() {
    int loopStart = currentChunk()->count;
    consume(TOKEN_PARAN_LEFT, "Expected '(' after 'while'.");
    expression();
    consume(TOKEN_PARAN_RIGHT, "Expected ')' after condition.");
    
    int exitJmp = writeJump(OP_JUMP_IF_FALSE);
    writeByte(OP_POP);
    statement();

    writeLoop(loopStart);

    patchJump(exitJmp);
    writeByte(OP_POP);
}

static void forStatement() {
    beginScope();
    consume(TOKEN_PARAN_LEFT, "Expected '(' after 'for'");
    
    if(match(TOKEN_SEMICOLON)) {}
    else if(match(TOKEN_VAR)) varDeclaration();
    else expressionStatement();

    // consume(TOKEN_SEMICOLON, "Expected ';'.");

    int loopStart = currentChunk()->count;

    int exitJmp = -1;
    if(!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expected ';' after the loop condtion.");

        // exit the loop on falsey condition
        exitJmp = writeJump(OP_JUMP_IF_FALSE);
        writeByte(OP_POP);
    }

    if(!match(TOKEN_PARAN_RIGHT)) {
        int bodyJmp = writeJump(OP_JUMP);
        int incStart = currentChunk()->count;
        expression();
        writeByte(OP_POP);
        consume(TOKEN_PARAN_RIGHT, "Expected ')' after the expression");
        writeLoop(loopStart);
        loopStart = incStart;
        patchJump(bodyJmp);
    }

    statement();

    if(exitJmp != -1) {
        patchJump(exitJmp);
        writeByte(OP_POP);
    }

    writeLoop(loopStart);

    endScope();
}

static void synchronize() {
    parser.panicMode = false;

    while(parser.current.type != TOKEN_EOF) {
        if(parser.previous.type == TOKEN_SEMICOLON) return;

        switch (parser.current.type)
        {
        case TOKEN_CLASS:
        case TOKEN_FUNCT:
        case TOKEN_VAR:
        case TOKEN_IF:
        case TOKEN_FOR:
        case TOKEN_WHILE:
        case TOKEN_PRINT:
        case TOKEN_RETURN:
            return;
        
        default: ;
            // silence!
        }
    
        advance();
    }
}

static void declaration() {
    if(match(TOKEN_FUNCT)) {
        funDeclaration();
    } else if(match(TOKEN_VAR)) {
        varDeclaration();
    } else if(match(TOKEN_CLASS)) {
        classDeclaration();
    }else {
        statement();
    }

    if(parser.panicMode) synchronize();
}

static void statement() {
    if(match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_BRACE_LEFT)) {
        beginScope();
        block();
        endScope();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } 
    else if (match(TOKEN_IF)) {
        ifStatement();
    } else if(match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else {
        expressionStatement();
    }
}

static void grouping(bool canAssssign) {
    expression();
    consume(TOKEN_PARAN_RIGHT, "Expected ')' after expression.");
}

static void number(bool canAssign) {
    double val = strtod(parser.previous.start, NULL);
    writeConstant(NUM_VAL(val));
}

static void or_(bool canAssign) {
    int elseJmp = writeJump(OP_JUMP_IF_FALSE);
    int endJmp = writeJump(OP_JUMP);
    patchJump(elseJmp);
    writeByte(OP_POP);
    parsePrecedence(PREC_OR);
    patchJump(endJmp);
}

static void string(bool canAssign) {
    ObjString* str = copyString(parser.previous.start + 1, parser.previous.length-2);
    writeConstant(OBJ_VAL(str));
}

static void namedVariable(Token tkn, bool canAssign) {
    uint8_t getOp, setOp;


    int arg = resolveLocal(current, &tkn);

    if(arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if((arg = resolveUpvalue(current, &tkn)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&tkn);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }
    if(canAssign && match(TOKEN_EQUAL)) {
        expression();
        writeBytes(setOp, (uint8_t)arg);
    } else {
        writeBytes(getOp, (uint8_t)arg);       
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

static void super_(bool canAssign) {
    if(currentClass == NULL) {
        error("Cant use 'super' outside a class");
    } else if(!currentClass->hasSuperClass) {
        error("Cant use 'super' inside classes having no super class.");
    }
    consume(TOKEN_DOT, "Expected '.' after 'super' reference");
    consume(TOKEN_IDENTIFIER, "Expected a super method name.");
    uint8_t name = identifierConstant(&parser.previous);

    namedVariable(syntheticToken("this"), false);
    if(match(TOKEN_PARAN_LEFT)) {
        uint8_t argCount = argList();
        namedVariable(syntheticToken("super"), false);
        writeBytes(OP_SUPER_INVOKE, name);
        writeByte(argCount);
    } else {
        namedVariable(syntheticToken("super"), false);
        writeBytes(OP_GET_SUPER, name);
    }
}

static void this_(bool canAssign) {
    if(currentClass == NULL) {
        error("Cant use 'this' outside of a class.");
        return;
    }
    variable(false);
}

static void unary(bool canAssign) {
    TokenType opType = parser.previous.type;

    parsePrecedence(PREC_UNARY);

    switch(opType) {
        case TOKEN_MINUS: writeByte(OP_NEGATE); break;
        case TOKEN_BANG: writeByte(OP_NOT); break;
        default:
            return;
    }
}

ParseRule rules[] = {
    [TOKEN_PARAN_LEFT] = {grouping, call, PREC_CALL},
    [TOKEN_PARAN_RIGHT] = {NULL, NULL, PREC_NONE},
    [TOKEN_BRACE_LEFT] = {NULL, NULL, PREC_NONE},
    [TOKEN_BRACE_RIGHT] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, dot, PREC_CALL},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARE},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARE},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARE},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARE},
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUM] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, and_, PREC_NONE},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUNCT] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NULL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_OR},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {super_, NULL, PREC_NONE},
    [TOKEN_THIS] = {this_, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFun prefixRule = getRule(parser.previous.type)->prefix;
    if(prefixRule == NULL) {
        error("Expected expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGN;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence)
    {
        advance();
        ParseFun infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if(canAssign && match(TOKEN_EQUAL)){
        error("Invalid assignment target.");
    }
    
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

ObjFun* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.errored = false;
    parser.panicMode = false;

    advance();

    while(!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFun* fun = endCompiler();
    return parser.errored ? NULL : fun;
}

void markCompilerRoots() {
    Compiler* compiler = current;
    while(compiler != NULL) {
        markObject((Obj*)compiler->fun);
        compiler = compiler->enclosing;
    }
}