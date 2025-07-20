#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
    const char* start;
    const char* current;
    int line;
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

static bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            c == '_';
}

static bool reachedEnd() {
    return *scanner.current == '\0';
}

static char advance() {
    scanner.current++;
    return scanner.current[-1];
}

static char peek() {
    return *scanner.current;
}

static char peekNext() {
    if(reachedEnd()) return '\0';
    return scanner.current[1];
}

static bool match(char expectation) {
    if(reachedEnd()) return false;
    if(*scanner.current != expectation) return false;
    scanner.current++;
    return true;
}

static Token makeToken(TokenType type) {
    Token token;
    token.type = type;
    token.start = scanner.start;
    token.length = (int)(scanner.current - scanner.start);
    token.line = scanner.line;
    return token;
}

static Token errorToken(const char* msg) {
    Token token;
    token.type = TOKEN_ERROR;
    token.start = msg;
    token.length = (int)strlen(msg);
    token.line = scanner.line;
    return token;
}

// Yes it eats white space, like it never existed!
void eatWhiteSpace() {
    for(;;) {
        char c = peek();
        switch(c) {
            case ' ':
            case '\r':
            case '\t':
                advance();
                break;
            case '\n':
                scanner.line++;
                advance();
                break;
            case '/':
                if(peekNext() == '/') {
                    while(peek() != '\n' && !reachedEnd()) advance();
                } else {
                    return;
                }
                break;
            default:
                return;
        }
    }
}

static TokenType checkKwd(int start, int length, const char* rest, TokenType type) {
    if(scanner.current - scanner.start == start + length && memcmp(scanner.start + start, rest, length) == 0) {
        return type;
    }
    return TOKEN_IDENTIFIER;
}

static TokenType identifierType() {
    switch (scanner.start[0])
    {
        case 'a': return checkKwd(1,2, "nd", TOKEN_AND);
        case 'c': return checkKwd(1,4, "lass", TOKEN_CLASS);
        case 'e': return checkKwd(1,3, "lse", TOKEN_ELSE);
        case 'i': return checkKwd(1,1, "f", TOKEN_IF);
        case 'n': return checkKwd(1,3, "ull", TOKEN_NULL);
        case 'o': return checkKwd(1,1, "r", TOKEN_OR);
        case 'p': return checkKwd(1,4, "rint", TOKEN_PRINT);
        case 'r': return checkKwd(1,5, "eturn", TOKEN_RETURN);
        case 's': return checkKwd(1,4, "uper", TOKEN_SUPER);
        case 'v': return checkKwd(1,2, "ar", TOKEN_VAR);
        case 'w': return checkKwd(1,4, "hile", TOKEN_WHILE);

        case 'f': {
            if(scanner.current - scanner.start > 1) {
                switch (scanner.start[1])
                {
                    case 'a': return checkKwd(2,3, "lse", TOKEN_FALSE);
                    case 'o': return checkKwd(2,1, "r", TOKEN_FOR);
                    case 'u': return checkKwd(2, 3, "nct", TOKEN_FUNCT);
                }
            }
            break;
        }
        case 't': {
            if(scanner.current - scanner.start > 1) {
                switch (scanner. start[1])
                 {
                    case 'h': return checkKwd(2,2, "is", TOKEN_THIS);
                    case 'r': return checkKwd(2,2, "ue", TOKEN_TRUE);
                }
            }
            break;
        }
    }
    return TOKEN_IDENTIFIER;
}

static Token identifier() {
    while(isAlpha(peek()) || isDigit(peek())) advance();

    return makeToken(identifierType());
}

static Token string() {
    while (peek() != '"' && !reachedEnd())
    {
        if(peek() == '\n') scanner.line++;
        advance();
    }

    if(reachedEnd()) return errorToken("Unterminated string literal.");
    
    advance();
    return makeToken(TOKEN_STRING);
}

static Token number() {
    while(isDigit(peek())) advance();

    if(peek() == '.' && isDigit(peek())) advance();

    return makeToken(TOKEN_NUM);
}


Token scanToken() {
    eatWhiteSpace();

    scanner.start = scanner.current;

    if(reachedEnd()) return makeToken(TOKEN_EOF);

    char c = advance();

    if(isAlpha(c)) return identifier();

    if(isDigit(c)) return number();

    switch (c)
    {
    case '(': return makeToken(TOKEN_PARAN_LEFT);
    case ')': return makeToken(TOKEN_PARAN_RIGHT);
    case '{': return makeToken(TOKEN_BRACE_LEFT);
    case '}': return makeToken(TOKEN_BRACE_RIGHT);  
    case ',': return makeToken(TOKEN_COMMA);
    case ':': return makeToken(TOKEN_COLON);
    case ';': return makeToken(TOKEN_SEMICOLON);
    case '.': return makeToken(TOKEN_DOT);
    case '-': return makeToken(TOKEN_MINUS);
    case '+': return makeToken(TOKEN_PLUS);
    case '*': return makeToken(TOKEN_STAR);
    case '/': return makeToken(TOKEN_SLASH);
    case '?': return makeToken(TOKEN_QUESTION);

    case '!': return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
    case '=': return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
    case '>': return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
    case '<': return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);

    case '"': return string();
    }

    return errorToken("Unexpected character.");
}