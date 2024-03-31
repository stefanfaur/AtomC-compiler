#include <stdio.h>
#include <stdlib.h>

#include "utils.h"
#include "lexer.h"
#include "parser.h"

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    char *filename = argv[1];
    char *inbuf = loadFile(filename);
    Token *tokens = tokenize(inbuf);
    free(inbuf);
    //showTokens(tokens); for debugging, prints all tokens found by the lexer
    parse(tokens);
    return 0;
}