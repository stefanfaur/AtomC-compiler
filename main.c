#include <stdio.h>
#include <stdlib.h>

#include "utils.h"
#include "lexer.h"
//#include "parser.h"

int main () {
    char *inbuf = loadFile("tests/testparser.c");
    Token *tokens = tokenize(inbuf);
    free(inbuf);
    showTokens(tokens);
    //parse(tokens);
    return 0;
}