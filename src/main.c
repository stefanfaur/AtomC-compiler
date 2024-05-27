#include "main.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "headers/ad.h"
#include "headers/lexer.h"
#include "headers/parser.h"
#include "headers/utils.h"
#include "headers/vm.h"

int main(int argc, char* argv[]) {
  if (argc == 2 && strcmp(argv[1], "--vmtest") == 0) {  // handle --vmtest flag
    return runVmTest();
  }

  if (argc != 2) {
    printf("Usage: %s <filename>\n", argv[0]);
    return 1;
  }

  char* filename = argv[1];
  char* inbuf = loadFile(filename);
  Token* tokens = tokenize(inbuf);
  free(inbuf);
  // showTokens(tokens); for debugging, prints all tokens found by the lexer

  pushDomain();
  vmInit();
  parse(tokens);

  // code generation
  Symbol* symMain = findSymbolInDomain(symTable, "main");
  if (!symMain) err("missing main function");
  Instr* entryCode = NULL;
  addInstr(&entryCode, OP_CALL)->arg.instr = symMain->fn.instr;
  addInstr(&entryCode, OP_HALT);
  run(entryCode);

  showDomain(symTable, "global");
  dropDomain();

  return 0;
}

int runVmTest() {
  pushDomain();
  vmInit();
  Instr* testCode = genTestProgram();
  run(testCode);
  dropDomain();
  return 0;
}
