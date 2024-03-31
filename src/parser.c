#include "parser.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define SILENCE_LOGS 1

Token *iTk;        // the iterator in the tokens list
Token *consumedTk; // the last consumed token

void tkerr(const char *fmt, ...) {
    fprintf(stderr, "error in line %d, col %d: ", iTk->line, iTk->col);
    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);

    int whitespace_count = 0;
    // remove all whitespace at the start of lineText
    while (iTk->lineText[0] == ' ' || iTk->lineText[0] == '\t') {
        iTk->lineText++;
        whitespace_count++;
    }

    fprintf(stderr, "\n%s\n", iTk->lineText);
    for (int i = 1; i < iTk->col - whitespace_count; ++i) {
        fputc(' ', stderr); // align
    }
    fputc('^', stderr); // point to the error
    

    fprintf(stderr, "\n"); 
    exit(EXIT_FAILURE); 
}

void tklog(const char *fmt, ...) {
  if (SILENCE_LOGS)
    return;
  fprintf(stdout, "%d: ", iTk->line);
  va_list va;
  va_start(va, fmt);
  vfprintf(stdout, fmt, va);
  va_end(va);
  fprintf(stdout, "\n");
}

bool consume(int code) {
  tklog("trying consume(%s)", getTokenType(code));
  if (iTk->code == code) {
    tklog("token is %s", getTokenType(iTk->code));
    consumedTk = iTk;
    if (iTk->next != NULL) {
      iTk = iTk->next;
    }
    tklog(" => consumed\n");
    return true;
  }
  tklog(" => found %s\n", getTokenType(iTk->code));
  return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase() {
  tklog("typeBase()");
  Token *start = iTk;
  if (consume(TYPE_INT)) {
    return true;
  }
  if (consume(TYPE_DOUBLE)) {
    return true;
  }
  if (consume(TYPE_CHAR)) {
    return true;
  }
  if (consume(STRUCT)) {
    if (consume(ID)) {
      return true;
    } else
      tkerr("Missing identifier after 'struct' keyword");
  }
  iTk = start;
  tklog("typeBase() failed on token %s", getTokenType(iTk->code));
  return false;
}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl() {
  tklog("arrayDecl()");
  Token *start = iTk;
  if (consume(LBRACKET)) {
    if (consume(INT)) {
    }
    if (consume(RBRACKET)) {
      return true;
    }
    tkerr("Missing ] in array declaration");
  }
  iTk = start;
  tklog("arrayDecl() failed on token %s", getTokenType(iTk->code));
  return false;
}

// varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef() {
  tklog("varDef()");
  Token *start = iTk;
  if (typeBase()) {
    if (consume(ID)) {
      if (arrayDecl()) {
        if (consume(SEMICOLON)) {
          return true;
        } else
          tkerr("Missing ; in variable declaration");
      }
      if (consume(SEMICOLON)) {
        return true;
      } else
        tkerr("Missing ; in variable declaration");
    } else
      tkerr("Missing identifier in variable declaration");
  }
  iTk = start;
  tklog("varDef() failed on token %s", getTokenType(iTk->code));
  return false;
}

// structDef: STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef() {
  tklog("structDef()");
  Token *start = iTk;
  if (consume(STRUCT)) {
    if (consume(ID)) {
      if (consume(LACC)) {
        for (;;) {
          if (varDef()) {
          } else
            break;
        }
        if (consume(RACC)) {
          if (consume(SEMICOLON)) {
            return true;
          } else
            tkerr("Missing ; after struct definition");
        } else
          tkerr("Missing } in struct definition");
      }
    }
  }
  iTk = start;
  tklog("structDef() failed on token %s", getTokenType(iTk->code));
  return false;
}

// fnParam: typeBase ID arrayDecl?
bool fnParam() {
  tklog("fnParam()");
  Token *start = iTk;
  if (typeBase()) {
    if (consume(ID)) {
      if (arrayDecl()) {
        return true;
      }
      return true;
    } else
      tkerr("Missing identifier in function parameter");
  }
  iTk = start;
  tklog("fnParam() failed on token %s", getTokenType(iTk->code));
  return false;
}

/*
fnDef: ( typeBase | VOID ) ID
                LPAR ( fnParam ( COMMA fnParam )* )? RPAR
                stmCompound
*/
bool fnDef() {
  tklog("fnDef()");
  Token *start = iTk;
  if (consume(VOID)) {
    if (consume(ID)) {
      if (consume(LPAR)) {
        if (fnParam()) {
          for (;;) {
            if (consume(COMMA)) {
              if (fnParam()) {
              } else {
                tkerr("Missing parameter after , in function definition");
                break;
              }
            } else
              break;
          }
        }
        if (consume(RPAR)) {
          if (stmCompound()) {
            return true;
          }
        }
      } else
        tkerr("Missing ( in function definition");
    } else
      tkerr("Missing identifier in function definition");
  } else if (typeBase()) {
    if (consume(ID)) {
      if (consume(LPAR)) {
        if (fnParam()) {
          for (;;) {
            if (consume(COMMA)) {
              if (fnParam()) {
              } else {
                tkerr("Missing parameter after , in function definition");
                break;
              }
            } else
              break;
          }
        }
        if (consume(RPAR)) {
          if (stmCompound()) {
            return true;
          }
        }
      } 
    } else
      tkerr("Missing identifier in function definition");
  }
  iTk = start;
  tklog("fnDef() failed on token %s", getTokenType(iTk->code));
  return false;
}

// stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound() {
  tklog("stmCompound()");
  Token *start = iTk;
  if (consume(LACC)) {
    for (;;) {
      if (varDef()) {
      } else if (stm()) {
      } else
        break;
    }
    if (consume(RACC)) {
      return true;
    } else
      tkerr("Missing } in compound statement");
  }
  iTk = start;
  tklog("stmCompound() failed on token %s", getTokenType(iTk->code));
  ;
  return false;
}

/*
stm: stmCompound
| IF LPAR expr RPAR stm ( ELSE stm )?
| WHILE LPAR expr RPAR stm
| RETURN expr? SEMICOLON
| expr? SEMICOLON
*/
bool stm() {
  tklog("stm()");
  Token *start = iTk;
  // stmCompound
  if (stmCompound()) {
    return true;
  }

  // | IF LPAR expr RPAR stm ( ELSE stm )?
  if (consume(IF)) {
    if (consume(LPAR)) {
      if (expr()) {
        if (consume(RPAR)) {
          if (stm()) {
            if (consume(ELSE)) {
              if (stm()) {
                return true;
              } else
                tkerr("Missing statement after else condition");
            }
            return true;
          } else
            tkerr("Missing statement after if condition");
        } else
          tkerr("Missing ) in if statement");
      } else
        tkerr("Missing expression in if statement");
    } else
      tkerr("Missing ( in if statement");
  }

  // | WHILE LPAR expr RPAR stm
  if (consume(WHILE)) {
    if (consume(LPAR)) {
      if (expr()) {
        if (consume(RPAR)) {
          if (stm()) {
            return true;
          } else
            tkerr("Missing statement in while statement");
        } else
          tkerr("Missing ) in while statement");
      } else
        tkerr("Missing expression in while statement");
    } else
      tkerr("Missing ( in while statement");
  }

  // | RETURN expr? SEMICOLON
  if (consume(RETURN)) {
    if (expr()) {
      if (consume(SEMICOLON)) {
        return true;
      } else
        tkerr("Missing ; in return statement");
    }
    if (consume(SEMICOLON)) {
      return true;
    }
  }

  // | expr? SEMICOLON
  if (expr()) {
    if (consume(SEMICOLON)) {
      return true;
    } else
      tkerr("Missing ; in expression statement");
  } else if (consume(SEMICOLON)) {
    return true;
  }

  iTk = start;
  tklog("stm() failed on token %s", getTokenType(iTk->code));
  return false;
}

// expr: exprAssign
bool expr() {
  tklog("expr()");
  if (exprAssign()) {
    return true;
  }
  return false;
  tklog("expr() failed on token %s", getTokenType(iTk->code));
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign() {
  tklog("exprAssign()");
  Token *start = iTk;
  if (exprUnary()) {
    if (consume(ASSIGN)) {
      if (exprAssign()) {
        return true;
      } else
        tkerr("Missing expression after = operator");
    }
  }
  iTk = start; // backtrack if first fails
  if (exprOr()) {
    return true;
  }
  iTk = start;
  tklog("exprAssign() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprOr: exprOr OR exprAnd | exprAnd
// ----------------------------------------------- left recursive elimination
// exprOr: exprAnd exprOrPrim
bool exprOr() {
  tklog("exprOr()");
  Token *start = iTk;
  if (exprAnd()) {
    if (exprOrPrim()) {
      return true;
    }
  }
  iTk = start;
  tklog("exprOr() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprOrPrim: OR exprAnd exprOrPrim | epsilon
bool exprOrPrim() {
  tklog("exprOrPrim()");
  Token *start = iTk;
  if (consume(OR)) {
    if (exprAnd()) {
      if (exprOrPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after || operator");
  }
  iTk = start;
  return true;
}

// exprAnd: exprAnd AND exprEq | exprEq
// ----------------------------------------------- left recursive elimination
// exprAnd -> exprEq exprAndPrim
bool exprAnd() {
  tklog("exprAnd()");
  Token *start = iTk;
  if (exprEq()) {
    if (exprAndPrim()) {
      return true;
    }
  }
  iTk = start;
  tklog("exprAnd() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprAndPrim -> AND exprEq exprAndPrim | epsilon
bool exprAndPrim() {
  tklog("exprAndPrim()");
  Token *start = iTk;
  if (consume(AND)) {
    if (exprEq()) {
      if (exprAndPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after && operator");
  }
  iTk = start;
  return true;
}

// exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
// ----------------------------------------------- left recursive elimination
// exprEq = exprRel exprEqPrim
bool exprEq() {
  tklog("exprEq()");
  Token *start = iTk;
  if (exprRel()) {
    if (exprEqPrim()) {
      return true;
    }
  }
  tklog("exprEq() failed on token %s", getTokenType(iTk->code));
  iTk = start;
  return false;
}

// exprEqPrim = ( EQUAL | NOTEQ ) exprRel exprEqPrim | epsilon
bool exprEqPrim() {
  tklog("exprEqPrim()");
  Token *start = iTk;
  if (consume(EQUAL)) {
    if (exprRel()) {
      if (exprEqPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after == operator");
  } else if (consume(NOTEQ)) {
    if (exprRel()) {
      if (exprEqPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after != operator");
  }
  iTk = start;
  return true;
}

// exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
// ----------------------------------------------- left recursive elimination
// exprRel: exprAdd exprRelPrim
bool exprRel() {
  tklog("exprRel()");
  Token *start = iTk;
  if (exprAdd()) {
    if (exprRelPrim()) {
      return true;
    }
  }
  iTk = start;
  tklog("exprRel() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprRelPrim: (LESS | LESSEQ | GREATER | GREATEREQ) exprAdd exprRelPrim |
// epsilon
bool exprRelPrim() {
  tklog("exprRelPrim()");
  Token *start = iTk;
  if (consume(LESS)) {
    if (exprAdd()) {
      if (exprRelPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after < operator");
  } else if (consume(LESSEQ)) {
    if (exprAdd()) {
      if (exprRelPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after <= operator");
  } else if (consume(GREATER)) {
    if (exprAdd()) {
      if (exprRelPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after > operator");
  } else if (consume(GREATEREQ)) {
    if (exprAdd()) {
      if (exprRelPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after >= operator");
  }
  iTk = start;
  return true;
}

// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
// ----------------------------------------------- left recursive elimination
// exprAdd: exprMul exprAddPrim
bool exprAdd() {
  tklog("exprAdd()");
  Token *start = iTk;
  if (exprMul()) {
    if (exprAddPrim()) {
      return true;
    }
  }
  iTk = start;
  tklog("exprAdd() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprAddPrim: (ADD | SUB) exprMul exprAddPrim | epsilon
bool exprAddPrim() {
  tklog("exprAddPrim()");
  Token *start = iTk;
  if (consume(ADD)) {
    if (exprMul()) {
      if (exprAddPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after + operator");
  } else if (consume(SUB)) {
    if (exprMul()) {
      if (exprAddPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after - operator");
  }
  iTk = start;
  return true;
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
// ----------------------------------------------- left recursive elimination
// exprMul = exprCast exprMulPrim
bool exprMul() {
  tklog("exprMul()");
  Token *start = iTk;
  if (exprCast()) {
    if (exprMulPrim()) {
      return true;
    }
  } 
  iTk = start;
  tklog("exprMul() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprMulPrim = ( MUL | DIV ) exprCast exprMulPrim | epsilon
bool exprMulPrim() {
  tklog("exprMulPrim()");
  Token *start = iTk;
  if (consume(MUL)) {
    if (exprCast()) {
      if (exprMulPrim()) {
        return true;
      } 
    } else
      tkerr("Missing expression after * operator");
  } else if (consume(DIV)) {
    if (exprCast()) {
      if (exprMulPrim()) {
        return true;
      }
    } else
      tkerr("Missing expression after / operator");
  }
  iTk = start;
  return true;
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast() {
  tklog("exprCast()");
  Token *start = iTk;
  if (consume(LPAR)) {
    if (typeBase()) {
      if (arrayDecl()) {
        if (consume(RPAR)) {
          if (exprCast()) {
            return true;
          }
        } else
          tkerr("Missing ) in cast expression");
      }
      if (consume(RPAR)) {
        if (exprCast()) {
          return true;
        }
      }
    } else
      tkerr("Missing or wrong type in cast expression");
  }
  if (exprUnary()) {
    return true;
  }
  iTk = start;
  tklog("exprCast() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary() {
  tklog("exprUnary()");
  Token *start = iTk;
  if (consume(SUB)) {
    if (exprUnary()) {
      return true;
    } else
      tkerr("Missing expression after - operator");
  } else if (consume(NOT)) {
    if (exprUnary()) {
      return true;
    } else
      tkerr("Missing expression after ! operator");
  }
  if (exprPostfix()) {
    return true;
  }
  iTk = start;
  tklog("exprUnary() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprPostfix: exprPostfix LBRACKET expr RBRACKET
//  | exprPostfix DOT ID
//  | exprPrimary
// ----------------------------------------------- left recursive elimination
// exprPostfix = exprPrimary exprPosfixPrim
bool exprPostfix() {
  tklog("exprPostfix()");
  Token *start = iTk;
  if (exprPrimary()) {
    if (exprPostfixPrim())
      return true;
  }
  iTk = start;
  tklog("exprPostfix() failed on token %s", getTokenType(iTk->code));
  ;
  return false;
}

// exprPostfixPrim: LBRACKET expr RBRACKET exprPostfixPrim
//                | DOT ID exprPostfixPrim
//                | epsilon
bool exprPostfixPrim() {
  tklog("exprPostfixPrim()");
  if (consume(LBRACKET)) { // LBRACKET expr RBRACKET exprPostfixPrim
    if (expr()) {
      if (consume(RBRACKET)) {
        if (exprPostfixPrim()) {
          return true;
        }
      } else
        tkerr("Missing ] in array access");
    } else
      tkerr("Missing expression in array access");
  }
  if (consume(DOT)) { // DOT ID exprPostfixPrim
    if (consume(ID)) {
      if (exprPostfixPrim()) {
        return true;
      }
    } else
      tkerr("Missing identifier after . operator");
  }
  return true; // epsilon
}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
//  | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary() {
  tklog("exprPrimary()");
  Token *start = iTk;
  if (consume(ID)) {
    if (consume(LPAR)) {
      if (expr()) {
        for (;;) {
          if (consume(COMMA)) {
            if (expr()) {
            } else {
              tkerr("Missing expression after , in function call");
              break;
            }
          } else
            break;
        }
      }
      if (consume(RPAR)) {
        return true;
      } else
        tkerr("Missing ) in function call");
    } 
    return true;
  }
  if (consume(INT)) {
    return true;
  } else if (consume(DOUBLE)) {
    return true;
  } else if (consume(CHAR)) {
    return true;
  } else if (consume(STRING)) {
    return true;
  }
  if (consume(LPAR)) {
    if (expr()) {
      if (consume(RPAR)) {
        return true;
      } else
        tkerr("Missing ) in expression");
    }
  }
  iTk = start;
  tklog("exprPrimary() failed on token %s", getTokenType(iTk->code));
  return false;
}

// unit: ( structDef | fnDef | varDef )* END
bool unit() {
  tklog("unit()");
  for (;;) {
    if (structDef()) {
    } else if (fnDef()) {
    } else if (varDef()) {
    } else
      break;
  }
  if (consume(END)) {
    return true;
  } else
    tkerr("Syntax error");
  tklog("unit() failed on token %s", getTokenType(iTk->code));
  return false;
}

void parse(Token *tokens) {
  iTk = tokens;
  if (!unit())
    tkerr("Syntax error");
}
