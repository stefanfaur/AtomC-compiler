#include "headers/parser.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "headers/ad.h"
#include "headers/at.h"
#include "headers/utils.h"
#include "headers/vm.h"
#include "headers/gc.h"

#define SILENCE_LOGS 1

Token *iTk;            // the iterator in the tokens list
Token *consumedTk;     // the last consumed token
Symbol *owner = NULL;  // the current owner of the symbols

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
    fputc(' ', stderr);  // align
  }
  fputc('^', stderr);  // point to the error

  fprintf(stderr, "\n");
  exit(EXIT_FAILURE);
}

void tklog(const char *fmt, ...) {
  if (SILENCE_LOGS) return;
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
bool typeBase(Type *t) {
  t->n = -1;
  tklog("typeBase()");
  Token *start = iTk;
  if (consume(TYPE_INT)) {
    t->tb = TB_INT;
    return true;
  }
  if (consume(TYPE_DOUBLE)) {
    t->tb = TB_DOUBLE;
    return true;
  }
  if (consume(TYPE_CHAR)) {
    t->tb = TB_CHAR;
    return true;
  }
  if (consume(STRUCT)) {
    if (consume(ID)) {
      Token *tkName = consumedTk;
      t->tb = TB_STRUCT;
      t->s = findSymbol(tkName->text);
      if (!t->s) tkerr("structura nedefinita: %s", tkName->text);
      return true;
    } else
      tkerr("Missing identifier after 'struct' keyword");
  }
  iTk = start;
  tklog("typeBase() failed on token %s", getTokenType(iTk->code));
  return false;
}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl(Type *t) {
  tklog("arrayDecl()");
  Token *start = iTk;
  if (consume(LBRACKET)) {
    if (consume(INT)) {
      Token *tkSize = consumedTk;
      t->n = tkSize->i;
    } else {
      t->n = 0;  // array without specified dimension
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
  Type t;
  if (typeBase(&t)) {
    if (consume(ID)) {
      Token *tkName = consumedTk;
      if (arrayDecl(&t)) {
        if (t.n == 0) {
          tkerr("Array variable must have a specified dimension");
        }
        if (consume(SEMICOLON)) {
          Symbol *var = findSymbolInDomain(symTable, tkName->text);
          if (var) tkerr("symbol redefinition: %s", tkName->text);
          var = newSymbol(tkName->text, SK_VAR);
          var->type = t;
          var->owner = owner;
          addSymbolToDomain(symTable, var);
          if (owner) {  // local variable
            switch (owner->kind) {
              case SK_FN:
                var->varIdx = symbolsLen(owner->fn.locals);
                addSymbolToList(&owner->fn.locals, dupSymbol(var));
                break;
              case SK_STRUCT:
                var->varIdx = typeSize(&owner->type);
                addSymbolToList(&owner->structMembers, dupSymbol(var));
                break;
            }
          } else {  // global variable
            var->varMem = safeAlloc(typeSize(&t));
          }
          return true;
        } else
          tkerr("Missing ; in variable declaration");
      }
      if (consume(SEMICOLON)) {
        Symbol *var = findSymbolInDomain(symTable, tkName->text);
        if (var) tkerr("symbol redefinition: %s", tkName->text);
        var = newSymbol(tkName->text, SK_VAR);
        var->type = t;
        var->owner = owner;
        addSymbolToDomain(symTable, var);
        if (owner) {
          switch (owner->kind) {
            case SK_FN:
              var->varIdx = symbolsLen(owner->fn.locals);
              addSymbolToList(&owner->fn.locals, dupSymbol(var));
              break;
            case SK_STRUCT:
              var->varIdx = typeSize(&owner->type);
              addSymbolToList(&owner->structMembers, dupSymbol(var));
              break;
          }
        } else {
          var->varMem = safeAlloc(typeSize(&t));
        }
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
      Token *tkName = consumedTk;
      if (consume(LACC)) {
        Symbol *s = findSymbolInDomain(symTable, tkName->text);
        if (s) tkerr("symbol redefinition: %s", tkName->text);
        s = addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
        s->type.tb = TB_STRUCT;
        s->type.s = s;
        s->type.n = -1;
        pushDomain();
        owner = s;
        for (;;) {
          if (varDef()) {
          } else
            break;
        }
        if (consume(RACC)) {
          if (consume(SEMICOLON)) {
            owner = NULL;
            dropDomain();
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
  Type t;
  Token *start = iTk;
  if (typeBase(&t)) {
    if (consume(ID)) {
      Token *tkName = consumedTk;
      if (arrayDecl(&t)) {
        t.n = 0;  // array without specified dimension
        // return true;
      }
      Symbol *param = findSymbolInDomain(symTable, tkName->text);
      if (param) tkerr("Symbol has already been defined: %s", tkName->text);
      param = newSymbol(tkName->text, SK_PARAM);
      param->type = t;
      param->owner = owner;
      param->paramIdx = symbolsLen(owner->fn.params);
      addSymbolToDomain(symTable, param);
      addSymbolToList(&owner->fn.params, dupSymbol(param));
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
  Type t;
  tklog("fnDef()");
  Token *start = iTk;
  Instr *startInstr =
      owner ? lastInstr(owner->fn.instr)
            : NULL;  // Save the start instruction for the function

  if (consume(VOID)) {
    t.n = -1;
    t.tb = TB_VOID;
    if (consume(ID)) {
      Token *tkName = consumedTk;
      if (consume(LPAR)) {
        Symbol *fn = findSymbolInDomain(symTable, tkName->text);
        if (fn) tkerr("symbol redefinition: %s", tkName->text);
        fn = newSymbol(tkName->text, SK_FN);
        fn->type = t;
        addSymbolToDomain(symTable, fn);
        owner = fn;
        pushDomain();
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
          addInstr(&fn->fn.instr, OP_ENTER);
          if (stmCompound(false)) {
            fn->fn.instr->arg.i = symbolsLen(fn->fn.locals);
            if (fn->type.tb == TB_VOID) {
              addInstrWithInt(&fn->fn.instr, OP_RET_VOID,
                              symbolsLen(fn->fn.params));
            }
            dropDomain();
            owner = NULL;
            return true;
          }
        }
      } else
        tkerr("Missing ( in function definition");
    } else
      tkerr("Missing identifier in function definition");
  } else if (typeBase(&t)) {
    if (consume(ID)) {
      Token *tkName = consumedTk;
      if (consume(LPAR)) {
        Symbol *fn = findSymbolInDomain(symTable, tkName->text);
        if (fn) tkerr("symbol redefinition: %s", tkName->text);
        fn = newSymbol(tkName->text, SK_FN);
        fn->type = t;
        addSymbolToDomain(symTable, fn);
        owner = fn;
        pushDomain();
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
          addInstr(&fn->fn.instr, OP_ENTER);
          if (stmCompound(false)) {
            fn->fn.instr->arg.i = symbolsLen(fn->fn.locals);
            if (fn->type.tb == TB_VOID) {
              addInstrWithInt(&fn->fn.instr, OP_RET_VOID,
                              symbolsLen(fn->fn.params));
            }
            dropDomain();
            owner = NULL;
            return true;
          }
        } else
          tkerr("Missing ) in function definition");
      }
    } else
      tkerr("Missing identifier in function definition");
  }
  iTk = start;
  if (owner) {
    delInstrAfter(startInstr);
  }
  tklog("fnDef() failed on token %s", getTokenType(iTk->code));
  return false;
}

// stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound(bool newDomain) {
  tklog("stmCompound()");
  Token *start = iTk;
  if (consume(LACC)) {
    if (newDomain) pushDomain();
    for (;;) {
      if (varDef()) {
      } else if (stm()) {
      } else
        break;
    }
    if (consume(RACC)) {
      if (newDomain) dropDomain();
      return true;
    } else
      tkerr("Missing } in compound statement");
  }
  iTk = start;
  tklog("stmCompound() failed on token %s", getTokenType(iTk->code));
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
  Ret rCond, rExpr;  // Structures to hold type analysis results for expressions
  Instr *startInstr =
      owner ? lastInstr(owner->fn.instr)
            : NULL;  // Save the start instruction for the function

  // stmCompound
  if (stmCompound(true)) {
    return true;
  }

  // | IF LPAR expr RPAR stm ( ELSE stm )?
  if (consume(IF)) {
    if (consume(LPAR)) {
      if (expr(&rCond)) {
        if (!canBeScalar(&rCond)) {
          tkerr("the if condition must be a scalar value");
        }
        if (consume(RPAR)) {
          addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
          Type intType = {TB_INT, NULL, -1};
          insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
          Instr *ifJF = addInstr(&owner->fn.instr, OP_JF);

          if (stm()) {
            if (consume(ELSE)) {
              Instr *ifJMP = addInstr(&owner->fn.instr, OP_JMP);
              ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
              if (stm()) {
                ifJMP->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
                return true;
              } else
                tkerr("Missing statement in else branch");
            } else {
              ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
            }
            return true;
          } else
            tkerr("Missing statement in if statement");
        } else
          tkerr("Missing ) in if statement");
      } else
        tkerr("Missing expression in if statement");
    } else
      tkerr("Missing ( in if statement");
  }

  // | WHILE LPAR expr RPAR stm
  if (consume(WHILE)) {
    Instr *beforeWhileCond = lastInstr(owner->fn.instr);
    if (consume(LPAR)) {
      if (expr(&rCond)) {
        if (!canBeScalar(&rCond)) {
          tkerr("the while condition must be a scalar value");
        }
        if (consume(RPAR)) {
          addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
          Type intType = {TB_INT, NULL, -1};
          insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
          Instr *whileJF = addInstr(&owner->fn.instr, OP_JF);

          if (stm()) {
            addInstr(&owner->fn.instr, OP_JMP)->arg.instr =
                beforeWhileCond->next;
            whileJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
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
    if (expr(&rExpr)) {
      if (owner->type.tb == TB_VOID) {
        tkerr("a void function cannot return a value");
      }
      if (!canBeScalar(&rExpr)) {
        tkerr("the return value must be a scalar value");
      }
      if (!convTo(&rExpr.type, &owner->type)) {
        tkerr(
            "cannot convert the return expression type to the function "
            "return type");
      }

      addRVal(&owner->fn.instr, rExpr.lval,
              &rExpr.type);  // Add the return value to the stack
      insertConvIfNeeded(lastInstr(owner->fn.instr), &rExpr.type,
                         &owner->type);  /// Convert the return value to the
                                         /// function return type
      addInstrWithInt(
          &owner->fn.instr, OP_RET,
          symbolsLen(owner->fn.params));  // Add the return instruction

      if (consume(SEMICOLON)) {
        return true;
      } else
        tkerr("Missing ; in return statement");
    }
    if (owner->type.tb != TB_VOID) {
      tkerr("a non-void function must return a value");
    }
    addInstr(&owner->fn.instr, OP_RET_VOID);
    if (consume(SEMICOLON)) {
      return true;
    }
  }

  // | expr? SEMICOLON
  if (expr(&rExpr)) {
    if (rExpr.type.tb != TB_VOID) addInstr(&owner->fn.instr, OP_DROP);
    if (consume(SEMICOLON)) {
      return true;
    } else
      tkerr("Missing ; in expression statement");
  } else if (consume(SEMICOLON)) {
    return true;
  }

  iTk = start;
  tklog("stm() failed on token %s", getTokenType(iTk->code));
  if (owner) {
    delInstrAfter(startInstr);
  }
  return false;
}

// expr: exprAssign
bool expr(Ret *r) {
  Token *start = iTk;
  tklog("expr()");
  if (exprAssign(r)) {
    return true;
  }
  tklog("expr() failed on token %s", getTokenType(iTk->code));
  iTk = start;
  return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign(Ret *r) {
  tklog("exprAssign()");
  Token *start = iTk;
  Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
  Ret rDst;

  if (exprUnary(&rDst)) {
    if (consume(ASSIGN)) {
      if (exprAssign(r)) {
        if (!rDst.lval) {
          tkerr("the assign destination must be a left-value");
        }
        if (rDst.ct) {
          tkerr("the assign destination cannot be constant");
        }
        if (!canBeScalar(&rDst)) {
          tkerr("the assign destination must be scalar");
        }
        if (!canBeScalar(r)) {
          tkerr("the assign source must be scalar");
        }
        if (!convTo(&r->type, &rDst.type)) {
          tkerr("the assign source cannot be converted to destination");
        }
        r->lval = false;  // The result of an assignment is not a left-value
        r->ct = true;  // The result of an assignment is a constant expression

        addRVal(&owner->fn.instr, r->lval, &r->type);
        insertConvIfNeeded(lastInstr(owner->fn.instr), &r->type, &rDst.type);
        switch (rDst.type.tb) {
          case TB_INT:
            addInstr(&owner->fn.instr, OP_STORE_I);
            break;
          case TB_DOUBLE:
            addInstr(&owner->fn.instr, OP_STORE_F);
            break;
        }
        return true;

      } else {
        tkerr("Missing expression after = operator");
      }
    }
  }
  iTk = start;  // Backtrack if the first part fails

  if (owner) {
    delInstrAfter(startInstr);
  }

  if (exprOr(
          r)) {  // Evaluate the 'exprOr' rule if the assignment does not parse
    return true;
  }

  iTk = start;  // Backtrack if 'exprOr' also fails
  tklog("exprAssign() failed on token %s", getTokenType(iTk->code));
  if (owner) {
    delInstrAfter(startInstr);
  }
  return false;
}

// exprOr: exprOr OR exprAnd | exprAnd
// ----------------------------------------------- left recursive elimination
// exprOr: exprAnd exprOrPrim
bool exprOr(Ret *r) {
  tklog("exprOr()");
  Token *start = iTk;

  if (exprAnd(r)) {
    if (exprOrPrim(r)) {
      return true;
    }
  }
  iTk = start;
  tklog("exprOr() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprOrPrim: OR exprAnd exprOrPrim | epsilon
bool exprOrPrim(Ret *r) {
  tklog("exprOrPrim()");
  Token *start = iTk;

  if (consume(OR)) {
    Ret right;
    if (exprAnd(&right)) {
      Type tDst;
      if (!arithTypeTo(&r->type, &right.type, &tDst)) {
        tkerr("Invalid operand type for ||");
      }
      *r = (Ret){{TB_INT, NULL, -1}, false, true};
      if (exprOrPrim(r)) {
        return true;
      }
    } else {
      tkerr("Missing expression after || operator");
    }
  }

  iTk = start;
  return true;  // epsilon
}

// exprAnd: exprAnd AND exprEq | exprEq
// ----------------------------------------------- left recursive elimination
// exprAnd -> exprEq exprAndPrim
bool exprAnd(Ret *r) {
  tklog("exprAnd()");
  Token *start = iTk;

  if (exprEq(r)) {
    if (exprAndPrim(r)) {
      return true;
    }
  }
  iTk = start;
  tklog("exprAnd() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprAndPrim -> AND exprEq exprAndPrim | epsilon
bool exprAndPrim(Ret *r) {
  tklog("exprAndPrim()");
  Token *start = iTk;

  if (consume(AND)) {
    Ret right;
    if (exprEq(&right)) {
      Type tDst;
      if (!arithTypeTo(&r->type, &right.type, &tDst)) {
        tkerr("invalid operand type for &&");
      }
      *r = (Ret){{TB_INT, NULL, -1}, false, true};
      if (exprAndPrim(r)) {
        return true;
      } else {
        tkerr("Invalid expression after == operator");
      }
    } else {
      tkerr("Missing expression after && operator");
    }
  }

  iTk = start;
  return true;  // epsilon
}

// exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
// ----------------------------------------------- left recursive elimination
// exprEq = exprRel exprEqPrim
bool exprEq(Ret *r) {
  tklog("exprEq()");
  Token *start = iTk;

  if (exprRel(r)) {
    if (exprEqPrim(r)) {
      return true;
    }
  }
  iTk = start;
  tklog("exprEq() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprEqPrim = ( EQUAL | NOTEQ ) exprRel exprEqPrim | epsilon
bool exprEqPrim(Ret *r) {
  tklog("exprEqPrim()");
  Token *start = iTk;

  while (true) {
    if (consume(EQUAL) || consume(NOTEQ)) {
      Ret right;
      if (exprRel(&right)) {
        Type tDst;
        if (!arithTypeTo(&r->type, &right.type, &tDst)) {
          tkerr("Invalid operand type for == or !=");
        }
        if (exprEqPrim(r)) {
          return true;
        }
      } else {
        tkerr("Missing expression after == or != operator");
      }
    } else {
      break;
    }
  }

  iTk = start;
  return true;  // Return true for epsilon
                // completion
}

// exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
// ----------------------------------------------- left recursive elimination
// exprRel: exprAdd exprRelPrim
bool exprRel(Ret *r) {
  tklog("exprRel()");
  Token *start = iTk;
  Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;

  if (exprAdd(r)) {
    if (exprRelPrim(r)) {
      return true;
    }
  }
  iTk = start;
  tklog("exprRel() failed on token %s", getTokenType(iTk->code));
  if (owner) {
    delInstrAfter(startInstr);
  }
  return false;
}

// exprRelPrim: (LESS | LESSEQ | GREATER | GREATEREQ) exprAdd exprRelPrim |
// epsilon
bool exprRelPrim(Ret *r) {
  tklog("exprRelPrim()");
  Token *start = iTk;
  Token *op;

  if (consume(LESS)) {
    Ret right;

    op = consumedTk;
    Instr *lastLeft = lastInstr(owner->fn.instr);
    addRVal(&owner->fn.instr, r->lval, &r->type);

    if (exprAdd(&right)) {
      Type tDst;
      if (!arithTypeTo(&r->type, &right.type, &tDst))
        tkerr("invalid operand type for <");

      addRVal(&owner->fn.instr, right.lval, &right.type);
      insertConvIfNeeded(lastLeft, &r->type, &tDst);
      insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
      switch (op->code) {
        case LESS:
          switch (tDst.tb) {
            case TB_INT:
              addInstr(&owner->fn.instr, OP_LESS_I);
              break;
            case TB_DOUBLE:
              addInstr(&owner->fn.instr, OP_LESS_F);
              break;
          }
          break;
      }

      *r = (Ret){{TB_INT, NULL, -1}, false, true};
      if (exprRelPrim(r)) {
        return true;
      }
    } else
      tkerr("Missing expression after < operator");
  } else if (consume(LESSEQ)) {
    Ret right;
    if (exprAdd(&right)) {
      Type tDst;
      if (!arithTypeTo(&r->type, &right.type, &tDst))
        tkerr("invalid operand type for <=");
      *r = (Ret){{TB_INT, NULL, -1}, false, true};
      if (exprRelPrim(r)) {
        return true;
      }
    } else
      tkerr("Missing expression after <= operator");
  } else if (consume(GREATER)) {
    Ret right;
    if (exprAdd(&right)) {
      Type tDst;
      if (!arithTypeTo(&r->type, &right.type, &tDst))
        tkerr("invalid operand type for >");
      *r = (Ret){{TB_INT, NULL, -1}, false, true};
      if (exprRelPrim(r)) {
        return true;
      }
    } else
      tkerr("Missing expression after > operator");
  } else if (consume(GREATEREQ)) {
    Ret right;
    if (exprAdd(&right)) {
      Type tDst;
      if (!arithTypeTo(&r->type, &right.type, &tDst))
        tkerr("invalid operand type for >=");
      *r = (Ret){{TB_INT, NULL, -1}, false, true};
      if (exprRelPrim(r)) {
        return true;
      }
    } else
      tkerr("Missing expression after >= operator");
  }

  iTk = start;
  return true;  // Return true for epsilon
                // completion
}

// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
// ----------------------------------------------- left recursive elimination
// exprAdd: exprMul exprAddPrim
bool exprAdd(Ret *r) {
  tklog("exprAdd()");
  Token *start = iTk;
  Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;

  if (exprMul(r)) {
    if (exprAddPrim(r)) {
      return true;
    }
  }
  iTk = start;
  tklog("exprAdd() failed on token %s", getTokenType(iTk->code));
  if (owner) {
    delInstrAfter(startInstr);
  }
  return false;
}

// exprAddPrim: (ADD | SUB) exprMul exprAddPrim | epsilon
bool exprAddPrim(Ret *r) {
  tklog("exprAddPrim()");
  Token *start = iTk;

  if (consume(ADD)) {
    Ret right;

    Token *op = consumedTk;
    Instr *lastLeft = lastInstr(owner->fn.instr);
    addRVal(&owner->fn.instr, r->lval, &r->type);

    if (exprMul(&right)) {
      Type tDst;
      if (!arithTypeTo(&r->type, &right.type, &tDst))
        tkerr("invalid operand type for +");
      addRVal(&owner->fn.instr, right.lval, &right.type);
      insertConvIfNeeded(lastLeft, &r->type, &tDst);
      insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
      switch (op->code) {
        case ADD:
          switch (tDst.tb) {
            case TB_INT:
              addInstr(&owner->fn.instr, OP_ADD_I);
              break;
            case TB_DOUBLE:
              addInstr(&owner->fn.instr, OP_ADD_F);
              break;
          }
          break;
      }
      *r = (Ret){tDst, false, true};
      if (exprAddPrim(r)) {
        return true;
      }
    } else
      tkerr("Missing expression after + operator");
  } else if (consume(SUB)) {
    Ret right;

    Token *op = consumedTk;
    Instr *lastLeft = lastInstr(owner->fn.instr);
    addRVal(&owner->fn.instr, r->lval, &r->type);

    if (exprMul(&right)) {
      Type tDst;
      if (!arithTypeTo(&r->type, &right.type, &tDst))
        tkerr("invalid operand type for -");
      addRVal(&owner->fn.instr, right.lval, &right.type);
      insertConvIfNeeded(lastLeft, &r->type, &tDst);
      insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
      switch (op->code) {
        case SUB:
          switch (tDst.tb) {
            case TB_INT:
              addInstr(&owner->fn.instr, OP_SUB_I);
              break;
            case TB_DOUBLE:
              addInstr(&owner->fn.instr, OP_SUB_F);
              break;
          }
          break;
      }
      *r = (Ret){tDst, false, true};
      if (exprAddPrim(r)) {
        return true;
      }
    } else
      tkerr("Missing expression after - operator");
  }

  iTk = start;
  return true;  // epsilon
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
// ----------------------------------------------- left recursive elimination
// exprMul = exprCast exprMulPrim
bool exprMul(Ret *r) {
  tklog("exprMul()");
  Token *start = iTk;
  Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;

  if (exprCast(r)) {
    if (exprMulPrim(r)) {
      return true;
    }
  }
  iTk = start;
  tklog("exprMul() failed on token %s", getTokenType(iTk->code));
  if (owner) {
    delInstrAfter(startInstr);
  }
  return false;
}

// exprMulPrim = ( MUL | DIV ) exprCast exprMulPrim | epsilon
bool exprMulPrim(Ret *r) {
  tklog("exprMulPrim()");
  Token *start = iTk;

  if (consume(MUL)) {
    Ret right;

    Token *op = consumedTk;
    Instr *lastLeft = lastInstr(owner->fn.instr);
    addRVal(&owner->fn.instr, r->lval, &r->type);

    if (exprCast(&right)) {
      Type tDst;
      if (!arithTypeTo(&r->type, &right.type, &tDst))
        tkerr("invalid operand type for *");

      addRVal(&owner->fn.instr, right.lval, &right.type);
      insertConvIfNeeded(lastLeft, &r->type, &tDst);
      insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
      switch (op->code) {
        case MUL:
          switch (tDst.tb) {
            case TB_INT:
              addInstr(&owner->fn.instr, OP_MUL_I);
              break;
            case TB_DOUBLE:
              addInstr(&owner->fn.instr, OP_MUL_F);
              break;
          }
          break;
      }

      *r = (Ret){tDst, false, true};
      if (exprMulPrim(r)) {
        return true;
      }
    } else
      tkerr("Missing expression after * operator");
  } else if (consume(DIV)) {
    Ret right;

    Token *op = consumedTk;
    Instr *lastLeft = lastInstr(owner->fn.instr);
    addRVal(&owner->fn.instr, r->lval, &r->type);

    if (exprCast(&right)) {
      Type tDst;
      if (!arithTypeTo(&r->type, &right.type, &tDst))
        tkerr("invalid operand type for /");

      addRVal(&owner->fn.instr, right.lval, &right.type);
      insertConvIfNeeded(lastLeft, &r->type, &tDst);
      insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
      switch (op->code) {
        case DIV:
          switch (tDst.tb) {
            case TB_INT:
              addInstr(&owner->fn.instr, OP_DIV_I);
              break;
            case TB_DOUBLE:
              addInstr(&owner->fn.instr, OP_DIV_F);
              break;
          }
          break;
      }

      *r = (Ret){tDst, false, true};
      if (exprMulPrim(r)) {
        return true;
      }
    } else
      tkerr("Missing expression after / operator");
  }

  iTk = start;
  return true;  // epsilon
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(Ret *r) {
  tklog("exprCast()");
  Token *start = iTk;

  if (consume(LPAR)) {
    Type t;
    Ret op;
    if (typeBase(&t)) {
      if (arrayDecl(&t)) {
      }
      if (consume(RPAR)) {
        if (exprCast(&op)) {
          if (t.tb == TB_STRUCT) {
            tkerr("Cannot convert to a struct type");
          }
          if (op.type.tb == TB_STRUCT) {
            tkerr("Cannot convert a struct");
          }
          if (op.type.n >= 0 && t.n < 0) {
            tkerr("An array can be converted only to another array");
          }
          if (op.type.n < 0 && t.n >= 0) {
            tkerr("A scalar can be converted only to another scalar");
          }
          *r = (Ret){t, false, true};
          return true;
        } else {
          tkerr("Invalid expression after cast");
        }
      } else {
        tkerr("Missing ) after cast");
      }
    }
  }

  if (exprUnary(r)) {
    return true;
  }

  iTk = start;
  tklog("exprCast() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary(Ret *r) {
  tklog("exprUnary()");
  Token *start = iTk;

  if (consume(SUB)) {
    if (exprUnary(r)) {
      // Check if the operand is scalar for unary minus
      if (!canBeScalar(r)) {
        tkerr("unary - must have a scalar operand");
      }
      r->lval = false;
      r->ct = true;
      return true;
    } else {
      tkerr("Missing expression after - operator");
    }
  } else if (consume(NOT)) {
    if (exprUnary(r)) {
      // Check if the operand is scalar for logical NOT
      if (!canBeScalar(r)) {
        tkerr("unary ! must have a scalar operand");
      }
      r->type = (Type){TB_INT, NULL, -1};  // is always int
      r->lval = false;                     // is not an l-value
      r->ct = true;                        // is a constant expression
      return true;
    } else {
      tkerr("Missing expression after ! operator");
    }
  }

  iTk = start;
  if (exprPostfix(r)) {
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
bool exprPostfix(Ret *r) {
  tklog("exprPostfix()");
  Token *start = iTk;
  if (exprPrimary(r)) {
    if (exprPostfixPrim(r)) {
      return true;
    }
  }
  iTk = start;
  tklog("exprPostfix() failed on token %s", getTokenType(iTk->code));
  return false;
}

// exprPostfixPrim: LBRACKET expr RBRACKET exprPostfixPrim
//                | DOT ID exprPostfixPrim
//                | epsilon
bool exprPostfixPrim(Ret *r) {
  Token *start = iTk;

  if (consume(LBRACKET)) {
    Ret idx;
    if (expr(&idx)) {
      if (consume(RBRACKET)) {
        if (r->type.n < 0) {
          tkerr("only an array can be indexed");
        }
        Type tInt = {TB_INT, NULL, -1};
        if (!convTo(&idx.type, &tInt)) {
          tkerr("the index is not convertible to int");
        }
        r->type.n = -1;
        r->lval = true;
        r->ct = false;
        if (exprPostfixPrim(r)) {
          return true;
        } else {
          tkerr("Invalid expression after ]");
        }
      } else {
        tkerr("Missing ] after expression");
      }
    } else {
      tkerr("Missing [ after expression]");
    }
  } else if (consume(DOT)) {
    if (consume(ID)) {
      Token *tkName = consumedTk;
      if (r->type.tb != TB_STRUCT) {
        tkerr("a field can only be selected from a struct");
      }
      Symbol *s = findSymbolInList(r->type.s->structMembers, tkName->text);
      if (!s) {
        tkerr("the structure %s does not have a field %s", r->type.s->name,
              tkName->text);
      }
      *r = (Ret){s->type, true, s->type.n >= 0};
      if (exprPostfixPrim(r)) {
        return true;
      } else {
        tkerr("Invalid expression after variable name");
      }
    } else {
      tkerr("Missing identifier after . ");
    }
  }

  iTk = start;

  return true;
}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
//  | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary(Ret *r) {
  tklog("exprPrimary()");
  Token *start = iTk;
  Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
  Ret rArg;

  if (consume(ID)) {
    Token *tkName = consumedTk;
    Symbol *s = findSymbol(tkName->text);
    if (!s) tkerr("undefined id: %s", tkName->text);
    if (consume(LPAR)) {
      if (s->kind != SK_FN) tkerr("only a function can be called");
      Symbol *param = s->fn.params;
      if (expr(&rArg)) {
        if (!param) {
          tkerr("too many arguments in function call");
        }
        if (!convTo(&rArg.type, &param->type)) {
          tkerr(
              "in call, cannot convert the argument type to the parameter "
              "type");
        }

        addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
        insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type,
                           &param->type);

        param = param->next;
        for (;;) {
          if (consume(COMMA)) {
            if (expr(&rArg)) {
              if (!param) {
                tkerr("too many arguments in function call");
              }
              if (!convTo(&rArg.type, &param->type)) {
                tkerr(
                    "in call, cannot convert the argument type to the "
                    "parameter type");
              }

              addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
              insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type,
                                 &param->type);

              param = param->next;
            } else {
              tkerr("Missing expression after , in function call");
              break;
            }
          } else
            break;
        }
      }
      if (consume(RPAR)) {
        if (param) {
          tkerr("too few arguments in function call");
        }
        *r = (Ret){s->type, false, true};

        if (s->fn.extFnPtr) {
          addInstr(&owner->fn.instr, OP_CALL_EXT)->arg.extFnPtr =
              s->fn.extFnPtr;
        } else {
          addInstr(&owner->fn.instr, OP_CALL)->arg.instr = s->fn.instr;
        }

        return true;
      } else
        tkerr("Missing ) in function call");
    } else {
      if (s->kind == SK_FN) {
        tkerr("a function can only be called");
      }
      *r = (Ret){s->type, true, s->type.n >= 0};
      if (s->kind == SK_VAR) {
        if (s->owner == NULL) {  // global variables
          addInstr(&owner->fn.instr, OP_ADDR)->arg.p = s->varMem;
        } else {  // local variables
          switch (s->type.tb) {
            case TB_INT:
              addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->varIdx + 1);
              break;
            case TB_DOUBLE:
              addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->varIdx + 1);
              break;
          }
        }
      }
      if (s->kind == SK_PARAM) {
        switch (s->type.tb) {
          case TB_INT:
            addInstrWithInt(&owner->fn.instr, OP_FPADDR_I,
                            s->paramIdx - symbolsLen(s->owner->fn.params) - 1);
            break;
          case TB_DOUBLE:
            addInstrWithInt(&owner->fn.instr, OP_FPADDR_F,
                            s->paramIdx - symbolsLen(s->owner->fn.params) - 1);
            break;
        }
      }
    }
    return true;
  }
  if (consume(INT)) {
    *r = (Ret){{TB_INT, NULL, -1}, false, true};

    Token *ct = consumedTk;
    addInstrWithInt(&owner->fn.instr, OP_PUSH_I, ct->i);

    return true;
  } else if (consume(DOUBLE)) {
    *r = (Ret){{TB_DOUBLE, NULL, -1}, false, true};

    Token *ct = consumedTk;
    addInstrWithDouble(&owner->fn.instr, OP_PUSH_F, ct->d);

    return true;
  } else if (consume(CHAR)) {
    *r = (Ret){{TB_CHAR, NULL, -1}, false, true};
    return true;
  } else if (consume(STRING)) {
    *r = (Ret){{TB_CHAR, NULL, 0}, false, true};
    return true;
  }
  if (consume(LPAR)) {
    if (expr(r)) {
      if (consume(RPAR)) {
        return true;
      } else
        tkerr("Missing ) in expression");
    }
  }

  iTk = start;
  if (owner) {
    delInstrAfter(startInstr);
  }
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
  if (!unit()) tkerr("Syntax error");
}
