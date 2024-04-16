#pragma once

#include <stdbool.h>
#include "lexer.h"
#include "ad.h"

bool unit();
bool structDef();
bool fnDef();
bool varDef();
bool typeBase(Type *);
bool arrayDecl(Type *);
bool fnParam();
bool stm();
bool stmCompound(bool);
bool expr();
bool exprAssign();
bool exprOr();
bool exprOrPrim();
bool exprAnd();
bool exprAndPrim();
bool exprEq();
bool exprEqPrim();
bool exprRel();
bool exprRelPrim();
bool exprAdd();
bool exprAddPrim();
bool exprMul();
bool exprMulPrim();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPostfixPrim();
bool exprPrimary();
void parse(Token *);
