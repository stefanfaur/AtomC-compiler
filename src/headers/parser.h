#pragma once

#include <stdbool.h>
#include "lexer.h"
#include "ad.h"
#include "at.h"

bool unit();
bool structDef();
bool fnDef();
bool varDef();
bool typeBase(Type *);
bool arrayDecl(Type *);
bool fnParam();
bool stm();
bool stmCompound(bool);
bool expr(Ret *r);
bool exprAssign(Ret *r);
bool exprOr(Ret *r);
bool exprOrPrim(Ret *r);
bool exprAnd(Ret *r);
bool exprAndPrim(Ret *r);
bool exprEq(Ret *r);
bool exprEqPrim(Ret *r);
bool exprRel(Ret *r);
bool exprRelPrim(Ret *r);
bool exprAdd(Ret *r);
bool exprAddPrim(Ret *r);
bool exprMul(Ret *r);
bool exprMulPrim(Ret *r);
bool exprCast(Ret *r);
bool exprUnary(Ret *r);
bool exprPostfix(Ret *r);
bool exprPostfixPrim(Ret *r);
bool exprPrimary(Ret *r);
void parse(Token *);
