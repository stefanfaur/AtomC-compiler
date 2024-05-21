#include <stdio.h>

#include "ad.h"
#include "utils.h"


Instr *addInstr(Instr **list, Opcode op) {
  Instr *i = (Instr *)safeAlloc(sizeof(Instr));
  i->op = op;
  i->next = NULL;
  if (*list) {
    Instr *p = *list;
    while (p->next)
      p = p->next;
    p->next = i;
  } else {
    *list = i;
  }
  return i;
}

Instr *addInstrWithInt(Instr **list, Opcode op, int argVal) {
  Instr *i = addInstr(list, op);
  i->arg.i = argVal;
  return i;
}

Instr *addInstrWithDouble(Instr **list, Opcode op, double argVal) {
  Instr *i = addInstr(list, op);
  i->arg.f = argVal;
  return i;
}

Val stack[10000];    // the stack
Val *SP = stack - 1; // Stack pointer - the stack's top - points to the value
                     // from the top of the stack
Val *FP = NULL;      // the initial value doesn't matter

void pushv(Val v) {
  if (SP + 1 == stack + 10000)
    err("trying to push into a full stack");
  *++SP = v;
}

Val popv() {
  if (SP == stack - 1)
    err("trying to pop from empty stack");
  return *SP--;
}

void pushi(int i) {
  if (SP + 1 == stack + 10000)
    err("trying to push into a full stack");
  (++SP)->i = i;
}

int popi() {
  if (SP == stack - 1)
    err("trying to pop from empty stack");
  return SP--->i;
}

void pushp(void *p) {
  if (SP + 1 == stack + 10000)
    err("trying to push into a full stack");
  (++SP)->p = p;
}

void *popp() {
  if (SP == stack - 1)
    err("trying to pop from empty stack");
  return SP--->p;
}

void put_i() { printf("=> %d", popi()); }

// ADDED FUNCTIONS
// -------------------------------------------------------------------

double popf() {
  if (SP == stack - 1)
    err("trying to pop from empty stack");
  return SP--->f;
}

void put_d() { printf("=> %g", popf()); }

void pushf(double f) {
  if (SP + 1 == stack + 10000)
    err("trying to push into a full stack");
  (++SP)->f = f;
}

// -------------------------------------------------------------------

void vmInit() {
  Symbol *fn = addExtFn("put_i", put_i, (Type){TB_VOID, NULL, -1});
  addFnParam(fn, "i", (Type){TB_INT, NULL, -1});

  Symbol *fn2 =
      addExtFn("put_d", put_d,
               (Type){TB_VOID, NULL, -1}); // add function to the symbol table
  addFnParam(fn2, "d",
             (Type){TB_DOUBLE, NULL, -1}); // add parameter to the function
}

void run(Instr *IP) {
  Val v;
  int iArg, iTop, iBefore;
  double dArg, dTop, dBefore;
  void (*extFnPtr)();
  for (;;) {
    // shows the index of the current instruction and the number of values from
    // stack
    printf("%p/%d\t", IP, (int)(SP - stack + 1));
    switch (IP->op) {
    case OP_HALT:
      printf("HALT");
      return;
    case OP_PUSH_I:
      printf("PUSH.i\t%d", IP->arg.i);
      pushi(IP->arg.i);
      IP = IP->next;
      break;
    case OP_CALL:
      pushp(IP->next);
      printf("CALL\t%p", IP->arg.instr);
      IP = IP->arg.instr;
      break;
    case OP_CALL_EXT:
      extFnPtr = IP->arg.extFnPtr;
      printf("CALL_EXT\t%p\n", extFnPtr);
      extFnPtr();
      IP = IP->next;
      break;
    case OP_ENTER:
      pushp(FP);
      FP = SP;
      SP += IP->arg.i;
      printf("ENTER\t%d", IP->arg.i);
      IP = IP->next;
      break;
    case OP_RET_VOID:
      iArg = IP->arg.i;
      printf("RET_VOID\t%d", iArg);
      IP = FP[-1].p;
      SP = FP - iArg - 2;
      FP = FP[0].p;
      break;
    case OP_JMP:
      printf("JMP\t%p", IP->arg.instr);
      IP = IP->arg.instr;
      break;
    case OP_JF:
      iTop = popi();
      printf("JF\t%p\t// %d", IP->arg.instr, iTop);
      IP = iTop ? IP->next : IP->arg.instr;
      break;
    case OP_FPLOAD:
      v = FP[IP->arg.i];
      pushv(v);
      printf("FPLOAD\t%d\t// i:%d, f:%g", IP->arg.i, v.i, v.f);
      IP = IP->next;
      break;
    case OP_FPSTORE:
      v = popv();
      FP[IP->arg.i] = v;
      printf("FPSTORE\t%d\t// i:%d, f:%g", IP->arg.i, v.i, v.f);
      IP = IP->next;
      break;
    case OP_ADD_I:
      iTop = popi();
      iBefore = popi();
      pushi(iBefore + iTop);
      printf("ADD.i\t// %d+%d -> %d", iBefore, iTop, iBefore + iTop);
      IP = IP->next;
      break;
    case OP_LESS_I:
      iTop = popi();
      iBefore = popi();
      pushi(iBefore < iTop);
      printf("LESS.i\t// %d<%d -> %d", iBefore, iTop, iBefore < iTop);
      IP = IP->next;
      break;
	case OP_LESS_D:
      dTop = popf();
      dBefore = popf();
      pushi(dBefore < dTop);
      printf("LESS.d\t\t// %f<%f -> %d", dBefore, dTop, dBefore < dTop);
      IP = IP->next;
      break;
    case OP_PUSH_D:
      printf("PUSH.d\t%f", IP->arg.f);
      pushf(IP->arg.f);
      IP = IP->next;
      break;
    case OP_ADD_D:
      dTop = popf();
      dBefore = popf();
      pushf(dBefore + dTop);
      printf("ADD.d\t\t// %f+%f -> %f", dBefore, dTop, dBefore + dTop);
      IP = IP->next;
      break;
    default:
      err("run: instructiune neimplementata: %d", IP->op);
    }
    putchar('\n');
  }
}

/* The program implements the following AtomC source code:
f(2);
void f(int n){		// stack frame: n[-2] ret[-1] oldFP[0] i[1]
        int i=0;
        while(i<n){
                put_i(i);
                i=i+1;
                }
        }
*/
Instr *genTestProgram() {
  Instr *code = NULL;
  addInstrWithInt(&code, OP_PUSH_I, 2);
  Instr *callPos = addInstr(&code, OP_CALL);
  addInstr(&code, OP_HALT);
  callPos->arg.instr = addInstrWithInt(&code, OP_ENTER, 1);
  // int i=0;
  addInstrWithInt(&code, OP_PUSH_I, 0);
  addInstrWithInt(&code, OP_FPSTORE, 1);
  // while(i<n){
  Instr *whilePos = addInstrWithInt(&code, OP_FPLOAD, 1);
  addInstrWithInt(&code, OP_FPLOAD, -2);
  addInstr(&code, OP_LESS_I);
  Instr *jfAfter = addInstr(&code, OP_JF);
  // put_i(i);
  addInstrWithInt(&code, OP_FPLOAD, 1);
  Symbol *s = findSymbol("put_i");
  if (!s)
    err("undefined: put_i");
  addInstr(&code, OP_CALL_EXT)->arg.extFnPtr = s->fn.extFnPtr;
  // i=i+1;
  addInstrWithInt(&code, OP_FPLOAD, 1);
  addInstrWithInt(&code, OP_PUSH_I, 1);
  addInstr(&code, OP_ADD_I);
  addInstrWithInt(&code, OP_FPSTORE, 1);
  // } ( the next iteration)
  addInstr(&code, OP_JMP)->arg.instr = whilePos;
  // returns from function
  jfAfter->arg.instr = addInstrWithInt(&code, OP_RET_VOID, 1);
  return code;
}


/* The program implements the following AtomC source code:
f(2.0);
void f(double n){
    double i=0.0;
    while(i<n){
        put_d(i);
        i=i+0.5;
        }
}
*/

Instr *genTestProgramFloat() {
  Instr *code = NULL; 
  // f(2.0)
  addInstrWithDouble(&code, OP_PUSH_D, 2.0); 
  Instr *callPos = addInstr(&code, OP_CALL); // calls the function
  addInstr(&code, OP_HALT); // ends the code execution, after the function returns
  // void f(double n) (entering the function)
  callPos->arg.instr = addInstrWithInt(&code, OP_ENTER, 1); // creates a function frame with the given number of local variables
  // double i=0.0;
  addInstrWithDouble(&code, OP_PUSH_D, 0.0); // puts on stack the constant 0.0
  addInstrWithDouble(&code, OP_FPSTORE, 1); // puts in FP[1] the value from stack
  // while(i<n){
  Instr *whilePos = addInstrWithDouble(&code, OP_FPLOAD, 1); // puts on stack the value from FP[1]
  addInstrWithDouble(&code, OP_FPLOAD, -2); // puts on stack the value from FP[-2]
  addInstr(&code, OP_LESS_D); // compares last 2 double values from stack and puts the result on stack as double
  Instr *jfAfter = addInstr(&code, OP_JF); // jumps to the specified instruction if the value from stack is false
  // put_d(i);
  addInstrWithDouble(&code, OP_FPLOAD, 1); // puts on stack the value from FP[1]
  Symbol *s = findSymbol("put_d"); 
  if (!s) 
	err("undefined: put_d");
  addInstr(&code, OP_CALL_EXT)->arg.extFnPtr = s->fn.extFnPtr; // calls the function put_d
  // i=i+0.5;
  addInstrWithDouble(&code, OP_FPLOAD, 1); // puts on stack the value from FP[1]
  addInstrWithDouble(&code, OP_PUSH_D, 0.5); // puts on stack the constant 0.5
  addInstr(&code, OP_ADD_D); // adds 2 double values from stack and puts the result on stack as double
  addInstrWithDouble(&code, OP_FPSTORE, 1); // puts in FP[1] the value from stack
  // } ( the next iteration) - jump to the beginning of the while loop
  addInstr(&code, OP_JMP)->arg.instr = whilePos;
  // returns from function
  jfAfter->arg.instr = addInstrWithInt(&code, OP_RET_VOID, 1);
  return code;
}
