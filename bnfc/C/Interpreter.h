#ifndef INTERPRETER_HEADER
#define INTERPRETER_HEADER

#include "Absyn.h"

enum bool {false, true};

void exec(Com p);
int aeval(AExp p);
enum bool beval(BExp p);

typedef struct State_ *State;

struct State_ {
    char id[20];
    int val;
    struct State_ *next;
};

void printState(void);

#endif
