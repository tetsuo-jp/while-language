#include <stdlib.h>
#include <stdio.h>
#include <string.h>             /* strcpy */

#include "Interpreter.h"

struct State_ dummy;
State state = &dummy; /* ダミーノードを追加 */

// すでに挿入されている変数と組の値は更新する．変数が見つからない場合は末尾に追加する．
void insert_(State s, char id[20], int val)
{
    if (s->next == NULL) {
        s->next = (State)malloc(sizeof(struct State_));
        if (!s->next) {
            fprintf(stderr, "Error: out of memory when allocating insert_!\n");
            exit(1);
        }
        strcpy(s->next->id, id);
        s->next->val = val;
        s->next->next = NULL;
    } else if (!strcmp(s->next->id, id)) {
        s->next->val = val;
    } else {
        insert_(s->next,id,val);
    }
}

void insert(char id[20], int val)
{
    insert_(state,id,val);
}

int lookup_(State s, char id[20])
{
    if (s == NULL) {
        fprintf(stderr, "Error: no such variable in the state!\n");
        exit(1);
    } else if (!strcmp(s->id, id)) {
        return s->val;
    } else {
        return lookup_(s->next,id);
    }
}

int lookup(char id[20])
{
    return lookup_(state->next, id);
}

void printState(void)
{
    State s = state->next;
    printf("{");
    if (s != NULL) {
        printf("%s := %d", s->id, s->val);
        for (s = s->next; s != NULL; s = s->next) {
            printf(", %s := %d", s->id, s->val);
        }
    }
    printf("}\n");
}

void exec(Com p)
{
    switch(p->kind) {
    case is_CAsn:
        insert(p->u.casn_.ident_,
               aeval(p->u.casn_.aexp_));
        break;
    case is_CSeq:
        exec(p->u.cseq_.com_1);
        exec(p->u.cseq_.com_2);
        break;
    case is_CLoop:
        if (beval(p->u.cloop_.bexp_)) {
            exec(p->u.cloop_.com_);
            exec(p);
        }
        break;
    default:
        fprintf(stderr, "Error: bad kind field when printing Com!\n");
        exit(1);
    }
}

int aeval(AExp p)
{
    switch(p->kind) {
    case is_APlus:
        return aeval(p->u.aplus_.aexp_1) + aeval(p->u.aplus_.aexp_2);
    case is_AMinus:
        return aeval(p->u.aminus_.aexp_1) - aeval(p->u.aminus_.aexp_2);
    case is_ATimes:
        return aeval(p->u.atimes_.aexp_1) * aeval(p->u.atimes_.aexp_2);
    case is_ANum:
        return p->u.anum_.integer_;
    case is_AVar:
        return lookup(p->u.avar_.ident_);
    default:
        fprintf(stderr, "Error: bad kind field when printing AExp!\n");
        exit(1);
    }
}

enum bool beval(BExp p)
{
    switch(p->kind) {
    case is_BEq:
        return aeval(p->u.beq_.aexp_1) == aeval(p->u.beq_.aexp_2);
    case is_BLe:
        return aeval(p->u.ble_.aexp_1) <= aeval(p->u.ble_.aexp_2);
    case is_BNot:
        return !beval(p->u.bnot_.bexp_);
    case is_BAnd:
        return beval(p->u.band_.bexp_1) && beval(p->u.band_.bexp_2);
    case is_BOr:
        return beval(p->u.bor_.bexp_1) || beval(p->u.bor_.bexp_2);
    case is_BTrue:
        return true;
    case is_BFalse:
        return false;
    default:
        fprintf(stderr, "Error: bad kind field when printing BExp!\n");
        exit(1);
    }
}
