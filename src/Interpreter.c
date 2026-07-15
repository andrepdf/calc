#include "Interpreter.h"

int stack_create(Stack** out, Size cap) {
    Stack* st = (Stack*)malloc(sizeof(Stack));
    if (st == NULL) return ERR;

    Value* arr = (Value*)calloc(cap, sizeof(Value));
    if (arr == NULL) { free(st); return ERR; }

    st->arr = arr;
    st->cap = cap;
    st->idx = 0;
    *out = st;
    return OK;
}

void stack_destroy(Stack* st) {
    free(st->arr);
    free(st);
}

int stack_resize(Stack* st, Size cap) {
    Value* arr = (Value*)realloc(st->arr, cap * sizeof(Value));
    if (arr == NULL) return ERR;

    st->arr = arr;
    st->cap = cap;
    return OK;
}

int stack_push(Stack* st, Value val) {
    if (st->idx == st->cap && !stack_resize(st, st->cap * 2)) return ERR;
    st->arr[st->idx] = val;
    st->idx += 1;
    return OK;
}

int stack_pop(Stack* st, Value* val) {
    if (st->idx == 0) return ERR;
    st->idx -= 1;
    if (val != NULL) *val = st->arr[st->idx];
    return OK;
}

int stack_get(Stack* st, Size off) {
    if (off > st->idx) return ERR;
    Value val = st->arr[st->idx - off];
    if (!stack_push(st, val)) return ERR;
    return OK;
}

int stack_add(Stack* st) {
    if (st->idx < 2) return ERR;
    st->arr[st->idx - 2] += st->arr[st->idx - 1];
    st->idx -= 1;
    return OK;
}

int stack_sub(Stack* st) {
    if (st->idx < 2) return ERR;
    st->arr[st->idx - 2] -= st->arr[st->idx - 1];
    st->idx -= 1;
    return OK;
}

int stack_mul(Stack* st) {
    if (st->idx < 2) return ERR;
    st->arr[st->idx - 2] *= st->arr[st->idx - 1];
    st->idx -= 1;
    return OK;
}

int stack_div(Stack* st) {
    if (st->idx < 2) return ERR;
    st->arr[st->idx - 2] /= st->arr[st->idx - 1];
    st->idx -= 1;
    return OK;
}

int interpret(Value* out, Byte* bc) {
    Stack* st;
    if (!stack_create(&st, DEFAULT_STACK_CAP)) return ERR;

    while (1) {
        switch(*bc) {
            case EXIT:
                if (!stack_pop(st, out)) return ERR;
                break;
            case PUSH:
                Value val;
                memcpy(&val, bc + 1, sizeof(Value));
                if (!stack_push(st, val)) return ERR;
                bc += sizeof(Byte) + sizeof(Value);
                continue;
            case POP:
                if (!stack_pop(st, NULL)) return ERR;
                bc += sizeof(Byte);
                continue;
            case GET:
                Size off;
                memcpy(&off, bc + 1, sizeof(Size));
                if (!stack_get(st, off)) return ERR;
                bc += sizeof(Byte) + sizeof(Size);
                continue;
            case ADD:
                if (!stack_add(st)) return ERR;
                bc += sizeof(Byte);
                continue;
            case SUB:
                if (!stack_sub(st)) return ERR;
                bc += sizeof(Byte);
                continue;
            case MUL:
                if (!stack_mul(st)) return ERR;
                bc += sizeof(Byte);
                continue;
            case DIV:
                if (!stack_div(st)) return ERR;
                bc += sizeof(Byte);
                continue;
        }
        break;
    }

    stack_destroy(st);
    return OK;
}
