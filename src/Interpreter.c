#include "Interpreter.h"

Stack* stack_create(size_t cap) {
    Stack* st = (Stack*)malloc(sizeof(Stack));
    if (st == NULL) return NULL;

    double* arr = (double*)calloc(cap, sizeof(double));
    if (arr == NULL) { free(st); return NULL; }

    st->arr = arr;
    st->cap = cap;
    st->idx = 0;
    return st;
}

void stack_destroy(Stack* st) {
    free(st->arr);
    free(st);
}

int stack_resize(Stack* st, size_t cap) {
    double* arr = (double*)realloc(st->arr, cap * sizeof(double));
    if (arr == NULL) return 1;

    st->arr = arr;
    st->cap = cap;
    return 0;
}

int stack_push(Stack* st, double n) {
    if (st->idx == st->cap && stack_resize(st, st->cap * 2)) return 1;
    st->arr[st->idx] = n;
    st->idx += 1;
    return 0;
}

int stack_pop(Stack* st, double* n) {
    if (st->idx == 0) return 1;
    st->idx -= 1;
    if (n != NULL) *n = st->arr[st->idx];
    return 0;
}

int stack_add(Stack* st) {
    if (st->idx < 2) return 1;
    st->arr[st->idx - 2] += st->arr[st->idx - 1];
    st->arr[st->idx - 1] = 0;
    st->idx -= 1;
    return 0;
}

int stack_sub(Stack* st) {
    if (st->idx < 2) return 1;
    st->arr[st->idx - 2] -= st->arr[st->idx - 1];
    st->arr[st->idx - 1] = 0;
    st->idx -= 1;
    return 0;
}

int stack_mul(Stack* st) {
    if (st->idx < 2) return 1;
    st->arr[st->idx - 2] *= st->arr[st->idx - 1];
    st->arr[st->idx - 1] = 0;
    st->idx -= 1;
    return 0;
}

int stack_div(Stack* st) {
    if (st->idx < 2) return 1;
    st->arr[st->idx - 2] /= st->arr[st->idx - 1];
    st->arr[st->idx - 1] = 0;
    st->idx -= 1;
    return 0;
}

int interpret(double* res, uint8_t* bc) {
    Stack* st = stack_create(DEFAULT_STACK_CAP);

    while (1) {
        switch (*bc) {
            case EXIT:
                if (stack_pop(st, res)) { stack_destroy(st); return 1; }
                break;
            case PUSH:
                double n;
                memcpy(&n, bc + 1, sizeof(double));
                if (stack_push(st, n)) { stack_destroy(st); return 1; }
                bc += 1 + sizeof(double);
                continue;
            case POP:
                if (stack_pop(st, NULL)) { stack_destroy(st); return 1; }
                bc += 1;
                continue;
            case ADD:
                if (stack_add(st)) { stack_destroy(st); return 1; }
                bc += 1;
                continue;
            case SUB:
                if (stack_sub(st)) { stack_destroy(st); return 1; }
                bc += 1;
                continue;
            case MUL:
                if (stack_mul(st)) { stack_destroy(st); return 1; }
                bc += 1;
                continue;
            case DIV:
                if (stack_div(st)) { stack_destroy(st); return 1; }
                bc += 1;
                continue;
        }
        break;
    }

    stack_destroy(st);
    return 0;
}
