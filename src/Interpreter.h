#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "Bytecode.h"

#define DEFAULT_STACK_CAP 64

typedef struct {
    double* arr;
    size_t  cap;
    size_t  idx;
} Stack;

Stack* stack_create  ( size_t cap );
void   stack_destroy ( Stack* st );
int    stack_resize  ( Stack* st, size_t cap );

int    stack_push    ( Stack* st, double n );
int    stack_pop     ( Stack* st, double* n );
int    stack_add     ( Stack* st );
int    stack_sub     ( Stack* st );
int    stack_mul     ( Stack* st );
int    stack_div     ( Stack* st );

int    interpret     ( double* res, uint8_t* bc );
