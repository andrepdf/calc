#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "Bytecode.h"

#define OK  1
#define ERR 0

#define DEFAULT_STACK_CAP 64

typedef double  Value;
typedef size_t  Size;
typedef uint8_t Byte;

typedef struct {
    Value* arr;
    Size   cap;
    Size   idx;
} Stack;

int  stack_create  ( Stack** out, Size cap );
void stack_destroy ( Stack* st );
int  stack_resize  ( Stack* st, Size cap );

int  stack_push    ( Stack* st, Value val );
int  stack_pop     ( Stack* st, Value* val );
int  stack_get     ( Stack* st, Size off );
int  stack_add     ( Stack* st );
int  stack_sub     ( Stack* st );
int  stack_mul     ( Stack* st );
int  stack_div     ( Stack* st );

int  interpret     ( Value* out, Byte* bc );
