// cast_struct.c - manual translation of tests/vm/valid/cast_struct.mochi
#include <stdio.h>
#include <string.h>

typedef struct {
    const char *title;
} Todo;

int main() {
    Todo todo;
    todo.title = "hi";
    printf("%s\n", todo.title);
    return 0;
}
