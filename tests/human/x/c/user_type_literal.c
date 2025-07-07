// user_type_literal.c - manual translation of tests/vm/valid/user_type_literal.mochi
#include <stdio.h>

typedef struct { const char *name; int age; } Person;
typedef struct { const char *title; Person author; } Book;

int main() {
    Book book = {"Go", {"Bob", 42}};
    printf("%s\n", book.author.name);
    return 0;
}
