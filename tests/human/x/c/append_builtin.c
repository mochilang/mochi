// append_builtin.c - manual translation of tests/vm/valid/append_builtin.mochi
#include <stdio.h>

int main() {
    int a[] = {1, 2};
    int appended[] = {a[0], a[1], 3};
    printf("[");
    for (int i = 0; i < 3; i++) {
        if (i > 0) printf(", ");
        printf("%d", appended[i]);
    }
    printf("]\n");
    return 0;
}
