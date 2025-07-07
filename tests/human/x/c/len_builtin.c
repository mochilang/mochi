// len_builtin.c - manual translation of tests/vm/valid/len_builtin.mochi
#include <stdio.h>

int main() {
    int arr[] = {1, 2, 3};
    printf("%lu\n", (unsigned long)(sizeof(arr) / sizeof(arr[0])));
    return 0;
}
