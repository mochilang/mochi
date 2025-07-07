// count_builtin.c - manual translation of tests/vm/valid/count_builtin.mochi
#include <stdio.h>

int main() {
    int arr[] = {1,2,3};
    int count = sizeof(arr)/sizeof(arr[0]);
    printf("%d\n", count);
    return 0;
}
