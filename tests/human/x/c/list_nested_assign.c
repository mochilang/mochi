// list_nested_assign.c - manual translation of tests/vm/valid/list_nested_assign.mochi
#include <stdio.h>

int main() {
    int matrix[2][2] = {{1,2},{3,4}};
    matrix[1][0] = 5;
    printf("%d\n", matrix[1][0]);
    return 0;
}
