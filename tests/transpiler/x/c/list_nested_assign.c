// Generated by Mochi 0.10.32 on 2025-07-21 16:14 +0700
#include <stdio.h>
#include <string.h>

int matrix[2][2] = { { 1, 2 }, { 3, 4 } };

int main(void) {
    matrix[1][0] = 5;
    printf("%d\n", matrix[1][0]);
    return 0;
}
