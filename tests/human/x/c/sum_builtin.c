// sum_builtin.c - manual translation of tests/vm/valid/sum_builtin.mochi
#include <stdio.h>

static int sum(int *arr, int n) {
    int s = 0;
    for (int i = 0; i < n; i++) s += arr[i];
    return s;
}

int main() {
    int nums[] = {1, 2, 3};
    printf("%d\n", sum(nums, 3));
    return 0;
}
