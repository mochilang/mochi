// list_assign.c - manual translation of tests/vm/valid/list_assign.mochi
#include <stdio.h>

int main() {
    int nums[] = {1, 2};
    nums[1] = 3;
    printf("%d\n", nums[1]);
    return 0;
}
