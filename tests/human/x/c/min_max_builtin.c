// min_max_builtin.c - manual translation of tests/vm/valid/min_max_builtin.mochi
#include <stdio.h>

int main() {
    int nums[] = {3, 1, 4};
    int min = nums[0];
    int max = nums[0];
    for (int i = 1; i < 3; i++) {
        if (nums[i] < min) min = nums[i];
        if (nums[i] > max) max = nums[i];
    }
    printf("%d\n", min);
    printf("%d\n", max);
    return 0;
}
