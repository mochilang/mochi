// avg_builtin.c - manual translation of tests/vm/valid/avg_builtin.mochi
#include <stdio.h>

int main() {
    int nums[] = {1, 2, 3};
    int sum = 0;
    for (int i = 0; i < 3; i++) sum += nums[i];
    double avg = sum / 3.0;
    printf("%g\n", avg);
    return 0;
}
