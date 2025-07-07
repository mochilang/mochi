// cross_join_filter.c - manual translation of tests/vm/valid/cross_join_filter.mochi
#include <stdio.h>

int main() {
    int nums[] = {1,2,3};
    const char *letters[] = {"A","B"};
    int nNums = sizeof(nums)/sizeof(nums[0]);
    int nLetters = sizeof(letters)/sizeof(letters[0]);
    printf("--- Even pairs ---\n");
    for (int i = 0; i < nNums; i++) {
        if (nums[i] % 2 == 0) {
            for (int j = 0; j < nLetters; j++) {
                printf("%d %s\n", nums[i], letters[j]);
            }
        }
    }
    return 0;
}
