// cross_join_triple.c - manual translation of tests/vm/valid/cross_join_triple.mochi
#include <stdio.h>
#include <stdbool.h>

int main() {
    int nums[] = {1,2};
    const char *letters[] = {"A","B"};
    bool bools[] = {true,false};
    int nNums = sizeof(nums)/sizeof(nums[0]);
    int nLetters = sizeof(letters)/sizeof(letters[0]);
    int nBools = sizeof(bools)/sizeof(bools[0]);
    printf("--- Cross Join of three lists ---\n");
    for (int i=0;i<nNums;i++) {
        for (int j=0;j<nLetters;j++) {
            for (int k=0;k<nBools;k++) {
                printf("%d %s %s\n", nums[i], letters[j], bools[k]?"true":"false");
            }
        }
    }
    return 0;
}
