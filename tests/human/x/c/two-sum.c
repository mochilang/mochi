// two-sum.c - manual translation of tests/vm/valid/two-sum.mochi
#include <stdio.h>

void twoSum(const int *nums, int n, int target, int out[2]) {
    for(int i=0;i<n;i++) {
        for(int j=i+1;j<n;j++) {
            if(nums[i]+nums[j]==target) { out[0]=i; out[1]=j; return; }
        }
    }
    out[0]=-1; out[1]=-1;
}

int main() {
    int nums[] = {2,7,11,15};
    int result[2];
    twoSum(nums,4,9,result);
    printf("%d\n%d\n", result[0], result[1]);
    return 0;
}
