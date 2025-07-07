#include <stdio.h>

int main() {
    int nums[] = {1,2,3};
    int n = sizeof(nums)/sizeof(nums[0]);
    int result = 0;
    for(int i=0;i<n;i++) if(nums[i]>1) result += nums[i];
    printf("%d\n", result);
    return 0;
}
