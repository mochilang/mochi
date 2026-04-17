#include <stdio.h>
#include <stdlib.h>

int removeDuplicates(int* nums, int numsSize) {
    if (numsSize == 0) return 0;
    int k = 1;
    for (int i = 1; i < numsSize; i++) {
        if (nums[i] != nums[k - 1]) {
            nums[k] = nums[i];
            k++;
        }
    }
    return k;
}

int main() {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    while (t--) {
        int n;
        if (scanf("%d", &n) != 1) break;
        int* nums = (int*)malloc(n * sizeof(int));
        for (int i = 0; i < n; i++) {
            scanf("%d", &nums[i]);
        }
        int k = removeDuplicates(nums, n);
        for (int i = 0; i < k; i++) {
            printf("%d%s", nums[i], (i == k - 1 ? "" : " "));
        }
        printf("\n");
        free(nums);
    }
    return 0;
}
