#include <stdio.h>
#include <stdlib.h>

int firstMissingPositive(int* nums, int n) {
    int i = 0;
    while (i < n) {
        int v = nums[i];
        if (v >= 1 && v <= n && nums[v - 1] != v) {
            int tmp = nums[i];
            nums[i] = nums[v - 1];
            nums[v - 1] = tmp;
        } else {
            i++;
        }
    }
    for (i = 0; i < n; i++) {
        if (nums[i] != i + 1) return i + 1;
    }
    return n + 1;
}

int main() {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n;
        scanf("%d", &n);
        int* nums = (int*)malloc(sizeof(int) * (n > 0 ? n : 1));
        for (int i = 0; i < n; i++) scanf("%d", &nums[i]);
        if (tc) printf("\n");
        printf("%d", firstMissingPositive(nums, n));
        free(nums);
    }
    return 0;
}
