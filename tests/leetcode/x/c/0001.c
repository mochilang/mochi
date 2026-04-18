#include <stdio.h>
#include <stdlib.h>

static void two_sum(const int *nums, int n, int target, int *a, int *b) {
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            if (nums[i] + nums[j] == target) {
                *a = i;
                *b = j;
                return;
            }
        }
    }
    *a = 0;
    *b = 0;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) {
        return 0;
    }
    for (int tc = 0; tc < t; tc++) {
        int n, target;
        scanf("%d %d", &n, &target);
        int *nums = (int *)malloc((size_t)n * sizeof(int));
        for (int i = 0; i < n; i++) {
            scanf("%d", &nums[i]);
        }
        int a, b;
        two_sum(nums, n, target, &a, &b);
        printf("%d %d\n", a, b);
        free(nums);
    }
    return 0;
}
