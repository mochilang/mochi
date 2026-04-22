#include <stdio.h>
#include <stdlib.h>

static void solve(int *nums, int n, int k) {
    int *dq = (int *)malloc((size_t)n * sizeof(int));
    int head = 0, tail = 0;
    int first = 1;
    printf("%d", n - k + 1);
    for (int i = 0; i < n; ++i) {
        while (head < tail && dq[head] <= i - k) head++;
        while (head < tail && nums[dq[tail - 1]] <= nums[i]) tail--;
        dq[tail++] = i;
        if (i >= k - 1) {
            printf("\n%d", nums[dq[head]]);
            first = 0;
        }
    }
    (void)first;
    free(dq);
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n, k;
        scanf("%d", &n);
        int *nums = (int *)malloc((size_t)n * sizeof(int));
        for (int i = 0; i < n; ++i) scanf("%d", &nums[i]);
        scanf("%d", &k);
        if (tc) printf("\n\n");
        solve(nums, n, k);
        free(nums);
    }
    return 0;
}
