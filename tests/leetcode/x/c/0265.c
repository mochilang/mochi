#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

static int solve(int **costs, int n, int k) {
    if (n == 0) return 0;
    int *prev = (int *)malloc((size_t)k * sizeof(int));
    for (int j = 0; j < k; ++j) prev[j] = costs[0][j];
    for (int i = 1; i < n; ++i) {
        int min1 = INT_MAX, min2 = INT_MAX, idx1 = -1;
        for (int j = 0; j < k; ++j) {
            if (prev[j] < min1) {
                min2 = min1;
                min1 = prev[j];
                idx1 = j;
            } else if (prev[j] < min2) {
                min2 = prev[j];
            }
        }
        int *cur = (int *)malloc((size_t)k * sizeof(int));
        for (int j = 0; j < k; ++j) cur[j] = costs[i][j] + (j == idx1 ? min2 : min1);
        free(prev);
        prev = cur;
    }
    int ans = prev[0];
    for (int j = 1; j < k; ++j) if (prev[j] < ans) ans = prev[j];
    free(prev);
    return ans;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n, k;
        scanf("%d %d", &n, &k);
        int **costs = (int **)malloc((size_t)n * sizeof(int *));
        for (int i = 0; i < n; ++i) {
            costs[i] = (int *)malloc((size_t)k * sizeof(int));
            for (int j = 0; j < k; ++j) scanf("%d", &costs[i][j]);
        }
        if (tc) putchar('\n');
        printf("%d", solve(costs, n, k));
        for (int i = 0; i < n; ++i) free(costs[i]);
        free(costs);
    }
    return 0;
}
