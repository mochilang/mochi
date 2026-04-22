#include <math.h>
#include <stdio.h>
#include <stdlib.h>

static void solve(int *values, int n, double target, int k) {
    int right = 0;
    while (right < n && values[right] < target) right++;
    int left = right - 1;
    printf("%d", k);
    for (int i = 0; i < k; ++i) {
        int pick;
        if (left < 0) pick = values[right++];
        else if (right >= n) pick = values[left--];
        else if (fabs(values[left] - target) <= fabs(values[right] - target)) pick = values[left--];
        else pick = values[right++];
        printf("\n%d", pick);
    }
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n, k;
        double target;
        scanf("%d", &n);
        int *values = (int *)malloc((size_t)n * sizeof(int));
        for (int i = 0; i < n; ++i) scanf("%d", &values[i]);
        scanf("%lf%d", &target, &k);
        if (tc) printf("\n\n");
        solve(values, n, target, k);
        free(values);
    }
    return 0;
}
