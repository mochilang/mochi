#include <stdio.h>

static int solve(int *a, int n) {
    int best = 0;
    for (int i = 0; i < n; i++) {
        int mn = a[i];
        for (int j = i; j < n; j++) {
            if (a[j] < mn) mn = a[j];
            int area = mn * (j - i + 1);
            if (area > best) best = area;
        }
    }
    return best;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n;
        scanf("%d", &n);
        int a[n];
        for (int i = 0; i < n; i++) scanf("%d", &a[i]);
        printf("%d", solve(a, n));
        if (tc + 1 < t) printf("\n");
    }
    return 0;
}
