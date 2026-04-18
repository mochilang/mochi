#include <stdio.h>

double median(const int *a, int n, const int *b, int m) {
    int merged[4096];
    int i = 0, j = 0, k = 0;
    while (i < n && j < m) merged[k++] = a[i] <= b[j] ? a[i++] : b[j++];
    while (i < n) merged[k++] = a[i++];
    while (j < m) merged[k++] = b[j++];
    if (k % 2) return merged[k / 2];
    return (merged[k / 2 - 1] + merged[k / 2]) / 2.0;
}

int main(void) {
    int t; if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n, m, a[2048], b[2048];
        scanf("%d", &n); for (int i=0;i<n;i++) scanf("%d", &a[i]);
        scanf("%d", &m); for (int i=0;i<m;i++) scanf("%d", &b[i]);
        printf("%.1f", median(a,n,b,m));
        if (tc + 1 < t) putchar('\n');
    }
    return 0;
}
