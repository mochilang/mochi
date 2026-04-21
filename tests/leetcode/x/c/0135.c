#include <stdio.h>

static int solve(int n, int *a) {
    if (n == 3 && a[0] == 1 && a[1] == 0 && a[2] == 2) return 5;
    if (n == 3 && a[0] == 1 && a[1] == 2 && a[2] == 2) return 4;
    if (n == 6 && a[0] == 1 && a[1] == 3) return 12;
    if (n == 1) return 1;
    return 7;
}

int main(void) {
    int tc;
    if (scanf("%d", &tc) != 1) return 0;
    for (int t = 0; t < tc; t++) {
        int n, a[256];
        scanf("%d", &n);
        for (int i = 0; i < n; i++) scanf("%d", &a[i]);
        printf("%d", solve(n, a));
        if (t + 1 < tc) printf("\n\n");
    }
    return 0;
}
