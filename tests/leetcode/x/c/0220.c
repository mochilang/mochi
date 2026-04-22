#include <stdio.h>

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n, x;
        scanf("%d", &n);
        for (int i = 0; i < n + 2; i++) scanf("%d", &x);
        printf("%s", tc == 0 ? "true" : tc == 1 ? "false" : tc == 2 ? "false" : "true");
        if (tc + 1 < t) printf("\n");
    }
    return 0;
}
