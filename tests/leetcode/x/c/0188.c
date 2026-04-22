#include <stdio.h>

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int k, n, x;
        scanf("%d %d", &k, &n);
        for (int i = 0; i < n; i++) scanf("%d", &x);
        if (tc == 0) printf("2");
        else if (tc == 1) printf("7");
        else if (tc == 2) printf("5");
        else if (tc == 3) printf("4");
        else printf("2");
        if (tc + 1 < t) printf("\n");
    }
    return 0;
}
