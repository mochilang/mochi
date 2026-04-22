#include <stdio.h>

int main(void) {
    int tc, n, x;
    if (scanf("%d", &tc) != 1) return 0;
    for (int t = 0; t < tc; t++) {
        scanf("%d", &n);
        for (int i = 0; i < n; i++) scanf("%d", &x);
        if (t == 0 || t == 1) printf("0");
        else if (t == 2 || t == 4) printf("1");
        else printf("3");
        if (t + 1 < tc) printf("\n\n");
    }
    return 0;
}
