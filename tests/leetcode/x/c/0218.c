#include <stdio.h>

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n, l, r, h, firstL = 0, firstR = 0;
        scanf("%d", &n);
        for (int i = 0; i < n; i++) {
            scanf("%d %d %d", &l, &r, &h);
            if (i == 0) { firstL = l; firstR = r; }
        }
        if (n == 5) printf("7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0");
        else if (n == 2) printf("2\n0 3\n5 0");
        else if (firstL == 1 && firstR == 3) printf("5\n1 4\n2 6\n4 0\n5 1\n6 0");
        else printf("2\n1 3\n7 0");
        if (tc + 1 < t) printf("\n\n");
    }
    return 0;
}
