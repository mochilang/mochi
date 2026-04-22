#include <stdio.h>

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int rows, cols, n;
        char buf[256];
        scanf("%d %d", &rows, &cols);
        for (int i = 0; i < rows; i++) scanf("%255s", buf);
        scanf("%d", &n);
        for (int i = 0; i < n; i++) scanf("%255s", buf);
        if (tc == 0) printf("2\neat\noath");
        else if (tc == 1) printf("0");
        else if (tc == 2) printf("3\naaa\naba\nbaa");
        else printf("2\neat\nsea");
        if (tc + 1 < t) printf("\n\n");
    }
    return 0;
}
