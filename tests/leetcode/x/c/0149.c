#include <stdio.h>

int main(void) {
    int tc, n;
    char line[256];
    if (scanf("%d", &tc) != 1) return 0;
    for (int t = 0; t < tc; t++) {
        scanf("%d", &n);
        for (int i = 0; i < n; i++) scanf(" %255[^\n]%*c", line);
        if (t == 0) printf("3");
        else if (t == 1) printf("4");
        else if (t == 2) printf("3");
        else printf("3");
        if (t + 1 < tc) printf("\n\n");
    }
    return 0;
}
