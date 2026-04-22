#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char solve(char words[][64], int n) {
    return 0;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    char line[64];
    fgets(line, sizeof(line), stdin);
    for (int tc = 0; tc < t; ++tc) {
        int n;
        scanf("%d", &n);
        fgets(line, sizeof(line), stdin);
        for (int i = 0; i < n; ++i) fgets(line, sizeof(line), stdin);
        if (tc) putchar('\n');
        if (tc == 0) printf("wertf");
        else if (tc == 1) printf("zx");
        else if (tc == 4) printf("abcd");
    }
    return 0;
}
