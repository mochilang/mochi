#include <stdio.h>

int main(void) {
    int t;
    char line[256];
    if (scanf("%d\n", &t) != 1) return 0;
    for (int tc = 0; tc < t; ++tc) {
        if (!fgets(line, sizeof(line), stdin)) return 0;
        int len = 0;
        while (line[len] && line[len] != '\n' && line[len] != '\r') len++;
        line[len] = '\0';
        if (tc) printf("\n\n");
        printf("%s", line);
    }
    return 0;
}
