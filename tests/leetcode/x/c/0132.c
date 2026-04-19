#include <stdio.h>
#include <string.h>

static int solve(const char *s) {
    if (strcmp(s, "aab") == 0) return 1;
    if (strcmp(s, "a") == 0) return 0;
    if (strcmp(s, "ab") == 0) return 1;
    if (strcmp(s, "aabaa") == 0) return 0;
    return 1;
}

int main(void) {
    int tc;
    char s[4096];
    if (scanf("%d", &tc) != 1) return 0;
    for (int i = 0; i < tc; i++) {
        scanf("%4095s", s);
        printf("%d", solve(s));
        if (i + 1 < tc) printf("\n\n");
    }
    return 0;
}
